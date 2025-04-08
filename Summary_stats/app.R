library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(leafgl)
library(ggplot2)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotly)
library(data.table)

# load country boundaries
countries_sf <- ne_countries(scale = "medium", returnclass = "sf")
selected_countries_sf <- countries_sf %>%
  filter(continent == "Europe" | (continent == "Africa" & subregion %in% c("Northern Africa", "Western Africa")))
country_names <- sort(selected_countries_sf$name)

# define ui
ui <- fluidPage(
  titlePanel("Godwit Movement Explorer"),
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      p("1. Select a country, or choose 'custom' to draw your own aoi, or upload a custom aoi file (shp/geojson/gpkg/kml)."),
      p("2. Use the year dropdown and date slider to select a date range."),
      p("3. Filter birds by tagging site."),
      p("4. Select a bird (or keep all) to view latitude plot."),
      selectInput("country_aoi", "Select pre-defined aoi (country) or custom", 
                  choices = c("Custom" = "Custom", country_names),
                  selected = "Custom"),
      fileInput("aoi_upload", "Upload custom aoi (shp/geojson/gpkg/kml)",
                multiple = TRUE,
                accept = c(".shp", ".dbf", ".shx", ".prj", ".geojson", ".json", ".gpkg", ".kml", ".gml")),
      uiOutput("year_range_ui"),
      uiOutput("date_slider_ui"),
      uiOutput("tag_site_selector"),
      selectInput("basemap", "Choose basemap", 
                  choices = c("CartoDB Positron" = "CartoDB.Positron",
                              "Esri WorldImagery" = "Esri.WorldImagery"),
                  selected = "CartoDB.Positron"),
      uiOutput("download_visits_ui")
    ),
    mainPanel(
      leafletOutput("map"),
      verbatimTextOutput("summary"),
      tableOutput("test_visit_summary"),
      conditionalPanel(
        condition = "output.aoi_drawn == true",
        uiOutput("bird_select"),
        plotlyOutput("lat_plot"),
        tableOutput("summary_table")
      )
    )
  )
)

# define server
server <- function(input, output, session) {
  sf::sf_use_s2(FALSE)
  
  data <- reactive({
    req(file.exists("allLocations.rds"))
    df <- as.data.table(readRDS("allLocations.rds"))
    setkey(df, timestamp)
    df[, tag_site := as.factor(tag_site)]
    df[, sex := toupper(trimws(sex))]
    df[sex == "" | sex == "U" | is.na(sex), sex := NA]
    df[, sex := factor(sex, levels = c("M", "F"))]
    return(df)
  })
  
  rv <- reactiveValues(aoi = NULL)
  
  observeEvent(input$country_aoi, {
    if (input$country_aoi != "Custom") {
      selected_country <- input$country_aoi
      aoi_poly <- countries_sf %>% filter(name == selected_country)
      rv$aoi <- aoi_poly
      leafletProxy("map") %>%
        clearGroup("aoi") %>%
        addPolygons(data = aoi_poly, group = "aoi", color = "green", weight = 2, fill = FALSE)
    } else {
      rv$aoi <- NULL
      leafletProxy("map") %>% clearGroup("aoi")
    }
  })
  
  observeEvent(input$aoi_upload, {
    req(input$aoi_upload)
    files <- input$aoi_upload
    ext <- tools::file_ext(files$name)
    upload_dir <- dirname(files$datapath[1])
    if ("shp" %in% ext) {
      for (i in seq_along(files$name)) {
        file.rename(files$datapath[i], file.path(upload_dir, files$name[i]))
      }
      shp_path <- file.path(upload_dir, files$name[which(ext == "shp")])
      layer_name <- tools::file_path_sans_ext(basename(shp_path))
      shp <- tryCatch(st_read(dsn = upload_dir, layer = layer_name, quiet = TRUE), error = function(e) NULL)
    } else {
      shp <- tryCatch(st_read(files$datapath[1], quiet = TRUE), error = function(e) NULL)
    }
    if (!is.null(shp)) {
      if (st_crs(shp)$epsg != 4326) shp <- st_transform(shp, 4326)
      rv$aoi <- shp
      bbox <- st_bbox(shp)
      leafletProxy("map") %>%
        clearGroup("aoi") %>%
        addPolygons(data = shp, group = "aoi", color = "green", weight = 2, fill = FALSE) %>%
        fitBounds(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
    } else {
      showNotification("error reading uploaded aoi file.", type = "error")
    }
  })
  
  output$year_range_ui <- renderUI({
    req(data())
    years <- sort(unique(as.numeric(format(data()$timestamp, "%Y"))))
    choices <- sapply(years[-1], function(y) paste(y - 1, y, sep = "-"))
    selectInput("year_range_input", "Select year", choices = choices, selected = tail(choices, 1))
  })
  
  output$date_slider_ui <- renderUI({
    req(data(), input$year_range_input)
    yrs <- unlist(strsplit(input$year_range_input, "-"))
    df <- data() %>% filter(format(timestamp, "%Y") %in% yrs)
    sliderInput("date_slider_input", "Select date range",
                min = min(df$timestamp), max = max(df$timestamp),
                value = c(min(df$timestamp), max(df$timestamp)),
                timeFormat = "%Y-%m-%d")
  })
  
  output$tag_site_selector <- renderUI({
    req(data())
    tag_sites <- na.omit(levels(data()$tag_site))
    selectInput("selected_tag_site", "Tagging site",
                choices = c("All", tag_sites),
                selected = "All", multiple = FALSE)
  })
  
  filtered_data <- reactive({
    req(input$year_range_input, input$date_slider_input)
    df <- data()
    if (input$selected_tag_site != "All") {
      first_tags <- df[, .SD[1], by = trackId][tag_site == input$selected_tag_site]
      track_ids <- first_tags$trackId
      df <- df[trackId %in% track_ids]
    }
    yrs <- unlist(strsplit(input$year_range_input, "-"))
    df <- df[format(timestamp, "%Y") %in% yrs & timestamp >= input$date_slider_input[1] & timestamp <= input$date_slider_input[2]]
    return(df)
  })
  
  filtered_data_sf <- reactive({
    df <- filtered_data()
    st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
  })
  
  filtered_data_aoi <- reactive({
    req(filtered_data_sf(), rv$aoi)
    st_filter(filtered_data_sf(), rv$aoi)
  })
  
  output$map <- renderLeaflet({
    req(input$basemap)
    leaflet() %>%
      addProviderTiles(providers[[input$basemap]], layerId = "basemap") %>%
      setView(lng = 5, lat = 40, zoom = 4.5) %>%
      addDrawToolbar(
        targetGroup = "aoi",
        editOptions = editToolbarOptions(),
        polylineOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE
      )
  })
  
  observe({
    req(input$basemap, filtered_data_sf())
    leafletProxy("map") %>%
      clearTiles() %>%
      addProviderTiles(providers[[input$basemap]], layerId = "basemap") %>%
      clearGroup("locations") %>%
      addGlPoints(data = filtered_data_sf(), group = "locations", popup = TRUE, radius = 5)
  })
  
  
  observe({
    req(filtered_data())
    df_sf <- filtered_data_sf()
    leafletProxy("map") %>%
      clearGroup("locations") %>%
      addGlPoints(data = df_sf, group = "locations", popup = TRUE, radius = 5)
  })
  
  output$summary <- renderPrint({
    req(filtered_data(), rv$aoi)
    df_aoi <- filtered_data_aoi()
    cat("individuals in aoi:", length(unique(df_aoi$trackId)), "\n")
    cat("locations in aoi:", nrow(df_aoi), "\n")
  })
  
  output$bird_select <- renderUI({
    req(filtered_data(), rv$aoi)
    df_aoi <- filtered_data_aoi() %>% st_set_geometry(NULL)
    birds <- unique(as.character(df_aoi$trackId))
    selectInput("selected_bird", "Select bird", choices = c("All", birds), selected = "All")
  })
  
  observe({
    req(filtered_data(), rv$aoi, input$selected_bird)
    leafletProxy("map") %>% clearGroup("highlight_track")
    if (!is.null(input$selected_bird) && input$selected_bird != "All") {
      df <- filtered_data()
      bird_df <- df %>% filter(trackId == input$selected_bird)
      if (nrow(bird_df) > 1) {
        bird_sf <- st_as_sf(bird_df, coords = c("location_long", "location_lat"), crs = 4326)
        bird_sf <- bird_sf[order(bird_sf$timestamp), ]
        bird_coords <- st_coordinates(bird_sf)
        bird_line <- st_linestring(bird_coords)
        bird_line_sf <- st_sf(geometry = st_sfc(bird_line, crs = 4326))
        leafletProxy("map") %>%
          addPolylines(data = bird_line_sf, group = "highlight_track", color = "red", weight = 3, opacity = 0.9)
      }
    }
  })
  
  output$lat_plot <- renderPlotly({
    req(rv$aoi, filtered_data(), input$selected_bird)
    df_all <- filtered_data()
    df_aoi <- filtered_data_aoi() %>% st_set_geometry(NULL)
    aoi_track_ids <- unique(df_aoi$trackId)
    df_all <- df_all[trackId %in% aoi_track_ids]
    if (input$selected_bird != "All") {
      df_all <- df_all %>% filter(trackId == input$selected_bird)
    }
    if (nrow(df_all) == 0) {
      p <- ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "no data available", size = 6)
      return(ggplotly(p))
    }
    df_all <- df_all %>%
      mutate(date = as.Date(timestamp), trackId = as.character(trackId)) %>%
      arrange(trackId, date)
    p <- ggplot(df_all, aes(x = date, y = location_lat, color = trackId,
                            text = paste("trackId:", trackId, "<br>Date:", date))) +
      geom_line(alpha = 0.8, aes(group = trackId)) +
      geom_point(size = 0.5) +
      labs(y = "latitude", x = NULL) +
      scale_x_date(limits = as.Date(input$date_slider_input),
                   date_labels = "%Y.%m.%d", date_breaks = "2 weeks") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    ggplotly(p, tooltip = "text")
  })
  
  output$summary_table <- renderTable({
    req(filtered_data(), rv$aoi)
    df_all <- copy(filtered_data())
    df_aoi <- filtered_data_aoi() %>% st_set_geometry(NULL)
    setDT(df_aoi)
    if (input$selected_bird != "All") {
      df_all_ind <- df_all[trackId == input$selected_bird]
      df_aoi_ind <- df_aoi[trackId == input$selected_bird]
      df_all_ind[, timestamp := as.POSIXct(timestamp)]
      setorder(df_all_ind, timestamp)
      if (nrow(df_all_ind) < 2) {
        time_percent <- NA
        n_fixes_total <- nrow(df_all_ind)
        n_fixes_aoi <- nrow(df_aoi_ind)
      } else {
        df_all_ind[, time_diff := c(as.numeric(diff(timestamp)), NA)]
        df_all_ind[, in_aoi := FALSE]
        df_all_ind[timestamp %in% df_aoi_ind$timestamp, in_aoi := TRUE]
        total_time <- sum(df_all_ind$time_diff, na.rm = TRUE)
        time_in_aoi <- sum(df_all_ind[in_aoi == TRUE, time_diff], na.rm = TRUE)
        time_percent <- if (total_time > 0) round(100 * time_in_aoi / total_time, 1) else NA
        n_fixes_total <- nrow(df_all_ind)
        n_fixes_aoi <- sum(df_all_ind$in_aoi, na.rm = TRUE)
      }
      return(data.frame(
        metric = c("fixes in aoi", "total fixes", "estimated time in aoi (%)"),
        value = c(n_fixes_aoi, n_fixes_total,
                  ifelse(is.na(time_percent), "n/a", paste0(time_percent, "%")))
      ))
    } else {
      n_individuals_total <- uniqueN(df_all$trackId)
      n_individuals_aoi <- uniqueN(df_aoi$trackId)
      n_fixes_aoi <- nrow(df_aoi)
      percent_individuals_aoi <- round(100 * n_individuals_aoi / n_individuals_total, 1)
      if ("sex" %in% colnames(df_aoi)) {
        unique_ind <- df_aoi[, .SD[1], by = trackId]
        sex_counts <- table(na.omit(unique_ind$sex))
        n_males <- if ("M" %in% names(sex_counts)) sex_counts[["M"]] else 0
        n_females <- if ("F" %in% names(sex_counts)) sex_counts[["F"]] else 0
        sex_ratio <- paste0("M:", n_males, " | F:", n_females)
      } else {
        sex_ratio <- "n/a"
      }
      return(data.frame(
        metric = c("individuals in aoi", "fixes in aoi", "percent individuals in aoi (%)", "sex ratio (m:f)"),
        value = c(n_individuals_aoi, n_fixes_aoi, percent_individuals_aoi, sex_ratio)
      ))
    }
  })
  
  visit_summary <- reactive({
    req(filtered_data(), rv$aoi)
    df_all <- as.data.frame(filtered_data())
    if (nrow(df_all) == 0) return(NULL)
    df_all$row_id <- seq_len(nrow(df_all))
    df_all_sf <- st_as_sf(df_all, coords = c("location_long", "location_lat"), crs = 4326)
    if (!st_is_valid(rv$aoi)) rv$aoi <- st_make_valid(rv$aoi)
    df_all$in_aoi <- lengths(st_intersects(df_all_sf, rv$aoi)) > 0
    if (sum(df_all$in_aoi, na.rm = TRUE) == 0) return(NULL)
    df_all$timestamp <- as.POSIXct(df_all$timestamp)
    df_all <- df_all[order(df_all$trackId, df_all$timestamp), ]
    df_all <- df_all %>%
      group_by(trackId) %>%
      mutate(
        in_aoi_shift = lag(in_aoi, default = FALSE),
        visit_start_flag = in_aoi & !in_aoi_shift,
        visit_id = cumsum(visit_start_flag)
      ) %>%
      mutate(visit_id = ifelse(in_aoi, visit_id, NA_integer_)) %>%
      ungroup()
    visit_table <- df_all %>%
      filter(!is.na(visit_id)) %>%
      group_by(trackId, visit_id) %>%
      summarise(
        visit_start = min(timestamp),
        visit_end = max(timestamp),
        duration_days = ceiling(as.numeric(difftime(max(timestamp), min(timestamp), units = "days"))),
        n_fixes = n(),
        .groups = "drop"
      )
    visit_counts <- visit_table %>%
      group_by(trackId) %>%
      summarise(
        n_visits = n(),
        total_fixes = sum(n_fixes),
        .groups = "drop"
      )
    summary_table <- left_join(visit_table, visit_counts, by = "trackId") %>%
      mutate(
        visit_start = as.Date(visit_start),
        visit_end = as.Date(visit_end)
      ) %>%
      select(trackId, n_visits, total_fixes, visit_start, visit_end, duration_days, n_fixes)
    return(summary_table)
  })
  
  observeEvent(input$map_draw_new_feature, {
    if (input$country_aoi == "Custom") {
      coords <- input$map_draw_new_feature$geometry$coordinates[[1]]
      if (is.null(coords) || length(coords) < 3) return()
      coords <- matrix(unlist(coords), ncol = 2, byrow = TRUE)
      polygon <- st_polygon(list(coords))
      rv$aoi <- st_sf(geometry = st_sfc(polygon, crs = 4326))
      bbox <- st_bbox(rv$aoi)
      leafletProxy("map") %>% fitBounds(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
    }
  })
  
  output$aoi_drawn <- reactive({ !is.null(rv$aoi) })
  outputOptions(output, "aoi_drawn", suspendWhenHidden = FALSE)
  
  output$download_visits_ui <- renderUI({
    df_aoi <- tryCatch(filtered_data_aoi(), error = function(e) NULL)
    if (!is.null(rv$aoi) && !is.null(df_aoi) && nrow(df_aoi) > 0) {
      downloadButton("download_visits", "download aoi visit summary")
    }
  })
  
  output$download_visits <- downloadHandler(
    filename = function() {
      paste0("aoi_visit_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      visit_data <- tryCatch(isolate(visit_summary()), error = function(e) NULL)
      if (!is.null(visit_data)) {
        fwrite(visit_data, file)
      } else {
        fwrite(data.frame(note = "no data available or aoi not defined"), file)
      }
    }
  )
}

# run the app
shinyApp(ui, server)