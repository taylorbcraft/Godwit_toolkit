library(shiny)
library(leaflet)
library(leaflet.extras2)
library(sf)
library(dplyr)
library(leafgl)
library(terra)
library(ggplot2)
library(patchwork)

ui <- fluidPage(
  titlePanel("Grassland Production Intensity Viewer in Southwest Friesland"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      p("1. Filter location and GPI data by year and date."),
      p("2. Use the drawing tools on the map to draw an area of interest (AOI)."),
      p("3. The number of individuals, number of locations, a histogram of GPI values, and a boxplot comparing used vs available locations will be shown."),
      
      uiOutput("year_select"),
      uiOutput("date_slider"),
      uiOutput("file_info"),
      uiOutput("bird_select"),
      
      selectInput("basemap", "Choose basemap", 
                  choices = c("CartoDB Positron" = "CartoDB.Positron",
                              "Esri WorldImagery" = "Esri.WorldImagery"),
                  selected = "CartoDB.Positron")
    ),
    
    mainPanel(
      leafletOutput("map"),
      verbatimTextOutput("summary"),
      plotOutput("gpi_plots", height = "350px")
    )
  )
)

server <- function(input, output, session) {
  
  # load data
  all_locations <- reactive({
    req(file.exists("locations_swf.rds"))
    readRDS("locations_swf.rds")
  })
  
  rv <- reactiveValues(aoi = NULL)
  
  # load rasters
  gpi_rasters <- reactive({
    files <- list.files("gpi_data", pattern = "\\.tif$", full.names = TRUE)
    validate(need(length(files) > 0, "Loading GPI rasters... please wait"))
    
    rasters <- list()
    for (file in files) {
      year <- gsub(".*gpi_(\\d{4})\\.tif$", "\\1", file)
      r <- rast(file)
      r <- projectRasterForLeaflet(r, method = 'bilinear')
      r <- spatSample(r, 100000, method = "regular", as.raster = TRUE)
      rasters[[paste0("gpi_", year)]] <- r
    }
    rasters
  })
  
  # year select
  output$year_select <- renderUI({
    req(all_locations())
    loc_years <- unique(format(all_locations()$timestamp, "%Y"))
    raster_files <- list.files("gpi_data", pattern = "gpi_\\d{4}\\.tif$")
    raster_years <- gsub("gpi_(\\d{4})\\.tif", "\\1", raster_files)
    valid_years <- sort(intersect(loc_years, raster_years), decreasing = TRUE)
    year_choices <- c(valid_years, "All Years")
    default_year <- ifelse(length(valid_years) > 0, valid_years[1], "All Years")
    selectInput("year", "Select Year", choices = year_choices, selected = default_year)
  })
  
  # date slider
  output$date_slider <- renderUI({
    req(all_locations(), input$year)
    df <- all_locations()
    if (input$year != "All Years") {
      df <- df %>% filter(format(timestamp, "%Y") == input$year)
    }
    sliderInput("date_range", "Select Date Range", 
                min = min(df$timestamp), max = max(df$timestamp),
                value = c(min(df$timestamp), max(df$timestamp)), 
                timeFormat = "%Y-%m-%d", step = 1)
  })
  
  # filtered data
  filtered_data <- reactive({
    req(input$year, input$date_range, all_locations())
    df <- all_locations()
    if (input$year != "All Years") {
      df <- df %>% filter(format(timestamp, "%Y") == input$year)
    }
    df %>% filter(timestamp >= as.POSIXct(input$date_range[1]) & 
                    timestamp <= as.POSIXct(input$date_range[2]))
  })
  
  # bird select
  output$bird_select <- renderUI({
    req(filtered_data())
    birds <- sort(unique(as.character(filtered_data()$trackId)))
    selectInput("selected_bird", "Select individual", 
                choices = c("All Birds", birds), selected = "All Birds")
  })
  
  # bird data
  bird_data <- reactive({
    req(filtered_data())
    df <- filtered_data()
    if (input$selected_bird != "All Birds") {
      df <- df %>% filter(trackId == input$selected_bird)
    }
    if (!is.null(rv$aoi)) {
      df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
      df <- df_sf[st_intersects(df_sf, rv$aoi, sparse = FALSE), ] %>% st_drop_geometry()
    }
    df
  })
  
  # map
  output$map <- renderLeaflet({
    req(filtered_data())
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(
        leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
          addPopups(lng = -10, lat = 14, popup = "no data available for this timeframe")
      )
    }
    
    df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
    if (input$year == "All Years") {
      selected_raster <- gpi_rasters()[["gpi_2024"]]
    } else {
      selected_raster <- gpi_rasters()[[paste0("gpi_", input$year)]]
    }
    
    pal <- colorNumeric(palette = "YlGn", domain = values(selected_raster), na.color = "transparent")
    
    if (input$selected_bird != "All Birds") {
      df_sf <- df_sf %>% filter(trackId == input$selected_bird)
    }
    
    map <- leaflet() %>%
      addProviderTiles(providers[[input$basemap]]) %>%
      addGlPoints(data = df_sf, group = "locations", popup = TRUE,
                  radius = 3, fillColor = 'cyan') %>%
      addRasterImage(selected_raster, project = FALSE, colors = pal) %>%
      addDrawToolbar(
        targetGroup = "aoi",
        editOptions = editToolbarOptions(),
        polylineOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE
      ) %>%
      addLegend(position = "bottomright", pal = pal, values = values(selected_raster),
                title = "GPI", opacity = 0.7, bins = 2,
                labFormat = function(type, cuts, p) c("low", "high"))

    if (!is.null(rv$aoi)) {
      map <- map %>% addPolygons(data = rv$aoi, group = "aoi", color = "red", weight = 2, fill = FALSE)
    }
    
    # add red polyline if single bird selected
    if (input$selected_bird != "All Birds") {
      df_path <- df %>%
        filter(trackId == input$selected_bird) %>%
        arrange(timestamp)
      
      if (nrow(df_path) > 1) {
        df_path_sf <- st_as_sf(df_path, coords = c("location_long", "location_lat"), crs = 4326)
        df_line <- df_path_sf %>%
          summarise(do_union = FALSE) %>%
          st_cast("LINESTRING")
        
        map <- map %>%
          addPolylines(data = df_line, color = "red", weight = 2, group = "selected path")
      }
    }
    
    map
  })
  
  # AOI updates
  observeEvent(input$map_draw_new_feature, {
    coords <- input$map_draw_new_feature$geometry$coordinates[[1]]
    coords <- matrix(unlist(coords), ncol = 2, byrow = TRUE)
    rv$aoi <- st_as_sf(st_sfc(st_polygon(list(coords)), crs = 4326))
  })

  observeEvent(input$map_draw_edited_features, {
    features <- input$map_draw_edited_features$features
    if (length(features) == 0) return()
    coords <- features[[1]]$geometry$coordinates[[1]]
    coords <- matrix(unlist(coords), ncol = 2, byrow = TRUE)
    rv$aoi <- st_as_sf(st_sfc(st_polygon(list(coords)), crs = 4326))
  })

  observeEvent(input$map_draw_deleted_features, {
    rv$aoi <- NULL
  })
  
  # summary
  output$summary <- renderPrint({
    req(filtered_data())
    df <- filtered_data()
    if (!is.null(rv$aoi)) {
      df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
      df <- df_sf[st_intersects(df_sf, rv$aoi, sparse = FALSE), ]
    }
    if (nrow(df) == 0) {
      cat("No tracks available for the selected time window.\n")
    } else {
      cat("Number of individuals within AOI:", length(unique(df$trackId)), "\n")
      cat("Total number of locations within AOI:", nrow(df), "\n")
    }
  })
  
  # use vs availability summary
  use_avail_summary <- reactive({
    req(bird_data(), input$year)
    df_sf <- st_as_sf(bird_data(), coords = c("location_long", "location_lat"), crs = 4326)
    
    if (input$year == "All Years") {
      r <- gpi_rasters()[["gpi_2024"]]
    } else {
      r <- gpi_rasters()[[paste0("gpi_", input$year)]]
    }
    req(r)
    
    df_sf <- st_transform(df_sf, crs(r))
    df_sf$type <- "used"
    
    set.seed(42)
    avail_pts <- spatSample(r,
                            size = nrow(df_sf) * 10,
                            method = "random",
                            as.points = TRUE,
                            na.rm = TRUE)
    n_avail <- nrow(avail_pts)
    
    avail_sf <- st_as_sf(avail_pts) |>
      mutate(
        trackId   = rep(df_sf$trackId, length.out = n_avail),
        timestamp = rep(df_sf$timestamp, length.out = n_avail),
        type      = "available"
      )
    
    combined_sf <- rbind(
      df_sf[, c("trackId", "timestamp", "type", "geometry")],
      avail_sf[, c("trackId", "timestamp", "type", "geometry")]
    )
    
    vals <- terra::extract(r, vect(combined_sf))
    combined_sf$gpi_val <- vals[, 2]
    st_drop_geometry(combined_sf)
  })
  
  # combined histogram + boxplot
  output$gpi_plots <- renderPlot({
    req(bird_data(), input$year)
    df <- bird_data()
    if (nrow(df) == 0) {
      plot.new(); text(0.5, 0.5, "no points in selected AOI or timeframe", cex = 1.5); return()
    }
    
    # select raster
    if (input$year == "All Years") {
      r <- gpi_rasters()[["gpi_2024"]]
    } else {
      r <- gpi_rasters()[[paste0("gpi_", input$year)]]
    }
    req(r)
    
    # extract GPI for used points
    df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326) |>
      st_transform(crs(r))
    vals <- terra::extract(r, vect(df_sf))
    gpi_vals <- vals[, 2]
    gpi_vals <- gpi_vals[!is.na(gpi_vals)]
    
    if (length(gpi_vals) == 0) {
      plot.new(); text(0.5, 0.5, "no GPI values available", cex = 1.5); return()
    }
    
    # Histogram
    p1 <- ggplot(data.frame(gpi_val = gpi_vals), aes(x = gpi_val)) +
      geom_histogram(fill = "darkgreen", color = "white", bins = 30) +
      labs(x = "Grassland Production Intensity", y = "Godwit Locations") +
      theme_minimal(base_size = 14)
    
    # Use vs availability boxplot
    df_use_avail <- use_avail_summary()
    if (nrow(df_use_avail) > 0) {
      df_use_avail$type <- factor(df_use_avail$type, levels = c("used", "available"))
      
      p2 <- ggplot(df_use_avail, aes(x = type, y = gpi_val, fill = type)) +
        geom_boxplot(width = 0.6, outlier.alpha = 0.3) +
        labs(x = NULL, y = "Grassland Production Intensity") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(size = 13), legend.position = "none") +
        scale_fill_manual(values = c("used" = "lightyellow", "available" = "darkgreen"))
      
      p1 + p2
    } else {
      p1
    }
  })
}

shinyApp(ui, server)
