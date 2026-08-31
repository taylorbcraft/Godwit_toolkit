library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(leafgl)
library(ggplot2)
library(plotly)
library(data.table)
library(DT)
library(shinycssloaders)

# load and prepare movement data
all_data <- readRDS("allLocations.rds")
setDT(all_data)
all_data[, timestamp := as.POSIXct(timestamp, tz = "UTC")]
all_data[, date := as.Date(timestamp)]
all_data[, calendar_year := as.integer(format(timestamp, "%Y"))]
all_data[, month := as.integer(format(timestamp, "%m"))]
all_data[, migration_year := ifelse(
  month >= 7,
  paste0(calendar_year, "-", calendar_year + 1),
  paste0(calendar_year - 1, "-", calendar_year)
)]
setindex(all_data, migration_year, timestamp, tag_site)

countries_sf <- readRDS("countries_sf.rds") %>%
  st_transform(4326) %>%
  st_make_valid()

country_names <- sort(unique(countries_sf$name))
migration_years <- sort(unique(all_data$migration_year))
tag_sites <- sort(unique(as.character(na.omit(all_data$tag_site))))
default_migration_year <- tail(migration_years, 1)
default_year_dates <- range(all_data[migration_year == default_migration_year]$date)

filter_to_aoi <- function(df, aoi) {
  if (is.null(aoi) || nrow(df) == 0) {
    return(df)
  }

  # limit exact spatial tests to nearby locations
  aoi <- st_make_valid(st_transform(aoi, 4326)) %>% st_union()
  bbox <- st_bbox(aoi)
  candidates <- df[
    location_long >= bbox[["xmin"]] &
      location_long <= bbox[["xmax"]] &
      location_lat >= bbox[["ymin"]] &
      location_lat <= bbox[["ymax"]]
  ]

  if (nrow(candidates) == 0) {
    return(candidates)
  }

  candidate_points <- st_as_sf(
    candidates,
    coords = c("location_long", "location_lat"),
    crs = 4326,
    remove = FALSE
  )
  inside_aoi <- lengths(st_intersects(candidate_points, aoi)) > 0
  candidates[inside_aoi]
}

prepare_map_points <- function(df, detail) {
  if (nrow(df) == 0) {
    return(st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326))
  }

  if (detail == "daily") {
    df <- df[order(timestamp), .SD[1], by = .(trackId, date)]
  }

  df[, popup_text := paste0(
    "<strong>", ifelse(is.na(ring_id), trackId, ring_id), "</strong><br>",
    format(timestamp, "%Y-%m-%d %H:%M UTC"), "<br>",
    "Tagging site: ", ifelse(is.na(tag_site), "Unknown", tag_site), "<br>",
    "Study: <a href='https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study",
    study_id, "' target='_blank'>", study_name, "</a>"
  )]

  st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
}

read_uploaded_aoi <- function(files) {
  extensions <- tolower(tools::file_ext(files$name))

  if ("shp" %in% extensions) {
    upload_dir <- dirname(files$datapath[1])
    file.copy(files$datapath, file.path(upload_dir, files$name), overwrite = TRUE)
    shp_name <- files$name[which(extensions == "shp")[1]]
    layer_name <- tools::file_path_sans_ext(shp_name)
    aoi <- st_read(dsn = upload_dir, layer = layer_name, quiet = TRUE)
  } else {
    aoi <- st_read(files$datapath[1], quiet = TRUE)
  }

  if (is.na(st_crs(aoi))) {
    st_crs(aoi) <- 4326
  } else {
    aoi <- st_transform(aoi, 4326)
  }

  st_make_valid(aoi) %>% st_union() %>% st_as_sf()
}

theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#b4532a",
  secondary = "#53636b",
  bg = "#f4f6f5",
  fg = "#203033",
  base_font = font_collection("Source Sans 3", "Segoe UI", "Arial", "sans-serif"),
  heading_font = font_collection("Source Sans 3", "Segoe UI", "Arial", "sans-serif")
)

ui <- page_fillable(
  theme = theme,
  padding = 0,
  tags$head(
    tags$style(HTML("
      .app-header { display: flex; align-items: center; min-height: 58px; padding: 0 1.25rem;
        background: #203033; color: white; box-shadow: 0 2px 12px rgba(24, 46, 49, .16); }
      .app-title { display: flex; align-items: center; font-size: 1.25rem; font-weight: 700; }
      .bslib-sidebar-layout > .sidebar { border-right: 1px solid #dde4e2; }
      .control-label { font-weight: 600; color: #33484c; }
      .filter-help { color: #66787b; font-size: .9rem; line-height: 1.35; }
      .metric-grid { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: .75rem; }
      .metric-card { border: 0; box-shadow: 0 2px 10px rgba(24, 46, 49, .08); }
      .metric-card .card-body { padding: .8rem 1rem; }
      .metric-label { color: #66787b; font-size: .78rem; font-weight: 700;
        letter-spacing: .04em; text-transform: uppercase; }
      .metric-value { color: #203033; font-size: 1.65rem; font-weight: 700; line-height: 1.2; }
      .map-card { overflow: hidden; border: 0; box-shadow: 0 3px 16px rgba(24, 46, 49, .10); }
      .map-card .card-body { padding: 0; }
      .analysis-card { border: 0; box-shadow: 0 2px 12px rgba(24, 46, 49, .08); }
      .empty-state { padding: 2rem; color: #66787b; text-align: center; }
      .download-stack .btn { width: 100%; margin-bottom: .5rem; }
      @media (max-width: 900px) { .metric-grid { grid-template-columns: repeat(2, 1fr); } }
    "))
  ),
  div(
    class = "app-header",
    div(
      class = "app-title",
      span("Godwit Movement Explorer")
    )
  ),
  layout_sidebar(
      sidebar = sidebar(
        width = 330,
        open = "desktop",
        h5("Movement filters"),
        selectInput(
          "migration_year",
          "Migration year",
          choices = migration_years,
          selected = default_migration_year
        ),
        sliderInput(
          "date_range",
          "Date range",
          min = default_year_dates[1],
          max = default_year_dates[2],
          value = default_year_dates,
          timeFormat = "%d %b %Y"
        ),
        selectInput(
          "tag_site",
          "Tagging site",
          choices = c("All sites" = "All", tag_sites),
          selected = "All"
        ),
        hr(),
        h5("Area of interest"),
        p(class = "filter-help", "Choose a country, upload a boundary, or draw a polygon on the map."),
        selectizeInput(
          "country_aoi",
          "Country",
          choices = c("No country selected" = "Custom", country_names),
          selected = "Custom"
        ),
        fileInput(
          "aoi_upload",
          "Upload AOI",
          multiple = TRUE,
          accept = c(".shp", ".dbf", ".shx", ".prj", ".geojson", ".json", ".gpkg", ".kml", ".gml")
        ),
        actionButton("clear_aoi", "Clear AOI", class = "btn-outline-secondary w-100"),
        hr(),
        h5("Map display"),
        selectInput(
          "basemap",
          "Basemap",
          choices = c(
            "OpenStreetMap" = "OpenStreetMap.Mapnik",
            "Satellite" = "Esri.WorldImagery"
          ),
          selected = "OpenStreetMap.Mapnik"
        ),
        radioButtons(
          "map_detail",
          "Location detail",
          choices = c("Daily locations" = "daily", "All fixes" = "all"),
          selected = "daily"
        ),
        p(class = "filter-help", "Daily locations are faster and recommended at flyway scale."),
        hr(),
        uiOutput("download_visits_ui")
      ),
      div(
        class = "d-flex flex-column gap-3 h-100",
        div(
          class = "metric-grid",
          card(class = "metric-card", div(class = "metric-label", "Locations"), div(class = "metric-value", textOutput("location_count", inline = TRUE))),
          card(class = "metric-card", div(class = "metric-label", "Individuals"), div(class = "metric-value", textOutput("individual_count", inline = TRUE))),
          card(class = "metric-card", div(class = "metric-label", "Studies"), div(class = "metric-value", textOutput("study_count", inline = TRUE))),
          card(class = "metric-card", div(class = "metric-label", "Area"), div(class = "metric-value", textOutput("aoi_status", inline = TRUE)))
        ),
        navset_card_tab(
          id = "main_tab",
          height = "100%",
          full_screen = TRUE,
          nav_panel(
            "Map",
            withSpinner(leafletOutput("map", height = "calc(100vh - 245px)"), type = 6, color = "#b4532a")
          ),
          nav_panel(
            "Movement and visits",
            div(
              class = "p-3",
              uiOutput("analysis_controls"),
              withSpinner(plotlyOutput("lat_plot", height = "360px"), type = 6, color = "#b4532a"),
              layout_columns(
                card(class = "analysis-card", card_header("Selection summary"), tableOutput("summary_table")),
                card(class = "analysis-card", card_header("AOI visits"), uiOutput("visit_controls"), DTOutput("visit_table")),
                col_widths = c(4, 8)
              )
            )
          )
        )
      )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(aoi = NULL, aoi_name = "Entire flyway")

  observeEvent(input$migration_year, {
    year_data <- all_data[migration_year == input$migration_year]
    updateSliderInput(
      session,
      "date_range",
      min = min(year_data$date),
      max = max(year_data$date),
      value = range(year_data$date)
    )
  }, ignoreInit = TRUE)

  selected_dates <- reactive(input$date_range) %>% debounce(400)

  filtered_data <- reactive({
    if (is.null(input$migration_year) || is.null(selected_dates())) {
      return(all_data[0])
    }

    date_range <- as.Date(selected_dates())

    df <- all_data[
      migration_year == input$migration_year &
        date >= date_range[1] &
        date <= date_range[2]
    ]

    if (!is.null(input$tag_site) && input$tag_site != "All") {
      df <- df[tag_site == input$tag_site]
    }

    df
  })

  aoi_filtered_data <- reactive({
    filter_to_aoi(filtered_data(), rv$aoi)
  })

  map_points <- reactive({
    df <- copy(aoi_filtered_data())
    if (nrow(df) == 0) {
      return(NULL)
    }

    detail <- if (is.null(input$map_detail)) "daily" else input$map_detail
    prepare_map_points(df, detail)
  })

  observeEvent(input$country_aoi, {
    if (input$country_aoi == "Custom") {
      return()
    }

    aoi <- countries_sf %>% filter(name == input$country_aoi)
    rv$aoi <- aoi
    rv$aoi_name <- input$country_aoi
    bbox <- st_bbox(aoi)
    leafletProxy("map") %>% fitBounds(
      bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]
    )
  }, ignoreInit = TRUE)

  observeEvent(input$aoi_upload, {
    aoi <- tryCatch(read_uploaded_aoi(input$aoi_upload), error = function(e) NULL)

    if (is.null(aoi)) {
      showNotification("The uploaded AOI could not be read.", type = "error")
      return()
    }

    rv$aoi <- aoi
    rv$aoi_name <- tools::file_path_sans_ext(input$aoi_upload$name[1])
    updateSelectInput(session, "country_aoi", selected = "Custom")
    bbox <- st_bbox(aoi)
    leafletProxy("map") %>% fitBounds(
      bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]
    )
  })

  observeEvent(input$clear_aoi, {
    rv$aoi <- NULL
    rv$aoi_name <- "Entire flyway"
    updateSelectInput(session, "country_aoi", selected = "Custom")
    leafletProxy("map") %>% setView(lng = 5, lat = 40, zoom = 4)
  })

  observeEvent(input$map_draw_new_feature, {
    coordinates <- input$map_draw_new_feature$geometry$coordinates[[1]]
    if (is.null(coordinates) || length(coordinates) < 3) {
      return()
    }

    coordinates <- matrix(unlist(coordinates), ncol = 2, byrow = TRUE)
    rv$aoi <- st_sf(geometry = st_sfc(st_polygon(list(coordinates)), crs = 4326))
    rv$aoi_name <- "Drawn AOI"
    updateSelectInput(session, "country_aoi", selected = "Custom")
  })

  observeEvent(input$map_draw_edited_features, {
    features <- input$map_draw_edited_features$features
    if (length(features) == 0) {
      return()
    }

    coordinates <- features[[1]]$geometry$coordinates[[1]]
    coordinates <- matrix(unlist(coordinates), ncol = 2, byrow = TRUE)
    rv$aoi <- st_sf(geometry = st_sfc(st_polygon(list(coordinates)), crs = 4326))
    rv$aoi_name <- "Drawn AOI"
  })

  observeEvent(input$map_draw_deleted_features, {
    rv$aoi <- NULL
    rv$aoi_name <- "Entire flyway"
  })

  output$map <- renderLeaflet({
    basemap <- if (is.null(input$basemap)) "OpenStreetMap.Mapnik" else input$basemap
    leaflet() %>%
      addProviderTiles(providers[[isolate(basemap)]], layerId = "basemap") %>%
      setView(lng = 5, lat = 40, zoom = 4) %>%
      addDrawToolbar(
        targetGroup = "aoi",
        editOptions = editToolbarOptions(),
        polylineOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE
      )
  })

  # update basemap independently
  observeEvent(input$basemap, {
    leafletProxy("map") %>%
      clearTiles() %>%
      addProviderTiles(providers[[input$basemap]], layerId = "basemap")
  }, ignoreInit = TRUE)

  # update AOI independently
  observeEvent(rv$aoi, {
    proxy <- leafletProxy("map") %>% clearGroup("aoi")
    if (!is.null(rv$aoi)) {
      proxy %>% addPolygons(
        data = rv$aoi,
        group = "aoi",
        color = "#238b8d",
        weight = 3,
        fillColor = "#2a9d8f",
        fillOpacity = 0.08
      )
    }
  }, ignoreInit = FALSE)

  # update movement points independently
  observeEvent(list(aoi_filtered_data(), input$map_detail), {
    points <- map_points()
    proxy <- leafletProxy("map") %>%
      clearGroup("locations") %>%
      clearGroup("highlight_track")

    if (!is.null(points)) {
      proxy %>% addGlPoints(
        data = points,
        group = "locations",
        popup = ~popup_text,
        color = "#165dff",
        opacity = 0.72,
        radius = 4
      )
    }
  }, ignoreInit = FALSE)

  output$location_count <- renderText(format(nrow(aoi_filtered_data()), big.mark = ","))
  output$individual_count <- renderText(format(uniqueN(aoi_filtered_data()$trackId), big.mark = ","))
  output$study_count <- renderText(format(uniqueN(aoi_filtered_data()$study_id), big.mark = ","))
  output$aoi_status <- renderText(rv$aoi_name)

  output$analysis_controls <- renderUI({
    df <- aoi_filtered_data()
    birds <- sort(unique(as.character(df$trackId)))

    selectizeInput(
      "selected_bird",
      "Individual",
      choices = c("All individuals" = "All", birds),
      selected = "All",
      options = list(placeholder = "Search by track ID")
    )
  })

  observeEvent(input$selected_bird, {
    proxy <- leafletProxy("map") %>% clearGroup("highlight_track")
    req(input$selected_bird != "All")

    bird_data <- filtered_data()[trackId == input$selected_bird][order(timestamp)]
    if (nrow(bird_data) < 2) {
      return()
    }

    coordinates <- as.matrix(bird_data[, .(location_long, location_lat)])
    bird_line <- st_sf(geometry = st_sfc(st_linestring(coordinates), crs = 4326))
    proxy %>% addPolylines(
      data = bird_line,
      group = "highlight_track",
      color = "#d1492e",
      weight = 4,
      opacity = 0.95
    )
  }, ignoreInit = TRUE)

  output$lat_plot <- renderPlotly({
    df <- copy(aoi_filtered_data())
    validate(need(nrow(df) > 0, "No movement data match the current filters."))

    if (!is.null(input$selected_bird) && input$selected_bird != "All") {
      df <- filtered_data()[trackId == input$selected_bird]
    }

    plot_data <- df[order(timestamp), .SD[1], by = .(trackId, date)]
    plot_data[, trackId := as.character(trackId)]

    plot <- ggplot(
      plot_data,
      aes(
        x = date,
        y = location_lat,
        color = trackId,
        group = trackId,
        text = paste0("Bird: ", trackId, "<br>Date: ", date, "<br>Latitude: ", round(location_lat, 2))
      )
    ) +
      geom_line(alpha = 0.65, linewidth = 0.5) +
      geom_point(size = 0.8, alpha = 0.75) +
      labs(x = NULL, y = "Latitude") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none", panel.grid.minor = element_blank())

    ggplotly(plot, tooltip = "text") %>% config(displaylogo = FALSE)
  })

  output$summary_table <- renderTable({
    df <- aoi_filtered_data()
    validate(need(nrow(df) > 0, "No data available."))

    if (!is.null(input$selected_bird) && input$selected_bird != "All") {
      bird_aoi <- df[trackId == input$selected_bird]
      bird_all <- filtered_data()[trackId == input$selected_bird]
      return(data.frame(
        Metric = c("Fixes in AOI", "Total fixes", "First fix", "Last fix"),
        Value = c(nrow(bird_aoi), nrow(bird_all), as.character(min(bird_all$date)), as.character(max(bird_all$date)))
      ))
    }

    data.frame(
      Metric = c("Individuals", "Locations", "Studies", "Tagging sites"),
      Value = c(uniqueN(df$trackId), nrow(df), uniqueN(df$study_id), uniqueN(na.omit(df$tag_site)))
    )
  }, striped = TRUE, bordered = FALSE)

  build_visit_summary <- function(df, aoi) {
    if (is.null(aoi) || nrow(df) == 0) {
      return(NULL)
    }

    df <- copy(as.data.table(df))
    df[, in_aoi := FALSE]
    aoi_points <- filter_to_aoi(df, aoi)

    if (nrow(aoi_points) == 0) {
      return(NULL)
    }

    df[aoi_points, on = .(trackId, timestamp), in_aoi := TRUE]
    setorder(df, trackId, timestamp)
    df[, previous_in_aoi := shift(in_aoi, fill = FALSE), by = trackId]
    df[, visit_start := in_aoi & !previous_in_aoi]
    df[, visit_id := cumsum(visit_start), by = trackId]

    visits <- df[in_aoi == TRUE, .(
      study_name = first(study_name),
      visit_start = min(timestamp),
      visit_end = max(timestamp),
      duration_days = ceiling(as.numeric(difftime(max(timestamp), min(timestamp), units = "days"))),
      n_fixes = .N
    ), by = .(trackId, visit_id)]

    visits[, n_visits := .N, by = trackId]
    visits[, `:=`(
      visit_start = as.character(as.Date(visit_start)),
      visit_end = as.character(as.Date(visit_end))
    )]
    visits[, .(trackId, study_name, n_visits, visit_start, visit_end, duration_days, n_fixes)]
  }

  visit_summary <- eventReactive(input$calculate_visits, {
    req(!is.null(rv$aoi))
    withProgress(message = "Calculating AOI visits", value = 0.4, {
      build_visit_summary(filtered_data(), rv$aoi)
    })
  }, ignoreInit = TRUE)

  output$visit_controls <- renderUI({
    if (is.null(rv$aoi)) {
      return(div(class = "empty-state", "Select or draw an AOI to calculate visits."))
    }

    actionButton("calculate_visits", "Calculate visits", class = "btn-primary mb-3")
  })

  output$visit_table <- renderDT({
    visits <- visit_summary()
    validate(need(!is.null(visits) && nrow(visits) > 0, "No visits calculated for the current selection."))
    datatable(
      visits,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$download_visits_ui <- renderUI({
    if (is.null(rv$aoi)) {
      return(NULL)
    }

    div(
      class = "download-stack",
      downloadButton("download_visits", "Download selected-year visits")
    )
  })

  output$download_visits <- downloadHandler(
    filename = function() paste0("godwit_visits_", input$migration_year, ".csv"),
    content = function(file) {
      visits <- visit_summary()
      if (is.null(visits)) {
        visits <- data.table(note = "Calculate visits before downloading")
      }
      fwrite(visits, file)
    }
  )

}

shinyApp(ui, server)
