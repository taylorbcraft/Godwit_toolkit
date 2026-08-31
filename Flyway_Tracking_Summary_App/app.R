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

options(shiny.maxRequestSize = 100 * 1024^2)

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
index_display_ranges <- list(
  GPI = c(708, 741),
  NDVI = c(-0.2, 0.9),
  EVI = c(-0.2, 1),
  NDWI = c(-0.5, 0.8),
  NDMI = c(-0.5, 0.8),
  SAVI = c(-0.2, 1),
  SWIR = c(0, 0.3)
)

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
      .metric-grid { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: .35rem; }
      .metric-card { --bslib-card-spacer-y: .2rem; --bslib-card-spacer-x: .55rem;
        min-height: 0; border: 0; box-shadow: 0 1px 6px rgba(24, 46, 49, .07); }
      .metric-card .card-body { min-height: 0; padding: .2rem .55rem !important; }
      .metric-label { color: #66787b; font-size: .62rem; font-weight: 700;
        letter-spacing: .04em; text-transform: uppercase; }
      .metric-value { color: #203033; font-size: 1.05rem; font-weight: 700; line-height: 1.05; }
      .map-card { overflow: hidden; border: 0; box-shadow: 0 3px 16px rgba(24, 46, 49, .10); }
      .map-card .card-body { padding: 0; }
      .analysis-card { border: 0; box-shadow: 0 2px 12px rgba(24, 46, 49, .08); }
      .empty-state { padding: 2rem; color: #66787b; text-align: center; }
      .download-stack .btn { width: 100%; margin-bottom: .5rem; }
      .gee-frame { width: 100%; height: 820px; border: 1px solid #d7dfdd;
        border-radius: .4rem; background: white; }
      .workflow-step { color: #66787b; font-size: .9rem; margin: 0; }
      details > summary { cursor: pointer; color: #33484c; font-weight: 600; }
      details[open] > summary { margin-bottom: 1rem; }
      @media (max-width: 900px) { .metric-grid { grid-template-columns: repeat(2, 1fr); } }
    "))
  ),
  div(
    class = "app-header",
    div(
      class = "app-title",
      span("Godwit Movement & Environment Explorer")
    )
  ),
  layout_sidebar(
      sidebar = sidebar(
        width = 330,
        open = "desktop",
        tags$details(
          open = NA,
          tags$summary("About this app"),
          p(
            class = "filter-help mt-2",
            "Explore godwit movements, filter locations by time, tagging site or area, and compare godwit tracks with satellite layers"
          ),
          tags$ol(
            class = "filter-help ps-3",
            tags$li("Filter the movement records."),
            tags$li("Draw, choose or upload an area when needed."),
            tags$li("Create and upload a clipped image in the Environment tab."),
            tags$li("Review sampled values or download a summary.")
          )
        ),
        hr(),
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
        selectizeInput(
          "tag_site",
          "Tagging sites",
          choices = tag_sites,
          selected = tag_sites,
          multiple = TRUE,
          options = list(plugins = list("remove_button"), placeholder = "Choose tagging sites")
        ),
        tags$details(
          tags$summary("Focus on an area"),
          p(class = "filter-help mt-2", "Choose a country, upload a boundary, or draw a polygon on the map."),
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
          actionButton("clear_aoi", "Clear AOI", class = "btn-outline-secondary w-100")
        ),
        tags$details(
          tags$summary("Map options"),
          selectInput(
            "basemap",
            "Basemap",
            choices = c(
              "OpenStreetMap" = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
              "Satellite" = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
            ),
            selected = "https://tile.openstreetmap.org/{z}/{x}/{y}.png"
          ),
          radioButtons(
            "map_detail",
            "Location detail",
            choices = c("Daily locations" = "daily", "All fixes" = "all"),
            selected = "daily"
          ),
          p(class = "filter-help", "Daily locations are faster and recommended at flyway scale.")
        ),
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
            "Movement",
            div(
              class = "p-3 d-flex flex-column gap-3",
              card(
                class = "map-card",
                withSpinner(leafletOutput("map", height = "calc(100vh - 310px)"), type = 6, color = "#b4532a")
              ),
              tags$details(
                tags$summary("Explore movement patterns and visits"),
                div(
                  class = "pt-3",
                  uiOutput("analysis_controls"),
                  withSpinner(plotlyOutput("lat_plot", height = "360px"), type = 6, color = "#b4532a"),
                  layout_columns(
                    card(class = "analysis-card", card_header("Selection summary"), tableOutput("summary_table")),
                    card(class = "analysis-card", card_header("Visits in the selected area"), uiOutput("visit_controls"), DTOutput("visit_table")),
                    col_widths = c(4, 8)
                  )
                )
              )
            )
          ),
          nav_panel(
            "Environment",
            div(
              class = "p-3 d-flex flex-column gap-3",
              card(
                class = "analysis-card",
                card_header("1. Build a satellite layer in Google Earth Engine"),
                p(
                  class = "filter-help",
                  "Define an area and period and choose an index. Before downloading, draw a clipping area around the area you need. Clipping is required to keep the download size manageable."
                ),
                tags$details(
                  tags$summary("Satellite Index Explorer"),
                  tags$iframe(
                    class = "gee-frame",
                    title = "Google Earth Engine Satellite Index Explorer",
                    src = "https://ee-tbcraft.projects.earthengine.app/view/satellite-index-explorer",
                    allow = "fullscreen"
                  )
                ),
                tags$a(
                  "Open Satellite Index Explorer in separate window",
                  href = "https://ee-tbcraft.projects.earthengine.app/view/satellite-index-explorer",
                  target = "_blank",
                  rel = "noopener noreferrer",
                  class = "btn btn-outline-secondary btn-sm align-self-start mt-2"
                )
              ),
              card(
                class = "analysis-card",
                card_header("2. Upload and analyse the satellite layer"),
                layout_columns(
                  fileInput(
                    "generated_raster",
                    "Upload the GeoTIFF downloaded from the Satellite Index Explorer",
                    accept = c(".tif", ".tiff")
                  ),
                  div(
                    p(
                      class = "filter-help",
                      "The first raster band is sampled at the currently filtered bird-day locations. Change the movement filters or AOI to update every result below."
                    ),
                    uiOutput("generated_raster_status")
                  ),
                  col_widths = c(4, 8)
                )
              ),
              uiOutput("generated_raster_results")
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

    if (!is.null(input$tag_site)) {
      df <- df[tag_site %chin% input$tag_site]
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
    basemap <- if (is.null(input$basemap)) "https://tile.openstreetmap.org/{z}/{x}/{y}.png" else input$basemap
    attribution <- if (grepl("arcgisonline", basemap)) "Tiles &copy; Esri" else "&copy; OpenStreetMap contributors"
    leaflet() %>%
      addTiles(urlTemplate = isolate(basemap), attribution = attribution, layerId = "basemap") %>%
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
    attribution <- if (grepl("arcgisonline", input$basemap)) "Tiles &copy; Esri" else "&copy; OpenStreetMap contributors"
    leafletProxy("map") %>%
      clearTiles() %>%
      addTiles(urlTemplate = input$basemap, attribution = attribution, layerId = "basemap")
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

  generated_raster <- reactive({
    req(input$generated_raster)
    validate(need(
      tolower(tools::file_ext(input$generated_raster$name)) %in% c("tif", "tiff"),
      "Upload a GeoTIFF file."
    ))

    raster <- tryCatch(
      terra::rast(input$generated_raster$datapath),
      error = function(error) NULL
    )
    validate(need(!is.null(raster), "The uploaded GeoTIFF could not be read."))
    validate(need(!is.na(terra::crs(raster)), "The uploaded GeoTIFF has no coordinate reference system."))
    raster
  })

  generated_raster_values <- reactive({
    raster <- generated_raster()

    movement_data <- copy(aoi_filtered_data())
    validate(need(nrow(movement_data) > 0, "No movement data match the current filters."))
    daily_locations <- movement_data[order(timestamp), .SD[1], by = .(trackId, date)]

    points <- terra::vect(
      daily_locations,
      geom = c("location_long", "location_lat"),
      crs = "EPSG:4326"
    )
    extracted <- terra::extract(raster[[1]], points, ID = FALSE)

    data.table(
      track_id = as.character(daily_locations$trackId),
      date = daily_locations$date,
      longitude = daily_locations$location_long,
      latitude = daily_locations$location_lat,
      value = as.numeric(extracted[[1]])
    )
  })

  output$generated_raster_status <- renderUI({
    raster <- generated_raster()
    values <- generated_raster_values()
    covered <- sum(!is.na(values$value))
    p(
      class = "filter-help",
      paste0(
        input$generated_raster$name, " · ", terra::ncol(raster), " × ", terra::nrow(raster),
        " pixels · ", format(covered, big.mark = ","), " daily bird records inside valid raster cells"
      )
    )
  })

  generated_raster_label <- reactive({
    supported_indices <- c("GPI", "NDVI", "EVI", "NDWI", "NDMI", "SAVI", "SWIR")
    raster_text <- toupper(paste(
      names(generated_raster()),
      tools::file_path_sans_ext(input$generated_raster$name),
      collapse = " "
    ))
    selected_index <- supported_indices[vapply(
      supported_indices,
      function(index) grepl(paste0("(^|[^A-Z0-9])", index, "([^A-Z0-9]|$)"), raster_text),
      logical(1)
    )]
    if (length(selected_index) > 0) {
      return(selected_index[1])
    }

    sampled_values <- generated_raster_values()$value
    sampled_median <- median(sampled_values, na.rm = TRUE)
    if (is.finite(sampled_median) && abs(sampled_median) > 2) {
      return("GPI")
    }

    "Satellite index"
  })

  output$generated_raster_results <- renderUI({
    if (is.null(input$generated_raster)) {
      return(div(
        class = "empty-state",
        "Generate a satellite layer in Earth Engine, or upload a GeoTIFF to begin the movement–environment analysis."
      ))
    }

    div(
      class = "d-flex flex-column gap-3",
      layout_columns(
        card(
          class = "analysis-card",
          card_header("Satellite layer and daily bird records"),
          withSpinner(leafletOutput("generated_raster_map", height = "600px"), type = 6, color = "#b4532a")
        ),
        card(
          class = "analysis-card",
          card_header("Selection summary"),
          tableOutput("generated_raster_summary")
        ),
        col_widths = c(8, 4)
      ),
      tags$details(
        tags$summary("Explore results"),
        div(
          class = "pt-3",
          card(
            class = "analysis-card",
            withSpinner(plotlyOutput("generated_raster_distribution", height = "340px"), type = 6, color = "#b4532a")
          )
        )
      )
    )
  })

  output$generated_raster_summary <- renderTable({
    all_values <- generated_raster_values()
    values <- all_values[!is.na(value)]
    validate(need(nrow(values) > 0, "The raster does not overlap the selected daily bird records."))

    data.frame(
      Metric = c("Daily bird records sampled", "Individuals", "Median", "Mean", "Range", "Interquartile range"),
      Value = c(
        format(nrow(values), big.mark = ","),
        format(uniqueN(values$track_id), big.mark = ","),
        round(median(values$value), 3),
        round(mean(values$value), 3),
        paste(round(range(values$value), 3), collapse = " – "),
        paste(round(quantile(values$value, c(0.25, 0.75)), 3), collapse = " – ")
      )
    )
  }, striped = TRUE, bordered = FALSE)

  output$generated_raster_map <- renderLeaflet({
    raster <- generated_raster()[[1]]
    values <- generated_raster_values()[!is.na(value)]
    validate(need(nrow(values) > 0, "The raster does not overlap the selected daily bird records."))
    display_range <- index_display_ranges[[generated_raster_label()]]
    if (is.null(display_range)) {
      display_range <- c(-0.5, 1)
    }

    display_raster <- raster
    if (terra::ncell(display_raster) > 600000) {
      factor <- ceiling(sqrt(terra::ncell(display_raster) / 600000))
      display_raster <- terra::aggregate(display_raster, fact = factor, fun = mean, na.rm = TRUE)
    }
    display_raster <- terra::clamp(
      display_raster,
      lower = display_range[1],
      upper = display_range[2],
      values = TRUE
    )

    raster_values <- terra::values(display_raster, na.rm = TRUE)
    palette <- colorNumeric("viridis", display_range, na.color = "transparent")
    raster_layer <- raster::raster(display_raster)
    map_points <- values[, .SD[1], by = .(track_id, date)]

    leaflet() %>%
      addTiles(
        urlTemplate = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = "&copy; OpenStreetMap contributors"
      ) %>%
      addRasterImage(raster_layer, colors = palette, opacity = 0.78, project = TRUE) %>%
      addCircleMarkers(
        data = map_points,
        lng = ~longitude,
        lat = ~latitude,
        radius = 3,
        stroke = TRUE,
        weight = 1,
        color = "#ffffff",
        fillColor = "#b4532a",
        fillOpacity = 0.8,
        popup = ~paste0(
          "Bird: ", track_id,
          "<br>Date: ", date,
          "<br>", generated_raster_label(), ": ", round(value, 3)
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = palette,
        values = display_range,
        title = generated_raster_label()
      ) %>%
      fitBounds(
        lng1 = min(map_points$longitude),
        lat1 = min(map_points$latitude),
        lng2 = max(map_points$longitude),
        lat2 = max(map_points$latitude)
      )
  })

  output$generated_raster_distribution <- renderPlotly({
    values <- generated_raster_values()[!is.na(value)]
    validate(need(nrow(values) > 0, "No valid raster values match the current selection."))

    plot <- ggplot(values, aes(x = value)) +
      geom_histogram(bins = 30, fill = "#607f84", color = "white") +
      labs(x = generated_raster_label(), y = "Daily bird records") +
      theme_minimal(base_size = 12)

    ggplotly(plot) %>% config(displaylogo = FALSE)
  })

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
