library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(leafgl)
library(terra)

ui <- fluidPage(
  titlePanel("Grassland Production Intensity (GPI)"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      p("1. Filter location and GPI data by year and date"),
      p("2. Use the drawing tools on the map to draw an area of interest (aoi)."),
      p("3. The number of individuals, number of locations, and histogram of GPI values used by birds within the aoi will be shown."),
      
      uiOutput("year_select"),
      uiOutput("date_slider"),  # added for the date slider
      uiOutput("file_info"),
      
      selectInput("basemap", "choose basemap", 
                  choices = c("CartoDB Positron" = "CartoDB.Positron",
                              "Esri WorldImagery" = "Esri.WorldImagery"),
                  selected = "CartoDB.Positron")
    ),
    
    mainPanel(
      leafletOutput("map"),
      verbatimTextOutput("summary"),
      plotOutput("gpi_hist", height = "300px")  # added histogram output
    )
  )
)

server <- function(input, output, session) {
  
  # load data once and reuse, filtering for 2021 and newer
  all_locations <- reactive({
    req(file.exists("GPI/locations_swf.rds"))
    data <- readRDS("GPI/locations_swf.rds")
  })
  
  # reactive value to store the drawn aoi
  rv <- reactiveValues(aoi = NULL)
  
  # load all raster files for different years
  gpi_rasters <- reactive({
    # load rasters for 2021, 2022, 2023, and 2024
    raster_files <- list(
      gpi_2021 = rast("GPI/gpi_data/gpi_2021.tif"),
      gpi_2022 = rast("GPI/gpi_data/gpi_2022.tif"),
      gpi_2023 = rast("GPI/gpi_data/gpi_2023.tif"),
      gpi_2024 = rast("GPI/gpi_data/gpi_2024.tif")
    )
    
    # project and sample the rasters for leaflet display
    lapply(raster_files, function(r) {
      r <- projectRasterForLeaflet(r, method = 'bilinear')
      r <- spatSample(r, 100000, method = "regular", as.raster = TRUE)
      return(r)
    })
  })
  
  # update the year selection input based on the uploaded data
  output$year_select <- renderUI({
    req(all_locations())
    years <- unique(format(all_locations()$timestamp, "%Y"))
    years <- sort(years, decreasing = TRUE)
    years <- c(years, "All Years")
    selectInput("year", "Select Year", choices = years, selected = years[1])
  })
  
  # update the date slider based on the year selection
  output$date_slider <- renderUI({
    req(all_locations())
    df <- all_locations()
    if (input$year != "All Years") {
      df <- df %>% filter(format(timestamp, "%Y") == input$year)
    }
    
    min_date <- min(df$timestamp)
    max_date <- max(df$timestamp)
    
    sliderInput("date_range", "Select Date Range", 
                min = min_date, max = max_date,
                value = c(min_date, max_date), 
                timeFormat = "%Y-%m-%d", step = 1)
  })
  
  # reactive expression to filter data by the selected year and date range
  filtered_data <- reactive({
    req(input$year, input$date_range, all_locations())
    df <- all_locations()
    
    if (input$year != "All Years") {
      df <- df %>% filter(format(timestamp, "%Y") == input$year)
    }
    
    df <- df %>% filter(timestamp >= as.POSIXct(input$date_range[1]) & 
                          timestamp <= as.POSIXct(input$date_range[2]))
    df
  })
  
  # render the leaflet map
  output$map <- renderLeaflet({
    req(filtered_data())
    df <- filtered_data()
    
    # check if there is any data before trying to create a leaflet map
    if (nrow(df) == 0) {
      leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
        addPopups(lng = -10, lat = 14, popup = "no data available for this timeframe")
    } else {
      df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
      
      # get the selected year and load the corresponding raster
      # if "all years" is selected, use the most recent year's raster by default
      if (input$year == "All Years") {
        selected_raster <- gpi_rasters()[["gpi_2024"]]
      } else {
        selected_raster <- gpi_rasters()[[paste0("gpi_", input$year)]]
      }
      
      # define the color palette for the raster values
      pal <- colorNumeric(palette = "YlGn", domain = values(selected_raster), na.color = "transparent")
      
      leaflet() %>%
        addProviderTiles(providers[[input$basemap]]) %>%
        addGlPoints(
          data = df_sf,  
          group = "locations",
          popup = TRUE,
          radius = 5,
          fillColor = 'cyan'
        ) %>%
        addDrawToolbar(
          targetGroup = "aoi",
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
          polylineOptions = FALSE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE
        ) %>%
        addRasterImage(selected_raster, 
                       project = FALSE, 
                       colors = pal) %>%
        # add legend with two bins and custom labels low and high
        addLegend(
          position = "bottomright",
          pal = pal,
          values = values(selected_raster),  # use the raster values as the domain
          title = "GPI",
          opacity = 0.7,
          bins = 2,  # force two bins for low and high
          labFormat = function(type, cuts, p) {
            c("low", "high")
          }
        )
    }
  })
  
  # update the aoi and recalculate the summary when a new polygon is drawn
  observeEvent(input$map_draw_new_feature, {
    coords <- input$map_draw_new_feature$geometry$coordinates[[1]]
    coords <- matrix(unlist(coords), ncol = 2, byrow = TRUE)
    rv$aoi <- st_as_sf(st_sfc(st_polygon(list(coords)), crs = 4326))
  })
  
  # calculate and display the summary 
  output$summary <- renderPrint({
    req(filtered_data())
    df <- filtered_data()
    # if aoi is drawn, restrict points to those within the aoi
    if (!is.null(rv$aoi)) {
      df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
      df <- df_sf[st_intersects(df_sf, rv$aoi, sparse = FALSE), ]
    }
    if (nrow(df) == 0) {
      cat("no tracks available for the selected time window.\n")
    } else {
      num_individuals <- length(unique(df$trackId))
      num_locations <- nrow(df)
      
      cat("number of individuals within aoi:", num_individuals, "\n")
      cat("total number of locations within aoi:", num_locations, "\n")
    }
  })
  
  # render the histogram of gpi values for points that intersect the locations
  output$gpi_hist <- renderPlot({
    req(filtered_data())
    df <- filtered_data()
    df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
    
    # if aoi is drawn, filter points to those within the aoi
    if (!is.null(rv$aoi)) {
      df_sf <- df_sf[st_intersects(df_sf, rv$aoi, sparse = FALSE), ]
    }
    
    # if no points are available, display a message
    if (nrow(df_sf) == 0) {
      plot.new()
      text(0.5, 0.5, "no points in selected aoi or timeframe", cex = 1.5)
      return()
    }
    
    # select the appropriate raster based on the year
    if (input$year == "All Years") {
      selected_raster <- gpi_rasters()[["gpi_2024"]]
    } else {
      selected_raster <- gpi_rasters()[[paste0("gpi_", input$year)]]
    }
    
    # transform points to the raster's crs to ensure proper extraction
    df_sf <- st_transform(df_sf, crs(selected_raster))
    
    # extract gpi values from the raster at the point locations
    coords <- st_coordinates(df_sf)
    ext_vals <- terra::extract(selected_raster, coords)
    # get the raster layer name
    raster_name <- names(selected_raster)[1]
    gpi_values <- ext_vals[[raster_name]]
    
    # remove missing values before plotting
    gpi_values <- gpi_values[!is.na(gpi_values)]
    
    # if no valid gpi values remain, display a message
    if (length(gpi_values) == 0) {
      plot.new()
      text(0.5, 0.5, "no gpi values available", cex = 1.5)
      return()
    }
    
    # scale the gpi values to range from 0 to 1
    min_val <- min(gpi_values)
    max_val <- max(gpi_values)
    # avoid division by zero if all values are identical
    if (max_val - min_val != 0) {
      gpi_values_scaled <- (gpi_values - min_val) / (max_val - min_val)
    } else {
      gpi_values_scaled <- rep(0, length(gpi_values))
    }
    
    # plot histogram of scaled gpi values with default numeric axis
    hist(gpi_values_scaled,main = "",
         xlab = "Grassland Production Intensity", ylab = "Godwit Locations", col = "lightblue", border = "white")
    
    # add additional text labels below 0 and 1 without replacing the default tick labels
    mtext("low", side = 1, at = 0, line = 3, cex = 0.8)
    mtext("high", side = 1, at = 1, line = 3, cex = 0.8)
  })
}

shinyApp(ui, server)
