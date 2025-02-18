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

# load country boundaries once for the UI choices
countries_sf <- ne_countries(scale = "medium", returnclass = "sf")
# filter for europe and north/west africa countries
selected_countries_sf <- countries_sf %>% 
  filter(continent == "Europe" | (continent == "Africa" & subregion %in% c("Northern Africa", "Western Africa")))
country_names <- sort(selected_countries_sf$name)

ui <- fluidPage(
  titlePanel("Black-tailed Godwit Tracks"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      p("1. Select a country, or choose 'Custom' to draw your own AOI."),
      p("2. Use the year dropdown and date slider to select a date range."),
      p("3. Once an AOI is set, use the bird dropdown to select a bird."),
      p("4. A one-year migration (latitude over time) plot will be generated for the selected bird."),
      
      # pre-defined AOI selector: either a country or "Custom"
      selectInput("country_aoi", "Select Pre-defined AOI (Country) or Custom", 
                  choices = c("Custom" = "Custom", country_names),
                  selected = "Custom"),
      
      # dropdown for selecting 2-year window
      uiOutput("year_range"),
      # slider for selecting date range within the 2-year window
      uiOutput("date_slider"),
      
      uiOutput("file_info"),
      
      selectInput("basemap", "Choose Basemap", 
                  choices = c("CartoDB Positron" = "CartoDB.Positron",
                              "Esri WorldImagery" = "Esri.WorldImagery"),
                  selected = "CartoDB.Positron")
    ),
    
    mainPanel(
      leafletOutput("map"),
      verbatimTextOutput("summary"),
      # dynamic ui for bird selection (appears after AOI is set)
      uiOutput("bird_select"),
      plotOutput("lat_plot")
    )
  )
)

server <- function(input, output, session) {
  
  # reactive expression to load the allLocations data from file
  data <- reactive({
    req(file.exists("allLocations.rds"))
    readRDS("allLocations.rds")
  })
  
  # reactive value to store the aoi (as an sf polygon)
  # this will be set either by Custom drawing or by country selection
  rv <- reactiveValues(aoi = NULL)
  
  # update pre-defined aoi based on country selection
  observeEvent(input$country_aoi, {
    if (input$country_aoi != "Custom") {
      # set aoi to the selected country's boundary
      selected_country <- input$country_aoi
      aoi_poly <- countries_sf %>% filter(name == selected_country)
      rv$aoi <- aoi_poly
      # add the country boundary to the map (group "aoi")
      leafletProxy("map") %>% 
        clearGroup("aoi") %>%
        addPolygons(data = aoi_poly, group = "aoi", color = "green", weight = 2, fill = FALSE)
    } else {
      # if "Custom" is chosen, clear any pre-defined aoi from map and reset rv$aoi
      rv$aoi <- NULL
      leafletProxy("map") %>% clearGroup("aoi")
    }
  })
  
  # dynamic ui: drop down for selecting a 2-year window
  output$year_range <- renderUI({
    req(data())
    df <- data()
    years <- sort(unique(as.numeric(format(df$timestamp, "%Y"))))
    if(length(years) < 2) {
      return(h4("Not enough years available"))
    }
    # generate 2-year windows (each option is "previousYear-currentYear")
    choices <- sapply(years[-1], function(y) paste(y - 1, y, sep = "-"))
    selectInput("year_range", "Select Year", choices = choices, selected = tail(choices, 1))
  })
  
  # dynamic ui: date slider based on the selected 2-year window
  output$date_slider <- renderUI({
    req(data(), input$year_range)
    # parse the selected 2-year window (e.g., "2024-2025")
    yrs <- unlist(strsplit(input$year_range, "-"))
    start_year <- as.numeric(yrs[1])
    end_year <- as.numeric(yrs[2])
    df <- data() %>% filter(as.numeric(format(timestamp, "%Y")) %in% c(start_year, end_year))
    min_date <- as.Date(min(df$timestamp))
    max_date <- as.Date(max(df$timestamp))
    
    sliderInput("date_slider", "Select Date Range",
                min = min_date,
                max = max_date,
                value = c(min_date, max_date),
                timeFormat = "%Y-%m-%d")
  })
  
  # reactive expression to filter data by the selected 2-year window and date range
  filtered_data <- reactive({
    req(input$year_range, input$date_slider)
    yrs <- unlist(strsplit(input$year_range, "-"))
    start_year <- as.numeric(yrs[1])
    end_year <- as.numeric(yrs[2])
    df <- data() %>% 
      filter(as.numeric(format(timestamp, "%Y")) %in% c(start_year, end_year))
    
    # further filter by the date slider range
    df <- df %>% 
      filter(as.Date(timestamp) >= input$date_slider[1],
             as.Date(timestamp) <= input$date_slider[2])
    
    df
  })
  
  
  # render the leaflet map with drawing tools
  output$map <- renderLeaflet({
    req(filtered_data())
    df <- filtered_data()
    
    # convert to an sf object for spatial filtering
    df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
    
    # use isolate(input$basemap) so that changes to basemap do not re-render the map
    leaflet() %>%
      addProviderTiles(providers[[isolate(input$basemap)]], layerId = "basemap") %>%
      addGlPoints(
        data = df_sf,
        group = "locations",
        popup = TRUE,
        radius = 5
      ) %>%
      addDrawToolbar(
        targetGroup = "aoi",
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
        polylineOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE
      )
  })
  
  # observer to update the basemap without resetting zoom/center
  observeEvent(input$basemap, {
    leafletProxy("map") %>%
      removeTiles(layerId = "basemap") %>%
      addProviderTiles(providers[[input$basemap]], layerId = "basemap")
  })
  
  # update the aoi when a new polygon is drawn on the map (only if Custom is selected)
  observeEvent(input$map_draw_new_feature, {
    if (input$country_aoi == "Custom") {
      coords <- input$map_draw_new_feature$geometry$coordinates[[1]]
      coords <- matrix(unlist(coords), ncol = 2, byrow = TRUE)
      rv$aoi <- st_as_sf(st_sfc(st_polygon(list(coords)), crs = 4326))
    }
  })
  
  # calculate and display summary statistics for points within the aoi
  output$summary <- renderPrint({
    req(filtered_data())
    df <- filtered_data()
    
    if (!is.null(rv$aoi)) {
      # convert df to an sf object
      df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
      
      # filter points that fall within the aoi
      df_within_aoi <- df_sf[st_intersects(df_sf, rv$aoi, sparse = FALSE), ]
      
      # summary statistics
      num_individuals <- length(unique(df_within_aoi$trackId))
      num_locations <- nrow(df_within_aoi)
      
      cat("Number of individuals within aoi:", num_individuals, "\n")
      cat("Total number of locations within aoi:", num_locations, "\n")
    } else {
      cat("Draw polygon of aoi (or select a country) to show summary statistics")
    }
  })
  
  # dynamic ui: drop down for bird selection after aoi is set
  output$bird_select <- renderUI({
    req(filtered_data(), rv$aoi)
    df <- filtered_data()
    df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
    df_within_aoi <- df_sf[st_intersects(df_sf, rv$aoi, sparse = FALSE), ]
    df_aoi <- df_within_aoi %>% st_set_geometry(NULL)
    
    birds <- unique(df_aoi$trackId)
    
    if (length(birds) > 0) {
      selectInput("selected_bird", "Select Bird", choices = birds)
    } else {
      h4("No birds available in aoi")
    }
  })
  
  # create latitude vs. date plot for the selected bird over the past 12 months
  output$lat_plot <- renderPlot({
    req(input$selected_bird)
    req(filtered_data(), rv$aoi)
    df <- filtered_data()
    
    # convert to sf and filter for points in aoi
    df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
    df_within_aoi <- df_sf[st_intersects(df_sf, rv$aoi, sparse = FALSE), ]
    
    if (nrow(df_within_aoi) == 0) {
      plot.new()
      text(0.5, 0.5, "No locations within aoi", cex = 1.5)
      return()
    }
    
    # remove spatial geometry
    df_aoi <- df_within_aoi %>% st_set_geometry(NULL)
    
    # filter for the selected bird and determine its capture time (latest timestamp within aoi)
    df_bird_aoi <- df_aoi %>% filter(trackId == input$selected_bird)
    capture_time <- max(df_bird_aoi$timestamp)
    
    # for the selected bird, extract full data from the past 12 months (using the full dataset)
    df_all <- data() %>% filter(trackId == input$selected_bird)
    df_plot <- df_all %>%
      filter(timestamp >= (capture_time - years(1)) & timestamp <= capture_time) %>%
      mutate(date = as.Date(timestamp))
    
    if (nrow(df_plot) == 0) {
      plot.new()
      text(0.5, 0.5, "No data in the past 12 months for the selected bird", cex = 1.5)
    } else {
      ggplot(df_plot, aes(x = date, y = location_lat)) +
        geom_line() +
        geom_point() +
        labs(x = "", y = "Latitude") +
        theme_minimal()
    }
  })
  
  # observer: when a bird is selected, add a polyline connecting its locations on the map
  observeEvent(input$selected_bird, {
    req(rv$aoi)
    # use the filtered data to determine capture time from the aoi points
    df <- filtered_data()
    df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
    df_within_aoi <- df_sf[st_intersects(df_sf, rv$aoi, sparse = FALSE), ]
    df_aoi <- df_within_aoi %>% st_set_geometry(NULL)
    
    # filter for the selected bird and determine its capture time (latest timestamp in aoi)
    df_bird_aoi <- df_aoi %>% filter(trackId == input$selected_bird)
    if(nrow(df_bird_aoi) == 0){
      leafletProxy("map") %>% clearGroup("selectedBirdLine")
      return()
    }
    capture_time <- max(df_bird_aoi$timestamp)
    
    # get full data for the selected bird for the past 12 months (as used for the plot)
    df_all <- data() %>% filter(trackId == input$selected_bird)
    df_line <- df_all %>% 
      filter(timestamp >= (capture_time - years(1)) & timestamp <= capture_time) %>% 
      arrange(timestamp)
    
    leafletProxy("map") %>%
      clearGroup("selectedBirdLine") %>%
      addPolylines(data = df_line,
                   lng = ~location_long,
                   lat = ~location_lat,
                   group = "selectedBirdLine",
                   color = "red", weight = 3)
  })
}

shinyApp(ui, server)
