library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(leafgl)
library(terra)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Water Availability in the Doñana Wetlands"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      p("Select a year and month to view godwit tracks and flood imagery for selected date."),
      
      uiOutput("year_select"),
      uiOutput("month_select"),
      uiOutput("file_info"),
      
      selectInput("basemap", "Choose Basemap", 
                  choices = c("CartoDB Positron" = "CartoDB.Positron",
                              "Esri WorldImagery" = "Esri.WorldImagery"),
                  selected = "CartoDB.Positron")
    ),
    
    mainPanel(
      leafletOutput("map"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output, session) {
  
  # load location data
  all_locations <- reactive({
    req(file.exists("Donana_flooding/locations_donana.rds"))
    readRDS("Donana_flooding/locations_donana.rds")
  })
  
  # update the year selection input based on the data
  output$year_select <- renderUI({
    req(all_locations())
    years <- unique(format(all_locations()$timestamp, "%Y"))
    years <- sort(years, decreasing = TRUE)
    years <- c(years, "All Years")
    selectInput("year", "Select Year", choices = years, selected = years[1])
  })
  
  # update the month selection input
  output$month_select <- renderUI({
    req(all_locations(), input$year)
    month_choices <- c("January", "February", "March", "April", "May", "June",
                       "July", "August", "September", "October", "November", "December", "All Months")
    if (input$year != "All Years") {
      df_year <- all_locations() %>% filter(format(timestamp, "%Y") == input$year)
      recent_month <- if(nrow(df_year) > 0) format(max(df_year$timestamp), "%B") else "All Months"
    } else {
      recent_month <- "All Months"
    }
    selectInput("month", "Select Month", choices = month_choices, selected = recent_month)
  })
  
  # reactive expression to filter data by selected year and month
  filtered_data <- reactive({
    req(input$year, input$month, all_locations())
    df <- all_locations()
    if (input$year != "All Years") df <- df %>% filter(format(timestamp, "%Y") == input$year)
    if (input$month != "All Months") df <- df %>% filter(format(timestamp, "%B") == input$month)
    df
  })
  
  # reactive expression to load and mask the raster for the selected year and month
  selected_raster <- reactive({
    req(input$year, input$month)
    if (input$year == "All Years" || input$month == "All Months") return(NULL)
    
    # get the three-letter uppercase month abbreviation (e.g., "JAN")
    month_abbrev <- toupper(substr(input$month, 1, 3))
    file_path <- paste0("Donana_flooding/SWIR/SWIR1_Composite_", month_abbrev, input$year, ".tif")
    if (!file.exists(file_path)) return(NULL)
    
    r <- rast(file_path)
    r <- projectRasterForLeaflet(r, method = 'bilinear')
    r <- spatSample(r, 100000, method = "regular", as.raster = TRUE)
    
    # read and mask to Donana geometry if available
    donana_shp <- "Donana_flooding/donana_geometry/Donana.shp"
    if (file.exists(donana_shp)) {
      donana_geom <- st_read(donana_shp, quiet = TRUE)
      # reproject Donana geometry to match the raster's CRS
      donana_geom <- st_transform(donana_geom, crs = crs(r))
      donana_vect <- vect(donana_geom)
      r <- mask(r, donana_vect)
    }
    r
  })
  
  # output file info
  output$file_info <- renderUI({
    req(filtered_data())
    df <- filtered_data()
    tagList(
      h4("tracks for selected year and month"),
      p(paste("number of locations:", nrow(df))),
      p(paste("number of individuals:", length(unique(df$trackId)))),
      p("*note: maximum of 5 points/day per bird are displayed")
    )
  })
  
  # render a text summary comparing flooded and non-flooded locations
  output$summary <- renderPrint({
    req(filtered_data())
    df <- filtered_data()
    r <- selected_raster()
    if (is.null(r)) {
      cat("no raster available for this selection.\n")
    } else {
      df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
      v <- terra::vect(df_sf)
      pixel_values <- terra::extract(r, v)
      # assume the raster pixel values are in the second column
      df_sf$pixel_val <- pixel_values[,2]
      df_sf$flooded <- ifelse(df_sf$pixel_val < 0.128, "flooded", "non-flooded")
      
      flooded_count <- sum(df_sf$flooded == "flooded", na.rm = TRUE)
      non_flooded_count <- sum(df_sf$flooded == "non-flooded", na.rm = TRUE)
      
      cat("locations in flooded pixels:", flooded_count, "\n")
      cat("locations in non-flooded pixels:", non_flooded_count, "\n")
    }
  })
  
  # render the leaflet map with points and (if available) the masked raster
  output$map <- renderLeaflet({
    req(filtered_data())
    df <- filtered_data()
    if (nrow(df) == 0) {
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPopups(lng = -10, lat = 14, popup = "no data available for this selection")
    } else {
      df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
      leaflet() %>%
        addProviderTiles(providers[[input$basemap]]) %>%
        addGlPoints(
          data = df_sf,  
          group = "locations",
          popup = TRUE,
          radius = 5,
          fillColor = 'red'
        ) %>%
        { 
          r <- selected_raster()
          if (!is.null(r)) {
            pal <- colorNumeric(palette = rev(RColorBrewer::brewer.pal(9, "Blues")), 
                                domain = values(r), na.color = "transparent")
            addRasterImage(., r, colors = pal, opacity = 1, project = FALSE)
          } else {
            .
          }
        }
    }
  })
}

shinyApp(ui, server)
