library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(leafgl)
library(terra)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Doñana Wetland Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      p("Select a year and month to view godwit tracks and flood imagery for selected date."),
      
      uiOutput("year_select"),
      uiOutput("month_select"),
      uiOutput("file_info"),
      
      numericInput("n_available", "Number of available points per used point", 
                   value = 10, min = 1, max = 50),
      numericInput("buffer_radius", "Buffer radius for availability (meters)", 
                   value = 500, min = 100, max = 5000, step = 100),
      
      
      selectInput("basemap", "Choose Basemap", 
                  choices = c("CartoDB Positron" = "CartoDB.Positron",
                              "Esri WorldImagery" = "Esri.WorldImagery"),
                  selected = "CartoDB.Positron")
    ),
    
    mainPanel(
      leafletOutput("map"),
      plotOutput("flood_barplot")
    )
  )
)

server <- function(input, output, session) {
  
  # load location data
  all_locations <- reactive({
    req(file.exists("locations_donana.rds"))
    readRDS("locations_donana.rds")
  })
  
  # update the year selection input with default of 2025 if available
  output$year_select <- renderUI({
    req(all_locations())
    years <- unique(format(all_locations()$timestamp, "%Y"))
    years <- sort(years, decreasing = TRUE)
    years <- c(years, "All years")
    # set default year to 2025 if available, otherwise use most recent
    default_year <- if ("2025" %in% years) "2025" else years[1]
    selectInput("year", "Select year", choices = years, selected = default_year)
  })
  
  # update the month selection input with default of January for a specific year
  output$month_select <- renderUI({
    req(all_locations(), input$year)
    month_choices <- c("January", "February", "March", "April", "May", "June",
                       "July", "August", "September", "October", "November", "December", "All Months")
    # if a specific year is selected (not All Years), default to January, else All Months
    default_month <- if (input$year != "All Years") "January" else "All Months"
    selectInput("month", "Select month", choices = month_choices, selected = default_month)
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
    file_path <- paste0("SWIR/SWIR1_Composite_", month_abbrev, input$year, ".tif")
    if (!file.exists(file_path)) return(NULL)
    
    r <- rast(file_path)
    r <- projectRasterForLeaflet(r, method = 'bilinear')
    r <- spatSample(r, 100000, method = "regular", as.raster = TRUE)
    
    # read and mask to donana geometry if available
    donana_shp <- "Donana_flooding/donana_geometry/Donana.shp"
    if (file.exists(donana_shp)) {
      donana_geom <- st_read(donana_shp, quiet = TRUE)
      # reproject donana geometry to match the raster's crs
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
      p("*note: maximum of 6 points/day per bird are displayed")
    )
  })
  
  flooded_fix_summary <- reactive({
    req(filtered_data(), selected_raster(), input$n_available, input$buffer_radius)
    df <- filtered_data()
    r <- selected_raster()
    
    used_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
    used_sf$type <- "used"
    
    set.seed(42)
    available_points <- do.call(rbind, lapply(1:nrow(used_sf), function(i) {
      pt <- st_geometry(used_sf[i, ])
      buff <- st_buffer(pt, dist = input$buffer_radius)
      rand_pts <- st_sample(buff, size = input$n_available)
      st_sf(
        trackId = rep(used_sf$trackId[i], length(rand_pts)),
        timestamp = rep(used_sf$timestamp[i], length(rand_pts)),
        geometry = rand_pts,
        type = "available"
      )
    }))
    
    combined_sf <- rbind(
      used_sf[, c("trackId", "timestamp", "geometry", "type")],
      available_points
    )
    
    combined_vect <- vect(st_transform(combined_sf, crs(r)))
    pixel_vals <- terra::extract(r, combined_vect)
    combined_sf$pixel_val <- pixel_vals[,2]
    combined_sf$flooded <- ifelse(combined_sf$pixel_val < 0.186, "flooded", "non-flooded")
    
    st_drop_geometry(combined_sf)
  })
  
  
  
  output$flood_barplot <- renderPlot({
    df <- flooded_fix_summary()
    
    df$type <- factor(df$type, levels = c("used", "available"))  # force used to come first
    
    ggplot(df, aes(x = type, y = pixel_val, fill = type)) +
      geom_boxplot(width = 0.6, outlier.alpha = 0.3) +
      geom_hline(yintercept = 0.186, linetype = "dashed", color = "blue", linewidth = 1) +
      annotate("text", x = 1.5, y = 0.186, label = "↑ non-flooded terrain ↑", vjust = -1, color = "blue", size = 4.5) +
      annotate("text", x = 1.5, y = 0.186, label = "↓ flooded terrain ↓", vjust = 2.2, color = "blue", size = 4.5) +
      labs(
        x = NULL,
        y = "SWIR Reflectance (Band 5)") +
      theme_minimal() +
      theme(
        text = element_text(size = 16),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "none"
      )
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
