library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(dplyr)
library(leafgl)
library(terra)
library(lubridate)
library(ggplot2)
library(scales)

ui <- fluidPage(
  titlePanel("Habitat Use of Black-tailed Godwits in the Senegal Delta"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("year_select"),
      uiOutput("subseason_select"),  # sub-season toggle
      uiOutput("individual_select"), # individual filter
      
      selectInput("basemap", "Choose Basemap", 
                  choices = c("CartoDB Positron" = "CartoDB.Positron",
                              "Esri WorldImagery" = "Esri.WorldImagery"),
                  selected = "CartoDB.Positron"),
      
      uiOutput("file_info")
    ),
    
    mainPanel(
      leafletOutput("map"),
      plotOutput("bar_plot")
    )
  )
)

server <- function(input, output, session) {
  
  # load data once and reuse
  all_locations <- reactive({
    req(file.exists("locations_senegal_delta.rds"))
    readRDS("locations_senegal_delta.rds")
  })
  
  # load raster file (land cover)
  land_cover_raster <- rast("landCover.tif")
  # project raster for leaflet and sample down
  land_cover_raster <- projectRasterForLeaflet(land_cover_raster, method = 'ngb')
  land_cover_raster <- spatSample(land_cover_raster, 1000000, method = "regular", as.raster = TRUE)
  
  # define land cover values and labels
  land_cover_values <- c(2, 3, 4, 5, 6, 7, 8, 9)
  land_cover_labels <- c("rice (wet season)", "mixed crops", "cattail", "floodplain wetlands", 
                         "semi-arid grassland", "bare", "open water", "rice (dry season)")
  
  # define color palette for raster categories
  land_cover_palette <- colorFactor(
    palette = c("darkred", "orange", "chartreuse2", "darkgreen", 
                "#b7950b", "yellow2", "blue3", "purple"), 
    na.color = "#FF000000",
    domain = land_cover_values
  )
  
  # function to compute season for each timestamp (season starts in july)
  compute_season <- function(ts) {
    m <- month(ts)
    y <- year(ts)
    if (m >= 7) {
      paste0(y, "-", y + 1)
    } else {
      paste0(y - 1, "-", y)
    }
  }
  
  # year/season selection
  output$year_select <- renderUI({
    req(all_locations())
    df <- all_locations()
    df$season <- sapply(df$timestamp, compute_season)
    seasons <- unique(df$season)
    # order from most recent to oldest
    seasons <- seasons[order(as.numeric(sub("-.*", "", seasons)), decreasing = TRUE)]
    seasons <- c(seasons, "All Years")
    
    selectInput("year", "Select Year", 
                choices = seasons, 
                selected = seasons[1])
  })
  
  # wet/dry season input
  output$subseason_select <- renderUI({
    selectInput("subseason", "Select Season", 
                choices = c("All", "Wet Season (Jul-Nov)", "Dry Season (Dec-Jun)"), 
                selected = "All")
  })
  
  # apply filters
  filtered_data <- reactive({
    req(all_locations(), input$year, input$subseason)
    df <- all_locations()
    
    # compute main season
    df$season <- sapply(df$timestamp, compute_season)
    
    # filter by chosen year
    if (input$year != "All Years") {
      df <- df %>% filter(season == input$year)
    }
    
    # filter by sub-season
    if (input$subseason != "All") {
      if (input$subseason == "Wet Season (Jul-Nov)") {
        # jul to nov
        df <- df %>% filter(month(timestamp) >= 7 & month(timestamp) <= 11)
      } else if (input$subseason == "Dry Season (Dec-Jun)") {
        # dec to jun
        # dec means month == 12 in the same "start" year
        # jan-jun means month <= 6 in the following year
        # for 'All Years' or a single season, keep months <= 6 or >= 12
        df <- df %>% filter(month(timestamp) >= 12 | month(timestamp) <= 6)
      }
    }
    
    df
  })
  
  # dynamically select only track IDs available in the chosen filters
  output$individual_select <- renderUI({
    df <- filtered_data()
    
    # if no data, provide a minimal dropdown
    if (nrow(df) == 0) {
      return(selectInput("individual", "Select Individual",
                         choices = "All", selected = "All"))
    }
    
    # otherwise, collect actual IDs
    ids <- sort(unique(as.character(df$trackId)))
    selectInput("individual", "Select Individual", 
                choices = c("All", ids),
                selected = "All")
  })
  
  # output file info
  output$file_info <- renderUI({
    df <- filtered_data()
    
    # if an individual is chosen, filter further
    if (!is.null(input$individual) && input$individual != "All") {
      df <- df %>% filter(as.character(trackId) == input$individual)
    }
    
    tagList(
      p(paste("number of locations:", nrow(df))),
      p(paste("number of individuals:", length(unique(df$trackId))))
    )
  })
  
  # render leaflet map
  output$map <- renderLeaflet({
    df <- filtered_data()
    
    # if an individual is chosen, filter further
    if (!is.null(input$individual) && input$individual != "All") {
      df <- df %>% filter(as.character(trackId) == input$individual)
    }
    
    if (nrow(df) == 0) {
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPopups(lng = -10, lat = 14, 
                  popup = "no data available for this timeframe")
    } else {
      df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
      
      # base map
      m <- leaflet() %>%
        addProviderTiles(providers[[input$basemap]]) %>%
        addGlPoints(
          data = df_sf,
          group = "locations",
          popup = TRUE,
          radius = 5,
          fillColor = 'cyan'
        ) %>%
        addRasterImage(land_cover_raster,
                       colors = land_cover_palette,
                       opacity = 1,
                       group = "land cover",
                       project = FALSE) %>%
        addLayersControl(
          overlayGroups = c("land cover"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addLegend(position = "bottomright",
                  pal = land_cover_palette,
                  values = land_cover_values,
                  labFormat = function(type, cuts, p) { land_cover_labels },
                  title = "land cover",
                  opacity = 1)
      
      # if a single individual is chosen, add a connecting line
      if (!is.null(input$individual) && input$individual != "All" && nrow(df) > 1) {
        df_sf <- df_sf %>% arrange(timestamp)
        coords <- st_coordinates(df_sf)
        line_geom <- st_linestring(as.matrix(coords[, c("X", "Y")]))
        line_sf <- st_sfc(line_geom, crs = 4326)
        line_sf <- st_sf(trackId = input$individual, geometry = line_sf)
        
        m <- m %>% 
          addPolylines(data = line_sf, color = "cyan", weight = 4, group = "track_line")
      }
      
      m
    }
  })
  
  # bar plot of land cover usage
  output$bar_plot <- renderPlot({
    df <- filtered_data()
    
    # if an individual is chosen, filter further
    if (!is.null(input$individual) && input$individual != "All") {
      df <- df %>% filter(as.character(trackId) == input$individual)
    }
    
    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "no data available for bar plot")
    } else {
      df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
      df_vect <- vect(df_sf)
      lc_extract <- terra::extract(land_cover_raster, df_vect)
      df$land_cover <- lc_extract[, 2]
      df <- df %>% filter(land_cover %in% land_cover_values)
      
      summary_df <- df %>%
        group_by(land_cover) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(percent = 100 * count / sum(count))
      
      summary_df <- summary_df %>% 
        mutate(land_cover_label = factor(
          ifelse(land_cover %in% land_cover_values, 
                 land_cover_labels[match(land_cover, land_cover_values)], 
                 as.character(land_cover)),
          levels = land_cover_labels)) %>%
        arrange(desc(percent))
      
      land_cover_colors <- c("darkred", "orange", "chartreuse2", "darkgreen", 
                             "#b7950b", "yellow2", "blue3", "purple")
      names(land_cover_colors) <- land_cover_labels
      
      p <- ggplot(summary_df, 
                  aes(x = reorder(land_cover_label, -percent), 
                      y = percent, 
                      fill = land_cover_label)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        labs(x = "land cover", y = "percent of total locations") +
        scale_y_continuous(labels = function(x) paste0(x, "%")) +
        scale_fill_manual(values = land_cover_colors) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      print(p)
    }
  })
}

shinyApp(ui, server)
