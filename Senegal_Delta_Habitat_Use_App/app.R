library(shiny)
library(leaflet)
library(leaflet.extras2)
library(sf)
library(dplyr)
library(leafgl)
library(terra)
library(lubridate)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Senegal Delta Habitat Use"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      p("1. Use the dropdowns to select a migration year, season, and optionally an individual bird."),
      p("2. The first barplot shows the distribution of used locations across land cover classes."),
      p("3. The second plot shows selection ratios (use relative to availability). Values > 1 indicate selection, values < 1 indicate avoidance."),
      br(),
      
      uiOutput("year_select"),
      uiOutput("subseason_select"),  
      uiOutput("individual_select"), 
      
      selectInput("basemap", "Choose basemap", 
                  choices = c("CartoDB Positron" = "CartoDB.Positron",
                              "Esri WorldImagery" = "Esri.WorldImagery"),
                  selected = "CartoDB.Positron"),
      
      uiOutput("file_info")
    ),
    
    mainPanel(
      leafletOutput("map"),
      plotOutput("bar_plot"),
      plotOutput("selection_ratio_plot")
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
  land_cover_raster <- projectRasterForLeaflet(land_cover_raster, method = 'ngb')
  land_cover_raster <- spatSample(land_cover_raster, 1000000, method = "regular", as.raster = TRUE)
  
  # define land cover values and labels
  land_cover_values <- c(2, 3, 4, 5, 6, 7, 8, 9)
  land_cover_labels <- c("rice (wet season)", "mixed crops", "cattail", "floodplain wetlands", 
                         "semi-arid grassland", "bare", "open water", "rice (dry season)")
  
  land_cover_palette <- colorFactor(
    palette = c("darkred", "orange", "chartreuse2", "darkgreen", 
                "#b7950b", "yellow2", "blue3", "purple"), 
    na.color = "#FF000000",
    domain = land_cover_values
  )
  
  # function to compute season for each timestamp (season starts in july)
  compute_season <- function(ts) {
    m <- month(ts); y <- year(ts)
    if (m >= 7) paste0(y, "-", y + 1) else paste0(y - 1, "-", y)
  }
  
  # year/season selection
  output$year_select <- renderUI({
    req(all_locations())
    df <- all_locations()
    df$season <- sapply(df$timestamp, compute_season)
    seasons <- unique(df$season)
    seasons <- seasons[order(as.numeric(sub("-.*", "", seasons)), decreasing = TRUE)]
    seasons <- c(seasons, "All years")
    selectInput("year", "Select year", choices = seasons, selected = seasons[1])
  })
  
  # wet/dry season input
  output$subseason_select <- renderUI({
    selectInput("subseason", "Select season", 
                choices = c("All", "Wet season (Jul-Nov)", "Dry season (Dec-Jun)"), 
                selected = "All")
  })
  
  # apply filters
  filtered_data <- reactive({
    req(all_locations(), input$year, input$subseason)
    df <- all_locations()
    df$season <- sapply(df$timestamp, compute_season)
    
    if (input$year != "All Years") {
      df <- df %>% filter(season == input$year)
    }
    if (input$subseason != "All") {
      if (input$subseason == "Wet Season (Jul-Nov)") {
        df <- df %>% filter(month(timestamp) >= 7 & month(timestamp) <= 11)
      } else if (input$subseason == "Dry Season (Dec-Jun)") {
        df <- df %>% filter(month(timestamp) >= 12 | month(timestamp) <= 6)
      }
    }
    df
  })
  
  # individual filter
  output$individual_select <- renderUI({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(selectInput("individual", "Select Individual", choices = "All", selected = "All"))
    }
    ids <- sort(unique(as.character(df$trackId)))
    selectInput("individual", "Select Individual", choices = c("All", ids), selected = "All")
  })
  
  # file info
  output$file_info <- renderUI({
    df <- filtered_data()
    if (!is.null(input$individual) && input$individual != "All") {
      df <- df %>% filter(as.character(trackId) == input$individual)
    }
    tagList(
      p(paste("Number of locations:", nrow(df))),
      p(paste("Number of individuals:", length(unique(df$trackId))))
    )
  })
  
  # map
  output$map <- renderLeaflet({
    df <- filtered_data()
    if (!is.null(input$individual) && input$individual != "All") {
      df <- df %>% filter(as.character(trackId) == input$individual)
    }
    if (nrow(df) == 0) {
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPopups(lng = -10, lat = 14, popup = "no data available for this timeframe")
    } else {
      df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
      m <- leaflet() %>%
        addProviderTiles(providers[[input$basemap]]) %>%
        addGlPoints(data = df_sf, group = "locations", popup = TRUE,
                    radius = 5, fillColor = 'cyan') %>%
        addRasterImage(land_cover_raster, colors = land_cover_palette,
                       opacity = 1, group = "land cover", project = FALSE) %>%
        addLayersControl(overlayGroups = c("land cover"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addLegend(position = "bottomright", pal = land_cover_palette,
                  values = land_cover_values, labFormat = function(type, cuts, p) land_cover_labels,
                  title = "land cover", opacity = 1)
      if (!is.null(input$individual) && input$individual != "All" && nrow(df) > 1) {
        df_sf <- df_sf %>% arrange(timestamp)
        coords <- st_coordinates(df_sf)
        line_geom <- st_linestring(as.matrix(coords[, c("X", "Y")]))
        line_sf <- st_sf(trackId = input$individual, geometry = st_sfc(line_geom, crs = 4326))
        m <- m %>% addPolylines(data = line_sf, color = "cyan", opacity = 1, weight = 4, group = "track_line")
      }
      m
    }
  })
  
  # bar plot of used land cover
  output$bar_plot <- renderPlot({
    df <- filtered_data()
    if (!is.null(input$individual) && input$individual != "All") {
      df <- df %>% filter(as.character(trackId) == input$individual)
    }
    if (nrow(df) == 0) {
      plot.new(); text(0.5, 0.5, "no data available for bar plot"); return()
    }
    df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
    df$land_cover <- terra::extract(land_cover_raster, vect(st_transform(df_sf, crs(land_cover_raster))))[, 2]
    df <- df %>% filter(land_cover %in% land_cover_values)
    summary_df <- df %>% group_by(land_cover) %>% summarise(count = n(), .groups = "drop") %>%
      mutate(percent = 100 * count / sum(count),
             land_cover_label = factor(land_cover_labels[match(land_cover, land_cover_values)],
                                       levels = land_cover_labels))
    land_cover_colors <- c("darkred", "orange", "chartreuse2", "darkgreen", 
                           "#b7950b", "yellow2", "blue3", "purple")
    names(land_cover_colors) <- land_cover_labels
    ggplot(summary_df, aes(x = reorder(land_cover_label, -percent), y = percent, fill = land_cover_label)) +
      geom_col(show.legend = FALSE) +
      labs(y = "percent of used locations", x = NULL) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      scale_fill_manual(values = land_cover_colors) +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
            axis.title.y = element_text(size = 14))
    
  })
  
  # selection ratio plot (use vs availability)
  output$selection_ratio_plot <- renderPlot({
    df <- filtered_data()
    if (!is.null(input$individual) && input$individual != "All") {
      df <- df %>% filter(as.character(trackId) == input$individual)
    }
    if (nrow(df) == 0) {
      plot.new(); text(0.5, 0.5, "no data available for selection ratio"); return()
    }
    
    # used points
    df_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326)
    used_vals <- terra::extract(land_cover_raster, vect(st_transform(df_sf, crs(land_cover_raster))))[, 2]
    used_vals <- used_vals[!is.na(used_vals)]
    
    # available points
    set.seed(42)
    avail_pts <- spatSample(land_cover_raster, size = length(used_vals) * 10,
                            method = "random", as.points = TRUE, na.rm = TRUE)
    avail_vals <- terra::extract(land_cover_raster, avail_pts)[, 2]
    avail_vals <- avail_vals[!is.na(avail_vals)]
    
    # proportions
    used_tab <- prop.table(table(factor(used_vals, levels = land_cover_values)))
    avail_tab <- prop.table(table(factor(avail_vals, levels = land_cover_values)))
    
    df_ratio <- data.frame(
      land_cover = land_cover_values,
      land_cover_label = factor(land_cover_labels, levels = land_cover_labels),
      ratio = as.numeric(used_tab) / as.numeric(avail_tab)
    )
    
    land_cover_colors <- c("darkred", "orange", "chartreuse2", "darkgreen", 
                           "#b7950b", "yellow2", "blue3", "purple")
    names(land_cover_colors) <- land_cover_labels
    
    ggplot(df_ratio, aes(x = reorder(land_cover_label, -ratio), y = ratio, fill = land_cover_label)) +
      geom_col(show.legend = FALSE) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
      scale_fill_manual(values = land_cover_colors) +
      labs(x = NULL, y = "Selection ratio (use / availability)") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))
    
  })
}

shinyApp(ui, server)
