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
  titlePanel("Doñana Wetland Seasonal Flooding Viewer"),
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      p("Select a year and month to view godwit tracks and flood imagery for the chosen date."),
      
      uiOutput("year_select"),
      uiOutput("month_select"),
      uiOutput("file_info"),
      
      selectInput("basemap", "Choose Basemap", 
                  choices = c("CartoDB Positron" = "CartoDB.Positron",
                              "Esri WorldImagery" = "Esri.WorldImagery"),
                  selected = "CartoDB.Positron"),
      
      p("The boxplot shows flooding conditions at the locations used by the birds (‘used’) compared to random locations (‘available’).")
    ),
    mainPanel(
      leafletOutput("map"),
      plotOutput("flood_barplot")
    )
  )
)

server <- function(input, output, session) {
  
  all_locations <- reactive({
    req(file.exists("locations_donana.rds"))
    readRDS("locations_donana.rds")
  })
  
  output$year_select <- renderUI({
    req(all_locations())
    years <- sort(unique(format(all_locations()$timestamp, "%Y")), decreasing = TRUE)
    years <- c(years, "All years")
    default_year <- if ("2025" %in% years) "2025" else years[1]
    selectInput("year", "Select year", choices = years, selected = default_year)
  })
  
  output$month_select <- renderUI({
    req(all_locations(), input$year)
    month_choices <- c(month.name, "All Months")
    default_month <- if (input$year != "All Years") "January" else "All Months"
    selectInput("month", "Select month", choices = month_choices, selected = default_month)
  })
  
  filtered_data <- reactive({
    req(input$year, input$month, all_locations())
    df <- all_locations()
    if (input$year != "All Years") df <- df %>% filter(format(timestamp, "%Y") == input$year)
    if (input$month != "All Months") df <- df %>% filter(format(timestamp, "%B") == input$month)
    df
  })
  
  selected_raster <- reactive({
    req(input$year, input$month)
    if (input$year == "All Years" || input$month == "All Months") return(NULL)
    
    month_abbrev <- toupper(substr(input$month, 1, 3))
    file_path <- paste0("SWIR/SWIR1_Composite_", month_abbrev, input$year, ".tif")
    if (!file.exists(file_path)) return(NULL)
    
    r <- rast(file_path)
    r <- projectRasterForLeaflet(r, method = "bilinear")
    r <- spatSample(r, 100000, method = "regular", as.raster = TRUE)
    
    donana_shp <- "Donana_flooding/donana_geometry/Donana.shp"
    if (file.exists(donana_shp)) {
      donana_geom <- st_read(donana_shp, quiet = TRUE) |> st_transform(crs(r))
      r <- mask(r, vect(donana_geom))
    }
    r
  })
  
  output$file_info <- renderUI({
    req(filtered_data())
    df <- filtered_data()
    tagList(
      p(paste("GPS points:", nrow(df))),
      p(paste("Individuals:", length(unique(df$trackId)))),
    )
  })
  
  flooded_fix_summary <- reactive({
    req(filtered_data(), selected_raster())
    df <- filtered_data()
    r  <- selected_raster()
    
    used_sf <- st_as_sf(df, coords = c("location_long", "location_lat"), crs = 4326) |>
      st_transform(crs(r)) |>
      mutate(type = "used") |>
      select(trackId, timestamp, type, geometry)
    
    set.seed(42)
    avail_pts <- spatSample(r,
                            size = nrow(used_sf) * 10,
                            method = "random",
                            as.points = TRUE,
                            na.rm = TRUE)
    
    available_points <- st_as_sf(avail_pts) |>
      mutate(trackId   = rep(used_sf$trackId, each = 10),
             timestamp = rep(used_sf$timestamp, each = 10),
             type      = "available") |>
      select(trackId, timestamp, type, geometry)
    
    combined_sf <- rbind(used_sf, available_points)
    
    pixel_vals <- terra::extract(r, vect(combined_sf))
    combined_sf$pixel_val <- pixel_vals[, 2]
    combined_sf$flooded <- ifelse(combined_sf$pixel_val < 0.186, "flooded", "non-flooded")
    
    st_drop_geometry(combined_sf)
  })
  
  output$flood_barplot <- renderPlot({
    df <- flooded_fix_summary()
    df$type <- factor(df$type, levels = c("used", "available"))
    
    # Boxplot (used vs available)
    p1 <- ggplot(df, aes(x = type, y = pixel_val, fill = type)) +
      geom_boxplot(width = 0.6, outlier.alpha = 0.3) +
      geom_hline(yintercept = 0.186, linetype = "dashed", color = "blue", linewidth = 1) +
      annotate("text", x = 1.5, y = 0.186, label = "↑ drier ↑", 
               vjust = -1, color = "black", size = 6) +
      annotate("text", x = 1.5, y = 0.186, label = "↓ wetter ↓", 
               vjust = 2.2, color = "black", size = 6) +
      scale_fill_manual(values = c("used" = "lightblue", "available" = "grey")) +
      labs(x = NULL, y = "Landsat SWIR reflectance\n (surface water)") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(size = 13),
            legend.position = "none")
    
    # Use only "used" points for flooded vs non-flooded barplot
    df_used <- df %>% filter(type == "used")
    
    p2 <- ggplot(df_used, aes(x = flooded, fill = flooded)) +
      geom_bar(aes(y = (..count..) / sum(..count..)), width = 0.6) +
      geom_text(stat = "count", aes(y = (..count..) / sum(..count..),
                                    label = paste0("GPS points = ", ..count..)),
                vjust = -0.5, size = 5) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = c("flooded" = "lightblue", "non-flooded" = "grey")) +
      labs(x = NULL, y = "Proportion of GPS points") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(size = 13))+
      theme(legend.position = "none")
    
    p1 + p2
  })
  
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
        addGlPoints(data = df_sf, group = "locations", popup = TRUE,
                    radius = 5, fillColor = "red") %>%
        {
          r <- selected_raster()
          if (!is.null(r)) {
            pal <- colorNumeric(palette = rev(RColorBrewer::brewer.pal(9, "Blues")),
                                domain = values(r), na.color = "transparent")
            addRasterImage(., r, colors = pal, opacity = 1, project = FALSE)
          } else .
        }
    }
  })
}

shinyApp(ui, server)
