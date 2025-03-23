library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(bslib)
library(RColorBrewer)
library(shinyjs)

# Load Croatia's administrative boundaries directly from local files
# This assumes the data files are available in the app directory

# Load the data
croatia_lau_data <- lau_data <- st_read('lau_hr.geojson')

# Create a function to make a map
create_map <- function(sf_data, color_var, transform = FALSE, palette_choice = "viridis") {
  # Determine label column (assuming "NAME" is common in both datasets)
  label_col <- if("NAME" %in% names(sf_data)) "NAME" else 
    if(any(grepl("NAME", names(sf_data)))) grep("NAME", names(sf_data), value = TRUE)[1] else
      names(sf_data)[1]
  
  # Define available color palettes
  palettes <- list(
    viridis = "viridis",
    magma = "magma",
    plasma = "plasma",
    inferno = "inferno",
    blues = colorRampPalette(c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B"))(100),
    greens = colorRampPalette(c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B"))(100),
    reds = colorRampPalette(c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#A50F15", "#67000D"))(100),
    purples = colorRampPalette(c("#FCFBFD", "#EFEDF5", "#DADAEB", "#BCBDDC", "#9E9AC8", "#807DBA", "#6A51A3", "#54278F", "#3F007D"))(100),
    oranges = colorRampPalette(c("#FFF5EB", "#FEE6CE", "#FDD0A2", "#FDAE6B", "#FD8D3C", "#F16913", "#D94801", "#A63603", "#7F2704"))(100),
    YlOrRd = colorRampPalette(brewer.pal(9, "YlOrRd"))(100),
    YlGnBu = colorRampPalette(brewer.pal(9, "YlGnBu"))(100),
    RdYlGn = colorRampPalette(brewer.pal(11, "RdYlGn"))(100),
    PuOr = colorRampPalette(brewer.pal(11, "PuOr"))(100)
  )
  
  # Select the palette
  selected_palette <- palettes[[palette_choice]]
  
  # Transform the data if needed (for population)
  if(transform && color_var == "POP_2021") {
    # Use log transformation for population
    values_to_plot <- log10(sf_data[[color_var]] + 1) # +1 to avoid log(0)
    
    # Create a color palette based on the transformed variable
    pal <- colorNumeric(
      palette = selected_palette,
      domain = values_to_plot
    )
    
    # Custom labels for the legend that show original values
    legend_values <- pretty(sf_data[[color_var]])
    legend_labels <- as.character(legend_values)
    
    # Create the leaflet map with transformed values
    map <- leaflet(sf_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(log10(get(color_var) + 1)),
        weight = 1,
        opacity = 1,
        color = "#666",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#000",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0(get(label_col), ": ", get(color_var)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        colors = colorNumeric(selected_palette, domain = log10(legend_values + 1))(log10(legend_values + 1)),
        labels = legend_labels,
        title = "Population (2021)<br><small>Log scale</small>",
        opacity = 0.7
      ) %>%
      setView(lng = 16.4, lat = 44.5, zoom = 7) # Set view to approximately center Croatia
  } else {
    # Regular palette for non-transformed variables
    pal <- colorNumeric(
      palette = selected_palette,
      domain = sf_data[[color_var]]
    )
    
    # Create the leaflet map without transformation
    map <- leaflet(sf_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(get(color_var)),
        weight = 1,
        opacity = 1,
        color = "#666",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#000",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0(get(label_col), ": ", get(color_var)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~get(color_var),
        title = color_var,
        opacity = 0.7
      ) %>%
      setView(lng = 16.4, lat = 44.5, zoom = 7) # Set view to approximately center Croatia
  }
  
  return(map)
}

# Define available variables for coloring (focus on the specified variables)
available_vars <- c(
  "Population (2021)" = "POP_2021",
  "Area (km²)" = "AREA_KM2"
)

# UI
ui <- page_fluid(
  title = "Croatia Administrative Maps",
  useShinyjs(),  # Enable shinyjs
  card(
    card_header(
      h1("Croatia Administrative Map", class = "text-center")
    ),
    card_body(
      page_sidebar(
        sidebar = sidebar(
          width = 300,
          h4("Map Controls"),
          selectInput(
            "lau_color_var", 
            "Color by Variable:", 
            choices = available_vars, 
            selected = "POP_2021"
          ),
          # Add color palette selector
          selectInput(
            "color_palette",
            "Color Palette:",
            choices = list(
              "Sequential (Blue to Purple)" = "viridis",
              "Sequential (Purple to Yellow)" = "magma",
              "Sequential (Green to Purple)" = "plasma",
              "Sequential (Black to Yellow)" = "inferno",
              "Blues (Light to Dark)" = "blues",
              "Greens (Light to Dark)" = "greens",
              "Reds (Light to Dark)" = "reds",
              "Purples (Light to Dark)" = "purples",
              "Oranges (Light to Dark)" = "oranges",
              "Yellow-Orange-Red" = "YlOrRd",
              "Yellow-Green-Blue" = "YlGnBu",
              "Red-Yellow-Green" = "RdYlGn",
              "Purple-Orange" = "PuOr"
            ),
            selected = "viridis"
          ),
          checkboxInput(
            "transform_data",
            "Transform Population Data (Log Scale)",
            value = TRUE
          ),
          hr(),
          p("This map shows Local Administrative Units (LAU) in Croatia, representing municipalities and cities."),
          conditionalPanel(
            condition = "input.lau_color_var == 'POP_2021' && input.transform_data == true",
            p("Population data is log-transformed to better visualize differences between municipalities.", 
              style = "font-style: italic; color: #666;")
          )
        ),
        leafletOutput("lau_map", height = "600px")
      )
    ),
    card_footer(
      p("Population data: 2021 Census", class = "text-center")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Render LAU map
  output$lau_map <- renderLeaflet({
    req(input$lau_color_var)
    create_map(
      croatia_lau_data, 
      input$lau_color_var, 
      transform = input$transform_data,
      palette_choice = input$color_palette
    )
  })
  
  # Hide transform checkbox when not showing population
  observe({
    if(input$lau_color_var != "POP_2021") {
      updateCheckboxInput(session, "transform_data", value = FALSE)
      shinyjs::hide("transform_data")
    } else {
      shinyjs::show("transform_data")
    }
  })
}

#shinylive::export(destdir = 'hr_map',appdir = 'app') 

# Run the app
shinyApp(ui = ui, server = server)