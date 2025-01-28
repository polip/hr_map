library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)

# Download and prepare Croatia counties data from GADM
gradovi <- sf::read_sf('gadm41_HRV_2.json') %>%
  rename(grad = NAME_2, zupanija=NAME_1) # Rename to match our existing code

zupanije <- sf::read_sf('gadm41_HRV_1.json') %>%
  rename(zupanija=NAME_1)

ui <- page_sidebar(
  title = "Croatia Counties Map",
  sidebar = sidebar(
    title = "Controls",
    checkboxGroupInput(
      "selected_counties",
      "Select Counties:",
      choices = sort(zupanije$zupanija) ,
      selected = sort(zupanije$zupanija)
    ),
    checkboxInput(
      "select_all",
      "Select/Deselect All",
      value = TRUE
    )
  ),
  card(
    full_screen = TRUE,
    card_header("Interactive Map of Croatian Counties"),
    leafletOutput("map", height = "70vh")
  )
)

server <- function(input, output, session) {
  
  # Observer for select all checkbox
  observe({
    if (input$select_all) {
      updateCheckboxGroupInput(session, 
                               "selected_counties",
                               selected = sort(zupanije$zupanija))
    } else {
      updateCheckboxGroupInput(session, 
                               "selected_counties",
                               selected = character(0))
    }
  })
  
  # Observer to update select all checkbox
  observe({
    if (length(input$selected_counties) == length(zupanije$zupanija)) {
      updateCheckboxInput(session, "select_all", value = TRUE)
    } else if (length(input$selected_counties) == 0) {
      updateCheckboxInput(session, "select_all", value = FALSE)
    }
  })
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    # Filter the data based on selected counties
    selected_data <- gradovi %>%
      filter(zupanija %in% input$selected_counties)
    
    # Create the map
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  # Changed base map style
      addPolygons(
        data = selected_data,
        fillColor = "#1E88E5",
        fillOpacity = 0.7,
        color = "white",
        weight = 2,
        label = ~grad,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      setView(lng = 16.4, lat = 45.1, zoom = 7)
  })
}

shinyApp(ui, server)
