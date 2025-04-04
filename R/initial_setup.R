rm(list=ls())
easypackages::packages("shiny", "leaflet", "sf", "dplyr",'mapdeck', 
                       "leaflet.extras", "httr",'data.table'
                       ,'magrittr')
# 1) -------------
## LeafLet ---------
# Create a simple map centered in Toronto
leaflet() %>%
  addTiles() %>%
  setView(lng = -79.4, lat = 43.7, zoom = 10) %>%
  addMarkers(lng = -79.4, lat = 43.7, popup = "Example Location: Toronto, Canada")

# 2) ------------
## MapDeck ------------
mapdeckapi <- readLines("myapi_mapbox.txt")
mapdeckapi
mapdeck(token = mapdeckapi) %>%
  add_scatterplot(
    data = data.frame(lon = -79.4, lat = 43.7),
    lon = "lon", lat = "lat",
    radius = 10, fill_colour = "red"
  )

# 3) -----------
##  Adding Routing --------
api_key <- readLines("api_heigit.txt")

get_route <- function(start, end, api_key) {
  url <- "https://api.openrouteservice.org/v2/directions/driving-car/geojson"
  
  body <- list(
    coordinates = list(
      c(start$lon, start$lat),
      c(end$lon, end$lat)
    )
  )
  
  res <- httr::POST(
    url,
    add_headers("Authorization" = api_key, "Content-Type" = "application/json"),
    body = toJSON(body, auto_unbox = TRUE)
  )
  
  if (res$status_code != 200) {
    stop("API request failed")
  }
  
  coords <- content(res)$features[[1]]$geometry$coordinates
  data.frame(lon = sapply(coords, `[[`, 1),
             lat = sapply(coords, `[[`, 2))
}

# 4) ----------
## Build a Shiny App -----------

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Map with Routing & Popups"),
  leafletOutput("map", height = 600),
  #actionButton("clear", "Clear Markers"),
  actionButton("reset", "Reset Map"),
  textOutput("status")
)
server <- function(input, output, session,api_key) {
  #api_key <- "your_api_key"  # ← Replace this with your OpenRouteService API key
  
  # Store clicked points
  clicked_points <- reactiveVal(data.frame(lon = numeric(0), lat = numeric(0)))
  
  # Render the base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -79.4, lat = 43.7, zoom = 12)
  })
  
  # Handle map clicks
  observeEvent(input$map_click, {
    click <- input$map_click
    current <- clicked_points()
    
    if (nrow(current) < 2) {
      new <- rbind(current, data.frame(lon = click$lng, lat = click$lat))
      clicked_points(new)
      
      leafletProxy("map") %>%
        addMarkers(lng = click$lng, lat = click$lat, popup = paste("Point", nrow(new)))
      
      if (nrow(new) == 2) {
        tryCatch({
          route <- get_route(new[1,], new[2,], api_key)
          leafletProxy("map") %>%
            addPolylines(data = route, lng = ~lon, lat = ~lat, color = "blue", weight = 4)
          output$status <- renderText("Route successfully retrieved.")
        }, error = function(e) {
          output$status <- renderText("Failed to fetch route. Check API key or coordinates.")
        })
      }
    }
  })
  
  # Reset button
  observeEvent(input$reset, {
    clicked_points(data.frame(lon = numeric(0), lat = numeric(0)))
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes()
    output$status <- renderText("")
  })
}

# Run the Shiny app
shinyApp(ui, server)

# End -----------