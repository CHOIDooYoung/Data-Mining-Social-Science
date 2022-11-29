#======================================================#
# Title: 'Spatial data for Social Science              #
# Sub-theme: "GSIAS HUFS Lecture 13 : Spatial data     #
# Author: "CHOI, Doo Young Wicks"                      #
# Date: 2022-11-28                                     #
#======================================================#

# Prerequisites and Preparations for the Spatial Geographic data mining with R

# Basic Map visualization

if(!require('leaflet'))install.packages('leaflet') # Map visualization
if(!require('leaflet.extras'))install.packages('leaflet.extras') # Map visualization




# Prerequisites and Preparations for the Spatial Geographic data mining with R
if(!require('rgdal'))install.packages('rgdal') # For the ESRI's ArgGIS shape file 
if(!require('raster'))install.packages('raster') # For the ESRI's ArgGIS shape file 
if(!require('sp'))install.packages('sp') # Classes and methods for spatial data, 
# e.g. for plotting data as maps, spatial selection, as well as methods for retrieving coordinates, for subsetting, print, summary, etc
if(!require('tidyverse'))install.packages('tidyverse')
if(!require('leaflet'))install.packages('leaflet') # Map visualization
if(!require('leaflet.extras'))install.packages('leaflet.extras') # Map visualization
if(!require('shiny'))install.packages('shiny') # A tool to build interactive web apps straight from R
if(!require('shinydashboard'))install.packages('shinydashboard') # A tool to build interactive web apps straight from R


#============================================
# Create a map 
#============================================
My.first.Map <- leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12)
My.first.Map %>% addTiles()





#============================================
# Little bit advanced 
#============================================


# Download World Bank-approved administrative boundaries map shape data
# https://datacatalog.worldbank.org/search/dataset/0038272/World-Bank-Official-Boundaries
# World Country Polygons - Very High Definition


# Load shape data
world.shape.df <- readOGR(dsn = file.choose())
world.shape.df$POP_EST= as.numeric(as.character(world.shape.df$POP_EST)) / 1000000 %>% round(2)

# Set Shiny Dash Board interface

User.interface <- dashboardPage(
  dashboardHeader(title = "R based Geo Spatial Application"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Maps",
        tabName = "maps",
        icon = icon("globe"),
        menuSubItem("Earthquake OSM", tabName = "m_osm", icon = icon("map")),
        menuSubItem("Earthquake Dark", tabName = "m_dark", icon = icon("map")),
        menuSubItem("Earthquake HeatMap", tabName = "m_heat", icon=icon("map")),
        menuSubItem("World Population", tabName = "m_chor", icon=icon("map"))
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "m_osm",
        tags$style(type = 'text/css', '#earthq_osm {height: calc(100vh - 80px) !important;}'),
        leafletOutput('earthq_osm')
      ),
      tabItem(
        tabName = "m_dark",
        tags$style(type = 'text/css', '#earthq_dark {height: calc(100vh - 80px) !important;}'),
        leafletOutput('earthq_dark')
      ),
      tabItem(
        tabName = "m_heat",
        tags$style(type = 'text/css', '#earthq_heat {height: calc(100vh - 80px) !important;}'),
        leafletOutput('earthq_heat')
      ),
      tabItem(
        tabName = "m_chor",
        tags$style(type = 'text/css', '#world_chor {height: calc(100vh - 80px) !important;}'),
        leafletOutput('world_chor')
      )
    )
  )
)

server <- function(input, output, session) {
  
  data(quakes)
  
  output$earthq_osm <- renderLeaflet({
    pal <- colorNumeric("Blues", quakes$mag)
    
    leaflet(data = quakes) %>% addTiles(group = "OpenStreetMap") %>%
      
      addProviderTiles(providers$Esri.WorldStreetMap, options = tileOptions(minZoom=0, maxZoom=13), group = "Esri.WorldStreetMap") %>%
      
      addProviderTiles(providers$Esri.WorldImagery, options=tileOptions(minZoom=0, maxZoom=13), group = "Esri.WorldImagery") %>%
      
      addCircles(radius=~10^mag/10, weight=1, color=~pal(mag), fillColor=~pal(mag), fillOpacity = 0.7, popup=~as.character(mag), label=~as.character(mag), group="Points") %>%
      
      addMarkers(lng = ~long, lat = ~lat, popup=~as.character(mag), label=~as.character(mag), group = "Markers") %>%
      
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Esri.WorldStreetMap", "Esri.WorldImagery"),
        overlayGroups = c("Markers", "Points"),
        options = layersControlOptions(collapsed=TRUE)
      ) %>%
      
      addLegend(
        position = "bottomright",
        pal=pal,
        values = ~mag, group = "Points",
        title = "Earthquake Magnitude"
      )
    
  })
  
  output$earthq_dark <- renderLeaflet({
    pal <- colorNumeric("OrRd", quakes$mag)
    
    leaflet(data = quakes) %>% addProviderTiles(providers$CartoDB.DarkMatter, options=tileOptions(minZoom=0, maxZoom=7)) %>%
      
      addCircles(radius = ~10^mag/10, weight=1, color=~pal(mag), fillColor = ~pal(mag), fillOpacity = 0.7, popup=~as.character(mag), label=~as.character(mag), group="Points") %>%
      
      addProviderTiles(providers$Esri.WorldImagery, options = tileOptions(minZoom=7, maxZoom=14)) %>%
      
      addLegend(
        position = "bottomright",
        pal=pal,
        values = ~mag, group = "Points",
        title = "Earthquake Magnitude"
      )
    
  })
  
  # Heatmap 
  output$earthq_heat <- renderLeaflet({
    pal <- colorNumeric("RdYlBu", quakes$mag)
    
    leaflet(data = quakes) %>% addProviderTiles(providers$Esri.WorldImagery, options=tileOptions(minZoom=0, maxZoom=13)) %>%
      
      addHeatmap(
        lng = ~long, lat = ~lat, intensity = ~mag, blur = 20, max = 0.05, radius = 15
      )
    
  })
  
  # Choropleth Map
  output$world_chor <- renderLeaflet({
    
    bins=c(0,10,20,50,100,500,Inf)
    pal = colorBin(palette = "YlOrBr", domain=world.shape.df$POP_EST, na.color = "transparent", bins=bins)
    
    customLabel = paste("Country: ", world.shape.df$NAME_EN, "<br/>", "Population: ", round(world.shape.df$POP_EST, 2), sep = "") %>% 
      lapply(htmltools::HTML)
    
    leaflet(world.shape.df) %>%
      
      addProviderTiles(providers$OpenStreetMap, options = tileOptions(minZoom=2, maxZoom=8)) %>%
      
      addPolygons(fillColor = ~pal(POP_EST),
                  fillOpacity = 0.9,
                  stroke = TRUE,
                  color="white",
                  highlight=highlightOptions(
                    weight = 5,
                    fillOpacity = 0.3
                  ),
                  label = customLabel,
                  weight = 0.3,
                  smoothFactor = 0.2) %>%
      
      addLegend(
        pal=pal,
        values = ~POP_EST,
        position = "bottomright",
        title ="World Population (Millions)"
      )
  })
}

shinyApp(User.interface, server)














