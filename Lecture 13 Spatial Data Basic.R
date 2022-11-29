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
if(!require('readxl'))install.packages('readxl') # Excel file manipulation
if(!require('writexl'))install.packages('writexl') # Excel file manipulation
if(!require('ggmap'))install.packages('ggmap') # google map tile


# Layer Structure
# Category    Add_function
# tile	      addTiles, addProviderTiles
# marker	    addMarkers, addCircleMarkers
# popup	      addPopups
# shape	      addPolygons, addPolylines, addCircles, addRectangles
# geojson	    addTopoJSON
# topjson	    addTopoJSON
# control	    addControl


# Basic idea of leaflet

# Name.of.Object <- leaflet () "Create an object" %>%
                    # Selecting a tile (basic map) %>%
                    # Selecting targeted view points %>%
                    # Plot the layer %>%



# Hello World : Create my first Map


HUFS <- leaflet() %>% #Create an object
  addTiles() %>% #Select a tile
  setView(lng = 127.05929, lat = 37.59809, zoom = 16) # Selecting targeted view points

HUFS 


#Load POI (Point of Interest) 
POI01 <- read_excel(file.choose()) # HUFS_POI_Sample.xlsx
POI02 <- read.csv(file.choose()) # map.csv

# Data with Marker

HUFS <- leaflet(POI01) %>%
  addTiles() %>% #Select a tile
  setView(lng = 127.05929, lat = 37.59809, zoom = 16) %>%  # Selecting targeted view points
  addMarkers(lng = ~lng,lat = ~lat)
HUFS

# Data with Marker and overall name display

HUFS <- leaflet(POI) %>%
  addTiles() %>% #Select a tile
  setView(lng = 127.05929, lat = 37.59809, zoom = 16) %>%  # Selecting targeted view points
  addMarkers(lng = ~lng,lat = ~lat, popup = ~pop, label = paste(POI$pop))
HUFS


# Data with Marker and additonal tools

HUFS <- leaflet(POI01) %>%
  addTiles() %>% #Select a tile
  setView(lng = 127.05929, lat = 37.59809, zoom = 16) %>%  # Selecting targeted view points
  addMarkers(lng = ~lng,lat = ~lat, popup = ~pop, label = paste(POI01$pop)) %>%
  addDrawToolbar(
  editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions()))

HUFS

# Mark the map with the circle

HUFS <- leaflet(POI) %>%
  addTiles() %>% #Select a tile
  setView(lng = 127.05929, lat = 37.59809, zoom = 16) %>%  # Selecting targeted view points
  addCircleMarkers(lng = ~lng,lat = ~lat, radius = 20, color = "#09f", weight = 2, label = paste(POI$pop))
HUFS


#Sample with Social Data
Bicycle.Theaft<-POI02[which(POI02$Crime_type == 'Bicycle theft'),]
View(Bicycle.Theaft)

##################################################
Bike.Theaf <- leaflet(Bicycle.Theaft) %>%
  addTiles() %>% #Select a tile
  setView(lng = -2.9437, lat = 53.45, zoom = 10) %>%  # Selecting targeted view points
  addMarkers(lng = ~Longitude, lat = ~Latitude, label = paste(Bicycle.Theaft$Location))
Bike.Theaf
