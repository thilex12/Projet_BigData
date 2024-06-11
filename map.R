source("Script.R")

install.packages("tidyverse")
install.packages("patchwork")
install.packages("lwgeom")
install.packages("leaflet")
 
library(leaflet)
library(sf)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(lwgeom)

nantes<-data.frame(points_transformed$) %>% 
    st_as_sf(coords = c("lon", "lat")) %>% 
  st_set_crs(4326)

mapview(nantes)

map_data <- points_sf
map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>% 
  addTiles(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
  updateWhenIdle = TRUE)           # map won't load new tiles when panning
    %>%
  setView(lng = 3.293264, lat = 49.8405, zoom = 6) %>%
  addCirclesMarkers(data = coords, x = ~X, y = ~Y, radius = 1, color='red')
map

points_sf <- st_as_sf(data.frame(x = c(data$X), y = c(data$Y)), 
                      coords = c("x", "y"), 
                      crs = 3949)
points_transformed <- st_transform(points_sf, 4326)
points_transformed

coords <- st_coordinates(points_transformed)
long = coords[,1]
lat = coords[,2]


sfc = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
sfc %>% st_set_crs(4326) %>% st_transform(3857)

View(coords)

st_crs(points_transformed)

head(points_transformed)
length(points_transformed[1])
points_transformed[[1]]


