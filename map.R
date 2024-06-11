source("Script.R")


data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec='.',sep=','))
 
library(sf)
library(dplyr)
library(leaflet)



#Transformation des coordonnées en crs -> 4326 



# View(coords)

# st_crs(points_transformed)

# head(points_transformed)
# length(points_transformed[1])
# points_transformed[[1]]





# install.packages('sf')



map_arbre <- function(data) {

    points_sf <- st_as_sf(data.frame(x = c(data$X), y = c(data$Y)), 
                        coords = c("x", "y"), 
                        crs = 3949)
    points_transformed <- st_transform(points_sf, 4326)
    points_transformed

    coords <- st_coordinates(points_transformed)
    long = coords[,1]
    lat = coords[,2]

    data_map <- data.frame(x= long, y= lat) %>%
        st_as_sf(coords=c("x", "y"), crs=4326)
    
    
    data_map %>%
      leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addTiles() %>%
      addCircles(radius = 2)
}

map_arbre(data)
