source("Script.R")


data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec='.',sep=','))
 




#Transformation des coordonnées en crs -> 4326 



# View(coords)

# st_crs(points_transformed)

# head(points_transformed)
# length(points_transformed[1])
# points_transformed[[1]]





# install.packages('sf')
library(dplyr)

quartiers <- unique(data$clc_quartier)
colors <- colorFactor(palette = rainbow(length(quartiers)), levels = quartiers)


map_arbre <- function(data) {
    library(sf)
    library(dplyr)
    library(leaflet)

    points_sf <- st_as_sf(data.frame(x = c(data$X), y = c(data$Y)),
                          coords = c("x", "y"),
                          crs = 3949)
    points_transformed <- st_transform(points_sf, 4326)
    # points_transformed

    coords <- st_coordinates(points_transformed)
    long <- coords[, 1]
    lat <- coords[, 2]

    data_map <- data.frame(x = long, y = lat) %>%
        st_as_sf(coords = c("x", "y"), crs = 4326)


    data_map %>%
      leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addTiles() %>%
      addCircles(radius = ifelse(data$remarquable == "Oui",10,2),
                 color = ifelse(data$remarquable == "Oui","black",colors(data$clc_quartier)),
                 popup = ~paste("ID :", data$id_arbre,
                                "<br>Quartier :", data$clc_quartier,
                                "<br>Secteur :", data$clc_secteur,
                                "<br>Etat :", data$fk_arb_etat,
                                "<br> Remarquable :", data$remarquable)
                )
}



# map_arbre(data)

#Save map .html & .png
saveWidget(map_arbre(data), "temp.html", selfcontained = FALSE)
webshot("temp.html", file = "Rplot.png",cliprect = "viewport")