# source("Script.R")


# data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec='.',sep=','))
 




#Transformation des coordonnées en crs -> 4326 



# View(coords)

# st_crs(points_transformed)

# head(points_transformed)
# length(points_transformed[1])
# points_transformed[[1]]





# install.packages('sf')
# library(dplyr)




map_arbre <- function(data) {
    

    install.packages("sf")
    install.packages("dplyr")
    install.packages("leaflet")

    library(sf)
    library(dplyr)
    library(leaflet)

    quartiers <- unique(data$clc_quartier)
    colors_quartiers <- colorFactor(palette = rainbow(length(quartiers)), levels = quartiers)

    etats <- unique(data$fk_arb_etat)
    colors_etat <- colorFactor(palette = rainbow(length(etats)), levels = etats)
    
    stadedev <- unique(data$fk_stadedev)
    colors_dev <- colorFactor(palette = rainbow(length(stadedev)), levels = stadedev)


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
                 color = ifelse(data$remarquable == "Oui","black",colors_quartiers(data$clc_quartier)),
                 popup = ~paste("ID :", data$id_arbre,
                                "<br>Quartier :", data$clc_quartier,
                                "<br>Secteur :", data$clc_secteur,
                                "<br>Etat :", data$fk_arb_etat,
                                "<br> Remarquable :", data$remarquable),
                 group = "Quartiers"
                ) %>%
      addLegend(position = "bottomright",
                colors = colors_quartiers(quartiers),
                labels = quartiers,
                title = "Quartiers",
                group = "Quartiers") %>%
      
      addCircles(radius = 2,
                 color = colors_etat(data$fk_arb_etat),
                 popup = ~paste("ID :", data$id_arbre,
                                "<br>Quartier :", data$clc_quartier,
                                "<br>Secteur :", data$clc_secteur,
                                "<br>Etat :", data$fk_arb_etat,
                                "<br> Remarquable :", data$remarquable),
                 group = "Etat"
                ) %>%
      addLegend(position = "bottomright",
                colors = colors_etat(etats),
                labels = etats,
                title = "Etat de l'arbre",
                group = "Etat") %>%

      addCircles(radius = 2,
                 color = colors_dev(data$fk_stadedev),
                 popup = ~paste("ID :", data$id_arbre,
                                "<br>Quartier :", data$clc_quartier,
                                "<br>Secteur :", data$clc_secteur,
                                "<br>Etat :", data$fk_arb_etat,
                                "<br> Remarquable :", data$remarquable),
                 group = "Developpement"
                ) %>%
      addLegend(position = "bottomright",
                colors = colors_dev(stadedev),
                labels = stadedev,
                title = "Stade de devleoppement",
                group = "Developpement") %>%

      addLayersControl(overlayGroups = c("Quartiers", "Etat", "Developpement"))


    
}













map_arbre_quartier <- function(data) {
    

    install.packages("sf")
    install.packages("dplyr")
    install.packages("leaflet")

    library(sf)
    library(dplyr)
    library(leaflet)

    quartiers <- unique(data$clc_quartier)
    colors <- colorFactor(palette = rainbow(length(quartiers)), levels = quartiers)

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
                ) %>%
      addLegend(position = "bottomright",
      colors = colors(quartiers),
      labels = quartiers,
      title = "Quartiers")
    # return(data_map)
}



# map_arbre(data)





map_arbre_etat <- function(data) {
    

    install.packages("sf")
    install.packages("dplyr")
    install.packages("leaflet")

    library(sf)
    library(dplyr)
    library(leaflet)

    etats <- unique(data$fk_arb_etat)
    colors <- colorFactor(palette = rainbow(length(etats)), levels = etats)

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
      addCircles(radius = 2,
                 color = colors(data$fk_arb_etat),
                 popup = ~paste("ID :", data$id_arbre,
                                "<br>Quartier :", data$clc_quartier,
                                "<br>Secteur :", data$clc_secteur,
                                "<br>Etat :", data$fk_arb_etat,
                                "<br> Remarquable :", data$remarquable)
                ) %>%
      addLegend(position = "bottomright",
        colors = colors(etats),
        labels = etats,
        title = "Etat de l'arbre")
    # return(data_map)
}


map_arbre_stadedev <- function(data) {
    

    install.packages("sf")
    install.packages("dplyr")
    install.packages("leaflet")

    library(sf)
    library(dplyr)
    library(leaflet)

    stadedev <- unique(data$fk_stadedev)
    colors <- colorFactor(palette = rainbow(length(stadedev)), levels = stadedev)

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
      addCircles(radius = 2,
                 color = colors(data$fk_stadedev),
                 popup = ~paste("ID :", data$id_arbre,
                                "<br>Quartier :", data$clc_quartier,
                                "<br>Secteur :", data$clc_secteur,
                                "<br>Etat :", data$fk_arb_etat,
                                "<br> Remarquable :", data$remarquable)
                ) %>%
      addLegend(position = "bottomright",
      colors = colors(stadedev),
      labels = stadedev,
      title = "Stade de devleoppement")
    # return(data_map)
}



#Save map .html & .png
# saveWidget(map_arbre(data), "temp.html", selfcontained = FALSE)
# webshot("temp.html", file = "Rplot.png", cliprect = "viewport")


map_web <- function(map){
    install.packages("devtools")

    library(devtools)
    install_github("wch/webshot")

    library(htmlwidgets)
    library(webshot)
    saveWidget(map, "temp.html", selfcontained = FALSE)
    webshot("temp.html", file = "Rplot.png", cliprect = "viewport")
}

# map_web(map_arbre(data))
# map_web(map_arbre_etat(data))
# map_web(map_arbre(data))





