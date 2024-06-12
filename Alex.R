source("Script.R")

install.packages("ggplot2")
library(ggplot2)
install.packages("sf")
library(sf)

data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec='.',sep=','))
View(data$fk_pied)

# x <- st_transform(data$X, crs = st_crs(data$X), dst= 4326)
sfc = st_sfc(st_point(c(1720320.1079,1721095.6459)), st_point(c(8294619.3561,8293514.7374)), crs = 3949)
x =st_transform(sfc, crs = 3949, dst = 4326)
# st_transform(sfc, 4326)
x
x[1][1]

# for(i in length(data$X)){

#     data$X[i],data$Y[i] <- st_transform(st_sfc(st_point(c(data$X[i], data$Y[i]), crs = 3949)), crs = 3949, dst = 4326)

# }

# View(data)
# sfc <- st_sfc()

install.packages("mapview")
library(mapview)
install.packages("sp")
library(sp)

mapview(x = c(data$X,data$Y), xcol = data$X, ycol = data$Y, crs = 3949, dst = 4326)
spTransform(c(data$X,data$Y), "+init=epsg:3949")




freq_biv <- function(tab1, tab2) {
  tab_1 <- data[[tab1]]
  tab_2 <- data[[tab2]]

  my_plto <- ggplot(data, aes(x = tab_1, fill = tab_2)) +
    geom_histogram(binwidth = 5, alpha = 0.7 ,color = 'cyan' ,fill = "blue") +
    labs(binwidth = 1, title = "Fréquence des catégories",
         x = "Catégories",
         y = "Fréquence") +
    theme_minimal()

  my_plto
}

freq_biv("age_estim", "haut_tot")

box_plot <- function(tab1, tab2) {
  tab_1 <- data[[tab1]]
  tab_2 <- data[[tab2]]

  my_plto <- ggplot(data, aes(x = tab_1, y = tab_2)) +
    geom_boxplot(alpha = 0.7, color = "black", fill = "blue") +
    labs(title = "Fréquence des catégories",
         x = "Catégories",
         y = "Fréquence") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  my_plto
}

box_plot("clc_quartier", "haut_tot")
