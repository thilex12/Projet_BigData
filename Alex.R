source("Script.R")

install.packages("ggplot2")
library(ggplot2)

install.packages("sf")
library(sf)

install.packages("mapview")
library(mapview)

install.packages("sp")
library(sp)


data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec='.',sep=','))
View(data)



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
















#Prediction des valeurs manquante pour predire les ages manquants


#Prediciton hauteur tronc ~ haut_tot + tronc_diam + age_estim

data_5 <- data[data$haut_tronc != 0 & data$haut_tot != 0 & data$tronc_diam != 0, ]

hauttr_htot_diam <- lm(haut_tronc ~ haut_tot + tronc_diam + age_estim , data = data_5)
# summary(hauttr_htot_diam) #R 0.3

for (i in row(data)){
  if(
    data$haut_tronc[i] == 0 &

    data$haut_tot[i] != 0 &
    data$tronc_diam[i] != 0 &
    data$age_estim[i] != 0 &

    data$fk_arb_etat[i] == "EN PLACE"
  ){
    data$haut_tronc[i] <- predict(hauttr_htot_diam, data.frame("haut_tot" = data$haut_tot[i], "tronc_diam" = data$tronc_diam[i], "age_estim" = data$age_estim[i]))
  }
}




#hauteur tronc ~ haut_tot + age_estim 
data_4 <- data[data$haut_tronc != 0 & data$haut_tot != 0 & data$age_estim != 0, ]

tronc_haut_age_dev <- lm(haut_tronc ~ haut_tot + age_estim, data = data_4)
# summary(tronc_haut_age_dev) # R 0.3

for (i in row(data)){
  if(
    data$haut_tronc[i] == 0 &
    data$haut_tot[i] != 0 &
    data$age_estim[i] != 0 &
    data$fk_arb_etat[i] == "EN PLACE"
  ){
    data$haut_tronc[i] <- predict(tronc_haut_age_dev, data.frame("haut_tot" = data$haut_tot[i], "age_estim" = data$age_estim[i]))
  }
}





# Tronc diam ~ haut_tot + age_estim + haut_tronc

data_6 <- data[data$tronc_diam != 0 & data$haut_tot != 0 & data$age_estim != 0 & data$haut_tronc != 0, ]

tronc_diam_htot_age_tronc <- lm(tronc_diam ~ haut_tot + age_estim + haut_tronc, data = data_6)
# summary(tronc_diam_htot_age_tronc) #R 0.6 good

for (i in row(data)){
  if(
    data$tronc_diam[i] == 0 &

    data$haut_tot[i] != 0 &
    data$age_estim[i] != 0 &
    data$haut_tronc[i] != 0 &

    data$fk_arb_etat[i] == "EN PLACE"
  ){
    data$tronc_diam[i] <- predict(tronc_diam_htot_age_tronc, data.frame("haut_tot" = data$haut_tot[i], "age_estim" = data$age_estim[i], "haut_tronc" = data$haut_tronc[i]))
  }
}




# Prediciton hauteur tronc ~ haut_tot + tronc_diam

data_7 <- data[data$haut_tronc != 0 & data$haut_tot != 0 & data$tronc_diam != 0, ]

tronc_haut_htot_diam <- lm(haut_tronc ~ haut_tot + tronc_diam, data = data_7)
# summary(tronc_haut_htot_diam) #R 0.27


for (i in row(data)){
  if(
    data$haut_tronc[i] == 0 &

    data$haut_tot[i] != 0 &
    data$tronc_diam[i] != 0 &

    data$fk_arb_etat[i] == "EN PLACE"
  ){
    data$haut_tronc[i] <- predict(tronc_haut_htot_diam, data.frame("haut_tot" = data$haut_tot[i], "tronc_diam" = data$tronc_diam[i]))
  }
}

View(data)




# --- Prediction des ages --- #

data_age <- data[data$haut_tot != 0 & data$haut_tronc != 0 & data$tronc_diam != 0 & data$age_estim != 0, ]

model_age <- lm(age_estim ~ haut_tot + haut_tronc + tronc_diam, data = data_age)
# summary(model_age)

for (i in row(data)){
  if(
    data$age_estim[i] == 0 &

    data$haut_tot[i] != 0 &
    data$haut_tronc[i] != 0 &
    data$tronc_diam[i] != 0 &

    data$fk_arb_etat[i] == "EN PLACE"
  ){
    data$age_estim[i] <- predict(model_age, data.frame("haut_tot" = data$haut_tot[i], "haut_tronc" = data$haut_tronc[i], "tronc_diam" = data$tronc_diam[i]))
  }
}


View(data)

# par(mfrow = c(2, 2))
# plot(tronc_haut_htot_diam)