
data_vide <- read.csv("Patrimoine_Arbore_modif.csv", dec = ".", sep = ",")
View(data_vide)

traitement <- function(data) {


  #Numérique
  for (i in c(1:3, 9:12, 21:23)) {
    data[, i] <- as.numeric(data[, i])
  }


  #Date
  for (i in c(4, 20, 24, 27, 32, 34)){
    data[, i] <- as.Date(data[, i])
  }




  #Arbre sans quartier
  data$clc_quartier[data$clc_quartier == ""] <- "inconnu"
  data$clc_quartier[is.na(data$clc_quartier)] <- "inconnu"

  #Src geo
  

  #feuillage
  data$feuillage[data$feuillage == ""] <- "inconnu"
  
  #stade de dev
  data$fk_stadedev[data$fk_stadedev == ""] <- "inconnu"

  #fk_port port de l'abre , forme et strucutre
  data$fk_port[data$fk_port == ""] <- "inconnu"

  #fk_pied
  data$fk_pied[data$fk_pied == ""] <- "inconnu"

  #fk_situation
  data$fk_situation[data$fk_situation == ""] <- "inconnu"

  #fk_revetement
  data$fk_revetement[data$fk_revetement == ""] <- "inconnu"

  #tronc_diam
  data$tronc_diam[] <- data$tronc_diam[]/100

  #Gestion User inconnu
  data$created_user[data$created_user == ""] <- "inconnu"
  data$created_user[data$created_user == "Edouard Cauchon"] <- "edouard.cauchon"

  data$last_edited_user[data$last_edited_user == ""] <- "inconnu"

  data$Creator[data$Creator == ""] <- "inconnu"

  data$Editor[data$Editor == ""] <- "inconnu"

  #Chr UTF-8
  for (i in c(6:8, 13:19, 25, 26, 28:31, 33, 35:37)) {
    data[, i] <- iconv(data[, i], from = "latin1", to = "UTF-8")
  }

  #Remarquable
  data$remarquable[data$remarquable == ""] <- "Non"
  data$remarquable[is.na(data$remarquable)] <- "Non"


  data$src_geo[data$src_geo == ""] <- "à renseigner"
  
  for (i in c("Orthophoto", "orthophoto ", "Orthophoto plan", "Plan ortho")) {
     data$src_geo[data$src_geo == i] <- "orthophoto"
  }
  

  data$created_user <- as.factor(data$created_user)
  data$last_edited_user <- as.factor(data$last_edited_user)
  data$Creator <- as.factor(data$Creator)
  data$Editor <- as.factor(data$Editor)
  data$remarquable <- as.factor(data$remarquable)



  #As factor 28 -> nom ville
  for (i in c(5:8, 13, 15:17, 19, 25, 26, 30, 31, 36)) {
    data[, i] <- as.factor(data[, i])
  }

  #Nom de la ville
  data$villeca[] <- "CASQ"
  data$villeca <- as.factor(data$villeca)


  #Suppression des données invalides sur le coordonnées dhfcg
  data <- data[!is.na(data$X), ]
  data <- data[!is.na(data$Y), ]


  #remplis la colonne du stade avec la valeur "jeune" si age_estim=0
  data$fk_stadedev <- ifelse(data$age_estim == 0, "jeune", data$fk_stadedev)




  #age estime
  data$age_estim[data$age_estim > 500] <- NA
  mean_age <- round(mean(data$age_estim < 500, na.rm = TRUE))
  data$age_estim[is.na(data$age_estim)] <- mean_age



  # length(data[,1])
  return(data)

}



install.packages("ggplot2")
library(ggplot2)



# data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec='.',sep=','))
# View(data)
# summary(data)
# View(data$fk_stadedev)

# datab <- read.csv("Patrimoine_Arboré_(RO).csv", dec='.',sep=',')
# View(datab)





#Stat descriptive univariée

freq_categorielle = function(tab){
  
  tab <- data[[tab]]
  my_plto <- ggplot(data, aes(tab)) +
    geom_bar( fill = "blue", position="dodge") +
    labs(binwidth = 1, title = "Fréquence des catégories",
         x = "Catégories",
         y = "Fréquence") +
    theme_minimal()

  my_plto
}

# freq_categorielle("remarquable")


#Stat descirptive bivariée

freq_biv <- function(tab1, tab2) {
  tab_1 <- data[[tab1]]
  tab_2 <- data[[tab2]]

  my_plto <- ggplot(data, aes(x = tab_1, fill = tab_2)) +
    geom_bar( fill = "blue", position="dodge") +
    labs(binwidth = 1, title = "Fréquence des catégories",
         x = "Catégories",
         y = "Fréquence") +
    theme_minimal()

  my_plto
}

# freq_biv("fk_stadedev", "haut_tot")

# summary(data)
# head(data)
# data[1,1]
