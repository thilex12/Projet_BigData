
data <- read.csv("Patrimoine_Arbore_modif.csv", dec = ".", sep = ",")


traitement <- function(data) {


  #Numérique
  for (i in c(1:3, 9:12, 21:23)) {
    data[, i] <- as.numeric(data[, i])
  }


  #Date
  for (i in c(4, 20, 24, 27, 32, 34)){
    data[, i] <- as.Date(data[, i])
  }




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
  data$created_user <- as.factor(data$created_user)
  data$last_edited_user <- as.factor(data$last_edited_user)
  data$Creator <- as.factor(data$Creator)
  data$Editor <- as.factor(data$Editor)



  #As factor 28 -> nom ville
  for (i in c(5:8, 13:17, 19, 25, 26, 30, 31, 36)) {
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




age_arbre <- function(arbres) {
  if(arbres >= 250){return(FALSE)}
  else{return(TRUE)}
}


install.packages("ggplot2")
library(ggplot2)



data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec='.',sep=','))
View(data)
summary(data)
# View(data$created_user)

# datab <- read.csv("Patrimoine_Arboré_(RO).csv", dec='.',sep=',')
# View(datab)


# data$clc_quartier <- iconv(data$clc_quartier,from = "latin1" , to="UTF-8")
# data$clc_secteur <- iconv(data$clc_secteur,from = "latin1" , to="UTF-8")
# data$fk_arb_etat <- iconv(data$fk_arb_etat,from = "latin1" , to="UTF-8")
# data$commentaire_environnement <- iconv(data$commentaire_environnement,from = "latin1" , to="UTF-8") # nolint: line_length_linter.


# data$X <- as.numeric(data$X)
# data$Y <- as.numeric(data$Y)
# data$OBJECTID <- as.numeric(data$OBJECTID)
# data$created_date <- as.Date(data$created_date)

# names = colnames(data)






freq_categorielle = function(tab){
  tab <- data.frame(tab)
  len <- length(summary(tab))
  
  
  total <- sum(summary(tab))
  # freq <- c(summary(tab)) / total
  freq <- table(tab)
  
  ggplot(freq)
}

# freq_categorielle(data$remarquable)


# summary(data)
# head(data)
# data[1,1]
