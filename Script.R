
data <- read.csv("Patrimoine_Arbore_modif.csv", dec='.',sep=',')


traitement <- function(data){
  
  #Numérique
  for (i in c(1:3,9:12,21:23)) {
    data[,i] <- as.numeric(data[,i])
  }
  
  #Date
  for (i in c(4,20,24,27,32,34)){
    data[,i] <- as.Date(data[,i])
  }
  
  
  #Chr UTF-8
  for (i in c(5:8,13:19,25,26,28:31,33,35:37)) {
    data[,i] <- iconv(data[,i],from = "latin1" , to="UTF-8")
  }
  
  
  
  
  
  #Suppression des données invalides sur le coordonnées dhfcg
  data <- data[!is.na(data$X),]
  data <- data[!is.na(data$Y),]
  

  
  #remplis la colonne du stade avec la valeur "jeune" si age_estim=0
  data$fk_stadedev <- ifelse(data$age_estim == 0, "jeune", data$fk_stadedev)
  

  
  #age estime
  data$age_estim[is.na(data$age_estim)] = round(mean(data$age_estim < 500, na.rm=TRUE))
  
  #Supprimer les lignes où age_estim>250 => valeur 2010 abérante
  data <- data[age_arbre(data$age_estim),]
  
  
  #As factor
  data$remarquable = as.factor(data$remarquable)
  data$clc_quartier = as.factor(data$clc_quartier)
  data$clc_secteur = as.factor(data$clc_secteur)
  
  # length(data[,1])
  return(data)
  
}

age_arbre = function(arbres){
  if(arbres >= 250){return(FALSE)}
  else{return(TRUE)}
}


install.packages("ggplot2")
library(ggplot2)



data = traitement(read.csv("Patrimoine_Arbore_modif.csv", dec='.',sep=','))
View(data)




data$clc_quartier = iconv(data$clc_quartier,from = "latin1" , to="UTF-8")
data$clc_secteur = iconv(data$clc_secteur,from = "latin1" , to="UTF-8")
data$fk_arb_etat = iconv(data$fk_arb_etat,from = "latin1" , to="UTF-8")
data$commentaire_environnement = iconv(data$commentaire_environnement,from = "latin1" , to="UTF-8")


data$X <- as.numeric(data$X)
data$Y <- as.numeric(data$Y)
data$OBJECTID <- as.numeric(data$OBJECTID)
data$created_date <- as.Date(data$created_date)

names = colnames(data)






freq_categorielle = function(tab){
  tab = data.frame(tab)
  len = length(summary(tab))
  
  
  total = sum(summary(tab))
  # freq = c(summary(tab)) / total
  freq = table(tab)
  
  ggplot(freq)
}

freq_categorielle(data$remarquable)


summary(data)
head(data)
data[1,1]
