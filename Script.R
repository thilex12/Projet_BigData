
data <- read.csv("Patrimoine_Arbore.csv", dec='.',sep=',')


traitement = function(data){
  
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
  
  #Supprimer les lignes où age_estim>250 => valeur 2010 abérante
  data <- data[data$age_estim<=250,]
  
  #remplis la colonne du stade avec la valeur "jeune" si age_estim=0
  data$fk_stadedev <- ifelse(data$age_estim == 0, "jeune", data$fk_stadedev)
  

  
  
  
  #As factor
  data$remarquable = as.factor(data$remarquable)
  
  # length(data[,1])
  return(data)
  
}



data = traitement(read.csv("Patrimoine_Arbore.csv", dec='.',sep=','))
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


# total = sum(summary(data$remarquable))
total2 = sum(summary(data$remarquable)[2:4])
freq = c(summary(data$remarquable)[2:4])/total2

plot(freq)




freq_categorielle = function(tab){
  len = length(summary(tab)) - 1
  
  total = sum(summary(tab)[1:len])
  freq = c(summary(tab)[1:len]) / total
  
  hist(freq)
}

freq_categorielle(data$remarquable)


summary(data)
head(data)
data[1,1]
