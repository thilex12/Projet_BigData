test <- c(1:10)
#esrdghjk

#okokok

#blablabla
#library(readxl)
open(encoding='UTF-8')
data <- read.csv("Patrimoine_Arbore.csv", dec='.',sep=',')


data$X

#Numérique
for (i in c(1,2,3,9:12,21:23)) {
  data[,i] <- as.numeric(data[,i])
}

#Date
for (i in c('dte_abattage','EditDate','last_edited_date','created_date','CreationDate')){
  data[,i] <- as.Date(data[,i])
}


#Chr UTF-8
for (i in c(5:8,13:19,25,26,28:31,33,35:37)) {
  data[,i] = iconv(data[,i],from = "latin1" , to="UTF-8")
}



#Suppression des donnée invalide sur le coordonnées
data <- data[!is.na(data$X),]
data <- data[!is.na(data$Y),]




length(data[,1])

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






summary(data)
head(data)
data[1,1]
