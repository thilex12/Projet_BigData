test <- c(1:10)
#esrdghjk

#okokok

<<<<<<< HEAD
#blablablabla
=======
#blablabla
#library(readxl)
open(encoding='UTF-8')
<<<<<<< HEAD
data <- read.csv("Patrimoine_Arbore.csv", locale = locale(encoding = "UTF-8"))
=======
data <- read.csv("Patrimoine_Arbore.csv", dec='.',sep=',')

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

>>>>>>> c30291e0267deba02f24a85b4d79cad64d785cd2
data$clc_quartier = iconv(data$clc_quartier,from = "latin1" , to="UTF-8")
data$clc_secteur = iconv(data$clc_secteur,from = "latin1" , to="UTF-8")
data$fk_arb_etat = iconv(data$fk_arb_etat,from = "latin1" , to="UTF-8")
data$commentaire_environnement = iconv(data$commentaire_environnement,from = "latin1" , to="UTF-8")

for (col in data) {
  data$col = iconv(col,from = "latin1" , to="UTF-8")
}

View(data)

summary(data)
>>>>>>> 24a48e04ea2a1f7af904363ed10eccdf4eab4fef
