test <- c(1:10)
#esrdghjk

#okokok

#blablabla
#library(readxl)
open(encoding='UTF-8')
data <- read.csv("Patrimoine_Arbore.csv", dec='.',sep=',')




#Numérique
for (i in c(1,2,3,7,9:12,21:23)) {
  data[,i] <- as.numeric(data[,i])
  
}

#Date
for (i in c('dte_abattage','EditDate','last_edited_date','created_date','CreationDate')){
  data[,i] <- as.Date(data[,i])
}


#Chr UTF-8
for (i in c()) {
  data[,i] = iconv(data[,i],from = "latin1" , to="UTF-8")
}

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
