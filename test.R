open(encoding='UTF-8')
data <- read.csv("Patrimoine_Arbore.csv")
data$clc_quartier = iconv(data$clc_quartier,from = "latin1" , to="UTF-8")
data$clc_secteur = iconv(data$clc_secteur,from = "latin1" , to="UTF-8")
data$fk_arb_etat = iconv(data$fk_arb_etat,from = "latin1" , to="UTF-8")
data$commentaire_environnement = iconv(data$commentaire_environnement,from = "latin1" , to="UTF-8")

for (col in data) {
  data$col = iconv(col,from = "latin1" , to="UTF-8")
  
}

for (lig in  data){
  data$X[is.na(data$X)]
  data$X[is.na(data$X)]}
data$villeca[] <- data$villeca["Saint-Quentin"]

view(data)