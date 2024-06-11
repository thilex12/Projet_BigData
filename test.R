
data <- read.csv("Patrimoine_Arbore.csv", dec='.',sep=',')

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

#

data <- data[data$age_estim<=250,]
data$fk_stadedev <- ifelse(data$age_estim == 0, "jeune", data$fk_stadedev)

# 
# a = as.Date(as.character(1), format = "%Y")
# a = as.numeric(a)
# a = as.Date(a)
# a = as.numeric(Sys.Date()) - as.numeric(a)
# a = as.numeric(a)
# a = as.Date(a)



# length(data[,1])

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


#fonctionnalité 1

#Histogramme de la hauteur totale avec mètre par mètre
install.packages("ggplot2")
library(ggplot2)
histogram <- ggplot(data, aes(x = haut_tot)) +
  geom_histogram(binwidth = 1,fill = "red") +
  labs(title = "Histogramme de la hauteur totale",
       x = "Hauteur totale",
       y = "Fréquence") +
  theme_minimal()

histogram

#Répartition des types de feuillage:
feuillage_freq <- table(data$feuillage)
feuillage_freq

bar_plot <- ggplot(data, aes(x = feuillage)) +
  geom_bar(binwidth = 1, fill = "blue") +
  labs(title = "Répartition des types de feuillage",
       x = "Type de feuillage",
       y = "Fréquence") +
  theme_minimal()
print(bar_plot)


#box-plot de la hauteur du tronc
boxplot_diam_tronc <- ggplot(data, aes(y = tronc_diam)) +
  geom_boxplot(fill = "green") +
  labs(title = "Boxplot du diamètre du tronc",
       y = "Diamètre du tronc") +
  theme_minimal()

print(boxplot_diam_tronc)

#fonctionnalité 2


# Création d'un camembert de la répartition des arbres par stade de développement
pie_chart_stade_dev <- ggplot(data, aes(x = "", fill = fk_stadedev)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Répartition des arbres par stade de développement") +
  theme_void()  # Supprimer les axes
print(pie_chart_stade_dev)



# Création d'un camembert  de la répartition des arbres par situation
pie_chart_situation <- ggplot(data, aes(x = "", fill = fk_situation)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Répartition des arbres par situation") +
  theme_void()  # Supprimer les axes
print(pie_chart_situation)

