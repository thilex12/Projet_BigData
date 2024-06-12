source("Script.R")
data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec='.',sep=','))
View(data)

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
#on enlèvre les valeurs de tronc_dima qui sont égales à 0 car elles ne sont pas renseignées
boxplot_diam_tronc <- ggplot(data[data$tronc_diam != 0, ], aes(y = data$tronc_diam[data$tronc_diam != 0])) +
  geom_boxplot(fill = "green") +
  labs(title = "Boxplot du diamètre du tronc",
       y = "Diamètre du tronc") +
  theme_minimal()

print(boxplot_diam_tronc)

length(data$tronc_diam)
View(test)
test = data[data$tronc_diam != 0, ]
length(test$tronc_diam)
length(data$tronc_diam[data$tronc_diam!=0])

#fonctionnalité 2


# Création d'un camembert de la répartition des arbres par stade de développement
#data$stade_dev[] <- tolower(data$stade_dev) #mettre en minuscule
pie_chart_stade_dev <- ggplot(data, aes(x = "", fill = fk_stadedev)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Répartition des arbres par stade de développement") +
  theme_void()  
print(pie_chart_stade_dev)



# Création d'un camembert  de la répartition des arbres par situation
pie_chart_situation <- ggplot(data, aes(x = "", fill = fk_situation)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Répartition des arbres par situation") +
  theme_void()  # Supprimer les axes
print(pie_chart_situation)


#création d'un histogramme de la quantité d'arbres en fonction du quartier
histogram_quartier <- ggplot(data, aes(x = clc_quartier)) +
  geom_bar(fill = "blue") +
  labs(title = "Quantité d'arbres par quartier",
       x = "Quartier",
       y = "Quantité d'arbres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(histogram_quartier)

#fonctionnalité 4

#corrélation age et diamètre tronc de l'arbre
filtered_data_tronc_diam <- data[data$tronc_diam != 0, ]
correlation_value_tronc_diam <- cor(filtered_data_tronc_diam$age_estim, filtered_data_tronc_diam$tronc_diam, use = "complete.obs")
print(correlation_value_tronc_diam)
correlation_matrix_tronc_diam <- cor(filtered_data_tronc_diam[c("age_estim", "tronc_diam")], use = "complete.obs")
print(correlation_matrix_tronc_diam)

#corrélation age et hauteur totale de l'arbre
filtered_data_haut_tot <- data[data$haut_tot != 0, ]
correlation_value_haut_tot <- cor(filtered_data_haut_tot$age_estim, filtered_data_haut_tot$haut_tot, use = "complete.obs")
print(correlation_value_haut_tot)
correlation_matrix_haut_tot <- cor(filtered_data_haut_tot[c("age_estim", "haut_tot")], use = "complete.obs")
print(correlation_matrix_haut_tot)



#Conduire des analyses bivariées
# régression linéaire entre fk_situation et hauteur totale

data_filtered <- data[data$fk_situation != "inconnu", ]
lm_model_filtered <- lm(haut_tot ~ fk_situation, data = data_filtered)
summary(lm_model_filtered)