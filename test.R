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

data_feuillage <- data[data$feuillage != "inconnu",]
data_feuillage$feuillage <- droplevels(data_feuillage$feuillage)

bar_plot <- ggplot(data_feuillage, aes(x = feuillage)) +
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
data_situation <- data[data$fk_situation != "inconnu",]
data_situation$fk_situation <- droplevels(data_situation$fk_situation)

pie_chart_situation <- ggplot(data_situation, aes(x = "", fill = fk_situation)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Répartition des arbres par situation") +
  theme_void()  # Supprimer les axes
print(pie_chart_situation)


#création d'un histogramme de la quantité d'arbres en fonction du quartier
data_quartiers <- data[data$clc_quartier != "inconnu",]
data_quartiers$clc_quartier <- droplevels(data_quartier$clc_quartier)
histogram_quartier <- ggplot(data_quartiers, aes(x = clc_quartier)) +
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


#test chi2 pour corrélation feuillage (feuillu, conifère) et le nom de l'arbre
table_filtree_feuillage<-data[data$feuillage != 'inconnu', ] #enleve toutes les données inconnues pour exécuter notre chi2
contingency_table_feuillage <- table(table_filtree_feuillage$feuillage, table_filtree_feuillage$nomlatin)
print(contingency_table_feuillage)

testchi2 <- chisq.test(table_filtree_feuillage$feuillage, table_filtree_feuillage$nomlatin)
testchi2

#fonctionnalité 5 -

#fonction qui renvoit le nom des quartiers où le nombre d'arbres est inférieur à la médiane
find_quartier_with_least_trees <- function(data) {
  nb_arbres <- table(data$clc_quartier) # Calculer le nombre d'arbres par quartier
  df_nb_arbres <- as.data.frame(nb_arbres) 
  colnames(df_nb_arbres) <- c("Quartier", "Nombre_Arbres")
  median_nb_arbres <- median(df_nb_arbres$Nombre_Arbres)
  quartiers_below_median <- df_nb_arbres[df_nb_arbres$Nombre_Arbres < median_nb_arbres, "Quartier"]
  return(quartiers_below_median)
}
data_quartier <- data[data$clc_quartier != "inconnu",]
data_quartier$clc_quartier <- droplevels(data_quartier$clc_quartier)
quartier_moins_arbres <- find_quartier_with_least_trees(data_quartier)
print(quartier_moins_arbres)
#print(length(data$clc_quartier))
#print(length(data_quartier$clc_quartier))


# Régression pour savoir dans quelle zone il faut planter pour harmoniser le développement global de la ville
data_zone <- data[data$clc_quartier != "inconnu", ]
data_zone$clc_quartier <- droplevels(data_zone$clc_quartier)

nb_arbres_zone <- table(data_zone$clc_quartier) # Calcule le nombre d'arbres par quartier
df_nb_arbres <- as.data.frame(nb_arbres_zone)
colnames(df_nb_arbres) <- c("quartier", "nb_arbres")
mediane_arbres <- median(df_nb_arbres$nb_arbres) # Calcule la médiane du nombre d'arbres par quartier

# variable binaire : 1 si le nombre d'arbres est en dessous de la médiane, sinon 0:
df_nb_arbres$planter <- ifelse(df_nb_arbres$nb_arbres < mediane_arbres, 1, 0)

model_logistic <- glm(planter ~ quartier, data = df_nb_arbres, family = binomial) # Régression logistique
summary(model_logistic)

resultats <- list()

for (quartier in df_nb_arbres$quartier) {
  #quartier_ou_planter <- data.frame(quartier = "Quartier de l'Europe") # Régression logistique
  #resultat_predit <- predict(model_logistic, quartier_ou_planter, type = "response")
  #resultat_predit_binaire <- ifelse(resultat_predit > 0.5, "oui", "non")
  #print(resultat_predit_binaire)

  #créer une nouvelle colonne  dans la data frame qui sélectionne la colonne "nb d'arbres" pour les lignes correspondant au quartier égal au quartier actuel de la boucle
  quartier_ou_planter <- data.frame(nb_arbres = df_nb_arbres[df_nb_arbres$quartier == quartier, "nb_arbres"])  
  resultat_predit <- predict(model_logistic, quartier_ou_planter, type = "response")
  resultat_predit_binaire <- ifelse(resultat_predit > 0.5, "oui", "non")
  resultats[[quartier]] <- resultat_predit_binaire
}

print(resultats)


