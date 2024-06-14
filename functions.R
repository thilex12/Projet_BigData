#   _____                _   _                         _ _ _    __   _ 
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  / |
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \ | |
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/ | |
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |_|

#Traitement de la base de données
source("traitement.R")
install.packages("ggplot2")
library(ggplot2)

#Alexandre
#Graph sur la frequence des variables qualitatives (ou quantitatives)

freq_categorielle <- function(tab){
  
  tab <- data[[tab]]
  my_plto <- ggplot(data, aes(tab)) +
    geom_bar( fill = "blue", position="dodge") +
    labs(binwidth = 1, title = "Fréquence des catégories",
         x = "Catégories",
         y = "Fréquence") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  my_plto
}


#Eléa
#Boxplot de la hauteur totale des arbres par quartier

box_haut_tot_quartier <- function(data){
  data_quartier<-data[data$clc_quartier!="inconnu",] #ne prend pas en compte les valeurs "inconnu"
  data_quartier$clc_quartier<-droplevels(data_quartier$clc_quartier)

  ggplot(data_quartier, aes(x = clc_quartier, y = haut_tot)) +
    geom_boxplot() + #pour les boites à moutache
    labs(title = "Boxplot de la hauteur totale des arbres par quartier",
         x = "Quartiers",
         y = "Hauteurs Totales") +
    theme_minimal() + #pour enlever les carrés gris en arrière plan
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) #pour que nos titres de quartiers ne se superposent pas
}
#box_haut_tot_quartier(data)


#Eléa
#Distribution des types de feuillages par quartier

feuille_par_quartier <- function(data){
  data_feuillage<-data[data$feuillage!="inconnu",]
  data_feuillage$feuillage<-droplevels(data_feuillage$feuillage)

  ggplot(data_feuillage, aes(x = clc_quartier, fill = feuillage)) +
    geom_bar(position = "dodge") +
    labs(title = "Distribution des types de feuillage par quartier",
         x = "Quartiers",
         y = "Nombre d'arbres",
         fill = "Types de feuillage") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
# feuille_par_quartier(data)



#Carla
#Histogramme de la hauteur totale en mètres

hist_haut_tot <- function(data){
  histogram <- ggplot(data, aes(x = haut_tot)) +
    geom_histogram(binwidth = 1,fill = "red") +
    labs(title = "Histogramme de la hauteur totale",
         x = "Hauteur totale",
         y = "Fréquence") +
    theme_minimal()

  histogram
}
# hist_haut_tot(data)



#Carla
#box-plot de la hauteur du tronc

#on enlève les valeurs de tronc_diam qui sont égales à 0 car elles ne sont pas renseignées
boxplot_haut_tronc <- function(data){  
  boxplot_diam_tronc <- ggplot(data[data$tronc_diam != 0, ], aes(y = data$tronc_diam[data$tronc_diam != 0])) +
  geom_boxplot(fill = "green") +
  labs(title = "Boxplot du diamètre du tronc",
       y = "Diamètre du tronc") +
  theme_minimal()
  boxplot_diam_tronc
}

# boxplot_haut_tronc(data)


#   _____                _   _                         _ _ _    __   ____  
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  |___ \ 
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \   __) |
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/  / __/ 
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |_____|



#Eléa
# Calculer l'âge moyen des arbres par quartier

mean_age_quartier <- function(data){
  moyenne_ages <- numeric(length(levels(data$clc_quartier)))
  # Parcourir chaque niveau de clc_quartier et calculer la moyenne d'âge
  for(i in seq_along(levels(data$clc_quartier))) {
    niveau <- levels(data$clc_quartier)[i]
    moyenne_ages[i] <- mean(data$age_estim[data$clc_quartier == niveau], na.rm = TRUE)
  }
  data_summary <- data.frame(clc_quartier = levels(data$clc_quartier), moyenne_ages = moyenne_ages)
  
  #Eléa
  # Représentation graphique
  ggplot(data_summary, aes(x = clc_quartier, y = moyenne_ages, fill = clc_quartier)) +
  geom_bar(stat = "identity") + #mets des couleurs différentes pour chaque barre
  labs(title = "Âges moyens des arbres par quartier",
    x = "Quartiers",
    y = "Âges moyens estimés") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE) #supprime la légende pour les couleurs des quartiers
}
# mean_age_quartier(data)



#Eléa
#Situations des arbres en fonction de leur quartier

situa_quartier <- function(data){
  data_situa_quart<-data[data$fk_situation!="inconnu",]
  data_situa_quart$fk_situation<-droplevels(data_situa_quart$fk_situation)
  
  
  ggplot(data_situa_quart, aes(x = clc_quartier, fill = fk_situation)) +
    geom_bar(position = "dodge") + 
    labs(title = "Situations des arbres en fonction de leur quartier",
         x = "Quartiers",
         y = "Nombre d'arbres") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
}
#situa_quartier(data)


#Carla
# Création d'un diagramme circulaire de la répartition des arbres par stade de développement

pie_stade_dev <- function(data){
  data_stadedev <- data[data$fk_stadedev != "inconnu",]
  data_stadedev$fk_stadedev <- droplevels(data_stadedev$fk_stadedev)
  
  pie_chart_stade_dev <- ggplot(data_stadedev, aes(x = "", fill = fk_stadedev)) +
    geom_bar(width = 1) +
    coord_polar(theta = "y") +
    labs(title = "Répartition des arbres par stade de développement") +
    theme_void()  
  print(pie_chart_stade_dev)
}
# pie_stade_dev(data)



#Carla
# Création d'un diagramme circulaire  de la répartition des arbres par situation

pie_repartition_situation <- function(data){
  data_situation <- data[data$fk_situation != "inconnu",]
  data_situation$fk_situation <- droplevels(data_situation$fk_situation)

  pie_chart_situation <- ggplot(data_situation, aes(x = "", fill = fk_situation)) +
    geom_bar(width = 1) +
    coord_polar(theta = "y") +
    labs(title = "Répartition des arbres par situation") +
    theme_void()  # Supprimer les axes
  print(pie_chart_situation)
}
# pie_repartition_situation(data)



#Carla
#création d'un histogramme de la quantité d'arbres en fonction du quartier

histogram_quartier_fct <- function(data){
  data_quartiers <- data[data$clc_quartier != "inconnu",]
  data_quartiers$clc_quartier <- droplevels(data_quartiers$clc_quartier)
  histogram_quartier <- ggplot(data_quartiers, aes(x = clc_quartier)) +
    geom_bar(fill = "blue") +
    labs(title = "Quantité d'arbres par quartier",
         x = "Quartier",
         y = "Quantité d'arbres") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(histogram_quartier)
}
# histogram_quartier_fct(data)


#   _____                _   _                         _ _ _    __   _____
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  |___ /
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \   |_ \
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/  ___) |
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |____/

source("map.R")
#Alexandre


#   _____                _   _                         _ _ _    __   _  _   
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  | || |  
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \ | || |_ 
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/ |__   _|
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___|    |_|  


#Carla
#test chi2 pour corrélation feuillage (feuillu, conifère) et le nom de l'arbre

kie2_cor_feuillage_nom <- function(data){
  table_filtree_feuillage<-data[data$feuillage != 'inconnu', ] #enleve toutes les données inconnues pour exécuter notre chi2
  contingency_table_feuillage <- table(table_filtree_feuillage$feuillage, table_filtree_feuillage$nomlatin)
  print(contingency_table_feuillage)

  testchi2 <- chisq.test(table_filtree_feuillage$feuillage, table_filtree_feuillage$nomlatin)
  testchi2
}
# kie2_cor_feuillage_nom(data)


#Eléa
#1er test chi2, pour corrélation entre fk_arb_etat et fk_pied

ki2_cor_etat_pied <- function(data){
  table_filtree<-data[data$fk_pied != 'inconnu', ] #enleve toutes les données inconnues pour exécuter notre chi2
  contingency_table <- table(table_filtree$fk_pied, table_filtree$fk_arb_etat)
  print(contingency_table)
  
  testchi2 <- chisq.test(table_filtree$fk_pied, table_filtree$fk_arb_etat)
  testchi2
}
#ki2_cor_etat_pied(data)


#Eléa
#mosaicplot du premier test chi2

mosaic_kie2_etat_pied <- function(data){
  table_filtree<-data[data$fk_pied != 'inconnu', ] #enleve toutes les données inconnues pour exécuter notre chi2
  contingency_table <- table(table_filtree$fk_pied, table_filtree$fk_arb_etat)
  mosaicplot(contingency_table, main = "Mosaicplot des états des arbres en fonction du type de sol",
             xlab = "Type de sol (fk_pied)", ylab = "État de l'arbre (fk_arb_etat)",
             color = rainbow(length(unique(data$fk_arb_etat))),
             las=2)#place correctement les labels horizontalement/verticalement 
}

# mosaic_kie2_etat_pied(data)


#Eléa
#2eme test chi2, pour corrélation entre le quartier et le feuillage

kie2_cor_quartier_feuillage <- function(data){
  table_filtree2<-data[data$feuillage != 'inconnu', ] #enleve toutes les données inconnues pour exécuter notre chi2
  contingency_table2 <- table(table_filtree2$clc_quartier, table_filtree2$feuillage)
  print(contingency_table2)

  testchi2_2 <- chisq.test(data$clc_quartier, data$feuillage)
  testchi2_2
}
#kie2_cor_quartier_feuillage(data)

#Eléa
#mosaicplot du deuxieme test chi2
mosaic_kie2_quartier_feuillage <- function(data){
    table_filtree2<-data[data$feuillage != 'inconnu', ] #enleve toutes les données inconnues pour exécuter notre chi2
    contingency_table2 <- table(table_filtree2$clc_quartier, table_filtree2$feuillage)
    mosaicplot(contingency_table2, main = "Mosaicplot des feuillages en fonction des quartiers",
                 xlab = "Quartiers (clc_quartier)", ylab = "Feuillages (feuillage) ",
                 color = rainbow(length(unique(data$fk_arb_etat))),
                 las=2)
}
#mosaic_kie2_quartier_feuillage(data)


#Carla
#corrélation age et diamètre tronc de l'arbre

cor_age_diam <- function(data){
  filtered_data_tronc_diam <- data[data$tronc_diam != 0, ]
  correlation_value_tronc_diam <- cor(filtered_data_tronc_diam$age_estim, filtered_data_tronc_diam$tronc_diam, use = "complete.obs")
  print(correlation_value_tronc_diam)
  correlation_matrix_tronc_diam <- cor(filtered_data_tronc_diam[c("age_estim", "tronc_diam")], use = "complete.obs")
  print(correlation_matrix_tronc_diam)
}
# cor_age_diam(data)




#Carla
#corrélation age et hauteur totale de l'arbre

cor_age_haut <- function(data){
  filtered_data_haut_tot <- data[data$haut_tot != 0, ]
  correlation_value_haut_tot <- cor(filtered_data_haut_tot$age_estim, filtered_data_haut_tot$haut_tot, use = "complete.obs")
  print(correlation_value_haut_tot)
  correlation_matrix_haut_tot <- cor(filtered_data_haut_tot[c("age_estim", "haut_tot")], use = "complete.obs")
  print(correlation_matrix_haut_tot)
}
# cor_age_haut(data)


#   _____                _   _                         _ _ _    __   ____  
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  | ___| 
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \ |___ \ 
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/  ___) |
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |____/ 


#Eléa
#Liste des arbres à abattre

lst_abattre <- function(data){
  model <- glm(data$fk_arb_etat != "EN PLACE" ~ data$age_estim + data$fk_prec_estim + data$fk_stadedev + data$remarquable, family="binomial")
  # summary(model)
  resultat <- predict(model, data.frame(data),type="response")
  # plot(resultat)


  data$abattre <- FALSE
  data$abattre[resultat > 0.6 & data$remarquable == "Non"] <- TRUE

  return(data)
}


#Carla
#fonction qui renvoie le nom des quartiers où le nombre d'arbres est inférieur à la médiane

quartier_sous_mediane <- function(data){
  find_quartier_with_least_trees <- function(data) {
    nb_arbres <- table(data$clc_quartier) # Calculer le nombre d'arbres par quartier
    df_nb_arbres <- as.data.frame(nb_arbres) 
    colnames(df_nb_arbres) <- c("Quartier", "Nombre_Arbres")
    median_nb_arbres <- median(df_nb_arbres$Nombre_Arbres)
    quartiers_below_median <- df_nb_arbres[df_nb_arbres$Nombre_Arbres < median_nb_arbres, "Quartier"]
    return(quartiers_below_median)
  }
  data_quartier <- data[data$clc_quartier != "inconnu", ]
  data_quartier$clc_quartier <- droplevels(data_quartier$clc_quartier)
  quartier_moins_arbres <- find_quartier_with_least_trees(data_quartier)
  print(quartier_moins_arbres)
}
# quartier_sous_mediane(data)




#Carla
# Régression pour savoir dans quelle zone il faut planter pour harmoniser le développement global de la ville

quartier_replanter <- function(data){
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
}

# quartier_replanter(data)




# Prediction de l'age des arbres (-- deja inclus dans le traitement --)

# source("prediction_age.R")
# calcul_clear_age(data)





#  ------------------ Zone à replanter par quartier (Mathilde et Alexandre) ------------------ #


  ##-- Régression logistique zone à planter --##


replanter_alex_mathilde <- function(data){

  # Focntion calculant la densité
  densite_val <- function(data, quartier){
    tableau <- data[data$clc_quartier == quartier, ]
    longueur <- max(tableau$X) - min(tableau$X)
    largueur <- max(tableau$Y) - min(tableau$Y)

    superficie <- longueur * largueur
    taille <- length(tableau$X)
    densite<- (taille/superficie)*100

    return(densite)
  }

 

  # Création de la nouvelle data frame

  

  data$clc_quartier <- as.factor(data$clc_quartier)

  

  lst_quartiers <- levels(data$clc_quartier)

  lst_densites <- rep(0, length(lst_quartiers))

  for (i in 1:length(lst_quartiers)) {

    lst_densites[i] <- densite_val(data, lst_quartiers[i])

  }

  quartiers_data <- data.frame("quartiers" = levels(data$clc_quartier), "densite" = lst_densites)


  # Créer une nouvelle colonne

  quartiers_data$planter <- NA


  # Division de la base de données

  quartiers_train <- head(quartiers_data,8)
  quartiers_test <- tail(quartiers_data, 4)


  # Initialisation de la colonne "planter" dans la base d'entraînement

  quartiers_train$planter<-FALSE
  quartiers_train$planter[quartiers_train$densite <= 0.1 ] <- TRUE


  # Entraînement du modèle

  model_planter <- glm(planter ~ densite, data  = quartiers_train, family = "binomial")

  summary(model_planter)


  # Prédictions sur la base de test

  predictions <- predict(model_planter, type = "response", newdata = quartiers_test)
  plot(predictions)

  

  # Mise à jour des colonnes dans la base de données de test

  quartiers_test$predictions <- predictions
  quartiers_test$planter <- FALSE
  quartiers_test$planter[predictions>= 0.5] <- TRUE
}
# replanter_alex_mathilde(data)


# --- #


#   _____                _   _                         _ _ _    __    __   
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/   / /_  
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \ | '_ \ 
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/ | (_) |
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___|  \___/ 



export <- function(data){
  write.table(data, "data_export.csv", dec = ".", sep = ",", na = "NA", fileEncoding = "UTF-8")

}
# export(data)
