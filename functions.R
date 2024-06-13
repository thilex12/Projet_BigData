#   _____                _   _                         _ _ _    __   _ 
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  / |
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \ | |
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/ | |
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |_|

#Traitement de la base de données
source("traitement.R")

#Graph sur la frequences des varaible qualitatives (ou quantitatives)

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



#Distribution des types de feuillage par quartier

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







#   _____                _   _                         _ _ _    __   ____  
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  |___ \ 
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \   __) |
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/  / __/ 
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |_____|




# Calculer l'âge moyen des arbres par quartier
mean_age_quartier <- function(data){
  moyenne_ages <- numeric(length(levels(data$clc_quartier)))
  # Parcourir chaque niveau de clc_quartier et calculer la moyenne d'âge
  for(i in seq_along(levels(data$clc_quartier))) {
    niveau <- levels(data$clc_quartier)[i]
    moyenne_ages[i] <- mean(data$age_estim[data$clc_quartier == niveau], na.rm = TRUE)
  }
  data_summary <- data.frame(clc_quartier = levels(data$clc_quartier), moyenne_ages = moyenne_ages)
  
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


#Situations des arbres en fonction de leur quartier
situa_quartier <- function(data){
  ggplot(data, aes(x = clc_quartier, fill = fk_situation)) +
    geom_bar(position = "dodge") + 
    labs(title = "Situations des arbres en fonction de leur quartier",
         x = "Quartiers",
         y = "Nombre d'arbres") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
}




#   _____                _   _                         _ _ _    __   _____ 
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  |___ / 
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \   |_ \ 
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/  ___) |
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |____/ 


source("map.R")



#   _____                _   _                         _ _ _    __   _  _   
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  | || |  
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \ | || |_ 
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/ |__   _|
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___|    |_|  


#test chi2 pour corrélation entre fk_arb_etat et fk_pied

ki2_cor_etat_pied <- function(data){
  table_filtree<-data[data$fk_pied != 'inconnu', ] #enleve toutes les données inconnues pour exécuter notre chi2
  contingency_table <- table(table_filtree$fk_pied, table_filtree$fk_arb_etat)
  print(contingency_table)
  
  testchi2 <- chisq.test(table_filtree$fk_pied, table_filtree$fk_arb_etat)
  testchi2
}

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

#test chi2 pour corrélation entre le quartier et le feuillage

kie2_cor_quartier_feuillage <- function(data){
  table_filtree2<-data[data$feuillage != 'inconnu', ] #enleve toutes les données inconnues pour exécuter notre chi2
  contingency_table2 <- table(table_filtree2$clc_quartier, table_filtree2$feuillage)
  print(contingency_table2)

  testchi2_2 <- chisq.test(data$clc_quartier, data$feuillage)
  testchi2_2
}

#mosaicplot du premier test chi2

mosaic_kie2_quartier_feuillage <- function(data){
    table_filtree2<-data[data$feuillage != 'inconnu', ] #enleve toutes les données inconnues pour exécuter notre chi2
    contingency_table2 <- table(table_filtree2$clc_quartier, table_filtree2$feuillage)
    mosaicplot(contingency_table2, main = "Mosaicplot des feuillages en fonction des quartiers",
                 xlab = "Quartiers (clc_quartier)", ylab = "Feuillages (feuillage) ",
                 color = rainbow(length(unique(data$fk_arb_etat))),
                 las=2)
}





#   _____                _   _                         _ _ _    __   ____  
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  | ___| 
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \ |___ \ 
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/  ___) |
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |____/ 



#Liste des arbres à abattre

lst_abattre <- function(data){
  model <- glm(data$fk_arb_etat != "EN PLACE" ~ data$age + data$fk_stadedev + data$remarquable, family="binomial")
  # summary(model)
  resultat <- predict(model, data.frame(data),type="response")
  # plot(resultat)


  data$abattre <- FALSE
  data$abattre[resultat > 0.6] <- TRUE

  return(data)
}