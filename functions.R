#   _____                _   _                         _ _ _    __   _ 
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  / |
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \ | |
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/ | |
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |_|



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
    library("ggplot2")
  moyenne_ages <- c(rep(NA,length(levels(data$clc_quartier))))
  for(i in list(levels(data$clc_quartier))){
    moyenne_ages[i] <- mean(data$age_estim[data$clc_quartier == i])
  }
  data_summary <- data.frame(clc_quartier, moyenne_ages)

  # Représentation graphique
  ggplot(data_summary, aes(x = clc_quartier, y = age_moyen, fill = clc_quartier)) +
    geom_bar(stat = "identity") + #mets des couleurs différentes pour chaque barre
    labs(title = "Âges moyens des arbres par quartier",
      x = "Quartiers",
      y = "Âges moyens estimés") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = FALSE) #supprime la légende pour les couleurs des quartiers
}
 mean_age_quartier(data)




#   _____                _   _                         _ _ _    __   _____ 
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  |___ / 
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \   |_ \ 
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/  ___) |
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |____/ 




#   _____                _   _                         _ _ _    __   _  _   
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  | || |  
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \ | || |_ 
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/ |__   _|
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___|    |_|  






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



