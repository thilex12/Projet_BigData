source("functions.R")



#   _____                _   _                         _ _ _    __   _ 
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  / |
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \ | |
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/ | |
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |_|

#Traitement du la base de données
#Utilistion d'un .csv modifier avec suppréssion des ` " ` incoherent
data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec = ".",sep = ","))
# View(data)


#Boxplot de la hauteur totale des arbres par quartier

# box_haut_tot_quartier(data)


#Distribution des types de feuillage par quartier

# feuille_par_quartier(data)



#   _____                _   _                         _ _ _    __   ____  
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  |___ \ 
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \   __) |
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/  / __/ 
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |_____|


# Calculer l'âge moyen des arbres par quartier
# mean_age_quartier(data)


#Situations des arbres en fonction de leur quartier
# situa_quartier(data)




#   _____                _   _                         _ _ _    __   _____ 
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  |___ / 
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \   |_ \ 
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/  ___) |
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |____/ 


#Carte des arbres et differents layers d'information à sélectionner en haut à droite (1 à 1 pour ne pas avoir de supperposition de couleurs)
# map_arbre(data)

#Maps individuelles (deja comprisent dans map_arbres)
# map_arbre_quartier(data)
# map_arbre_etat(data)
# map_arbre_stadedev(data)



#   _____                _   _                         _ _ _    __   _  _   
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  | || |  
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \ | || |_ 
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/ |__   _|
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___|    |_|  



#test chi2 pour corrélation entre fk_arb_etat et fk_pied

ki2_cor_etat_pied(data)


#mosaicplot du premier test chi2

mosaic_kie2_etat_pied(data)



#test chi2 pour corrélation entre le quartier et le feuillage

# kie2_cor_quartier_feuillage(data)

#mosaicplot du premier test chi2

# mosaic_kie2_quartier_feuillage(data)



#   _____                _   _                         _ _ _    __   ____  
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  | ___| 
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \ |___ \ 
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/  ___) |
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |____/ 



#Liste des arbres à abattre -> nouvelle colonne dans la base de données

# data <- lst_abattre(data)
# View(data$abattre)
# View(data)
