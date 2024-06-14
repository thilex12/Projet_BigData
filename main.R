source("functions.R")



#   _____                _   _                         _ _ _    __   _ 
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  / |
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \ | |
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/ | |
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |_|

#Traitement du la base de données
#Utilistion d'un .csv modifier avec suppréssion des ` " ` incoherent
data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec = ".", sep = ","))
# View(data)

#Toute les autres fonctions se basent sur ce data traité, si des résultat sont incohérent essayé de refaire le traitement de la base de données avec la ligne ci-dessus


#Boxplot de la hauteur totale des arbres par quartier

# box_haut_tot_quartier(data)



#Distribution des types de feuillage par quartier

# feuille_par_quartier(data)



#Histogramme de la hauteur totale avec mètre par mètre

# hist_haut_tot(data)



#box-plot de la hauteur du tronc
#on enlèvre les valeurs de tronc_dima qui sont égales à 0 car elles ne sont pas renseignées

# boxplot_haut_tronc(data)




#   _____                _   _                         _ _ _    __   ____  
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  |___ \ 
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \   __) |
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/  / __/ 
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |_____|


# Calculer l'âge moyen des arbres par quartier
# mean_age_quartier(data)


#Situations des arbres en fonction de leur quartier
# situa_quartier(data)



# Création d'un camembert de la répartition des arbres par stade de développement

# pie_stade_dev(data)



# Création d'un camembert  de la répartition des arbres par situation

# pie_repartition_situation(data)



#création d'un histogramme de la quantité d'arbres en fonction du quartier

# histogram_quartier_fct(data)



#   _____                _   _                         _ _ _    __   _____ 
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  |___ / 
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \   |_ \ 
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/  ___) |
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |____/ 


#Carte des arbres et differents layers d'information à sélectionner en haut à droite (1 à 1 pour ne pas avoir de supperposition de couleurs)
# La creations des cartes peut prendre du temps (environ 1min max)
# map_arbre(data)


# Version web de la carte -> creation .htlm et dossier de fichier temp pour faire fonctionner la version web
# Pour faire fonctoinner, lancer sur un server (local par exemple : "Live Preview" sur VS Code en extension)
# De base tout les overlay sont selectionné, veillez à ne garder qu'un seul overlay d'ativer à la fois pour ne pa superposer les couleurs et multiplier le nombres de points sur la carte
# map_web(map_arbre(data))



#Maps individuelles (deja comprisent dans map_arbres)
# map_arbre_quartier(data)
# map_arbre_etat(data)
# map_arbre_stadedev(data)







#   _____                _   _                         _ _ _    __   _  _   
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  | || |  
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \ | || |_ 
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/ |__   _|
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___|    |_|  


#test chi2 pour corrélation feuillage (feuillu, conifère) et le nom de l'arbre

# kie2_cor_feuillage_nom(data)




#test chi2 pour corrélation entre fk_arb_etat et fk_pied

# ki2_cor_etat_pied(data)


#mosaicplot du premier test chi2

# mosaic_kie2_etat_pied(data)




#test chi2 pour corrélation entre le quartier et le feuillage

# kie2_cor_quartier_feuillage(data)

#mosaicplot du premier test chi2

# mosaic_kie2_quartier_feuillage(data)




#corrélation age et diamètre tronc de l'arbre

# cor_age_diam(data)




#corrélation age et hauteur totale de l'arbre

# cor_age_haut(data)




#   _____                _   _                         _ _ _    __   ____  
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  | ___| 
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \ |___ \ 
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/  ___) |
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |____/ 



#Liste des arbres à abattre -> nouvelle colonne dans la base de données

# data <- lst_abattre(data)
# View(data$abattre)
# View(data)


#fonction qui renvoit le nom des quartiers où le nombre d'arbres est inférieur à la médiane

# quartier_sous_mediane(data)



# Régression pour savoir dans quelle zone il faut planter pour harmoniser le développement global de la ville

# quartier_replanter(data)



# Prediction de l'age des arbres (-- deja inclus dans le traitement --)

# source("prediction_age.R")
# calcul_clear_age(data)



# Prediciotn quartier où il faut planter (Alexandre et Mathilde)

# replanter_alex_mathilde(data)


#   _____                _   _                         _ _ _    __    __   
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/   / /_  
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \ | '_ \ 
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/ | (_) |
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___|  \___/ 


# Exportation du fichier traité, passé en paramètre, en .csv

# export(data)