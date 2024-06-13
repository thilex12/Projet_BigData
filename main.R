source("./fichiers_R/functions.R")



#   _____                _   _                         _ _ _    __   _ 
#  |  ___|__  _ __   ___| |_(_) ___  _ __  _ __   __ _| (_) |_ /_/  / |
#  | |_ / _ \| '_ \ / __| __| |/ _ \| '_ \| '_ \ / _` | | | __/ _ \ | |
#  |  _| (_) | | | | (__| |_| | (_) | | | | | | | (_| | | | ||  __/ | |
#  |_|  \___/|_| |_|\___|\__|_|\___/|_| |_|_| |_|\__,_|_|_|\__\___| |_|

#Traitement du la base de données
#Utilistion d'un .csv modifier avec suppréssion des ` " ` incoherent
data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec = ".",sep = ","))
