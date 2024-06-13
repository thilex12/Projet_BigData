source("traitement.R")

data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec = ".",sep = ","))


View(data)
