source("Script.R")

install.packages("sf")
library(sf)

data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec='.',sep=','))

