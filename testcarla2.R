source("Script.R")
data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec='.',sep=','))


# Fonction pour remplacer les valeurs 0 par les valeurs prédites
replace_zeros_with_predictions <- function(data, target_column, predictor_column) {
  non_zero_data <- data[data[[target_column]] != 0, ] # Filtrer les lignes où target_column est différent de 0
  #Regression
  lm_model <- lm(non_zero_data[[target_column]] ~ non_zero_data[[predictor_column]]) # Ajuster le modèle de régression linéaire
  zero_data <- data[data[[target_column]] == 0, ] # Prédire les valeurs pour les lignes où target_column est 0
  #Prediction
  #predicted_values <- predict(lm_model, list(zero_data[[target_column]])) # Remplacer les valeurs 0 par les valeurs prédites
  predicted_values <- predict(lm_model, newdata=list(zero_data[[predictor_column]])) # Remplacer les valeurs 0 par les valeurs prédites
  # data[data[[target_column]] == 0, target_column] <- predicted_values
  return(predicted_values)
}
# Appliquer la fonction pour: hauteur tot, hauteur tronc, diamètre tronc
a <-replace_zeros_with_predictions(data, "haut_tot", "age_estim")
a <- data.frame(a)


summary(a)

for (variable in dim(a)[1]) {
  data$haut_tot=unlist(data$haut_tot)
  data$haut_tot[variable] <- a[variable]
}
indice <- list()
for (i in 1:nrow(data$haut_tot)) {
  if(data$haut_tot == 0 || is.na(data$haut_tot)){
    indice <-append(indice, i)
  }
}
indice

View(data)

data$haut_tot[data$haut_tot == 0] <- a$
  
  data <- replace_zeros_with_predictions(data, "haut_tronc", "age_estim")
data <- replace_zeros_with_predictions(data, "tronc_diam", "age_estim")

head(data)

