source("Script.R")
data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec = ".",sep = ","))


# Fonction pour remplacer les valeurs 0 par les valeurs prédites
replace_zeros_with_predictions <- function(data, target_column, predictor_column) {

  # Filtrer les lignes où target_column est différent de 0
  non_zero_data <- data[data[[target_column]] != 0, ]

  #Regression
  lm_model <- lm(non_zero_data[[target_column]] ~ non_zero_data[[predictor_column]])

  # Prédire les valeurs pour les lignes où target_column est 0
  zero_data <- data[data[[target_column]] == 0, ]


  #Prediction
  #predicted_values <- predict(lm_model, list(zero_data[[target_column]])) # Remplacer les valeurs 0 par les valeurs prédites
  predicted_values <- predict(lm_model, newdata = zero_data[[predictor_column]])
  # data[data[[target_column]] == 0, target_column] <- predicted_values
  return(predicted_values)
}
# Appliquer la fonction pour: hauteur tot, hauteur tronc, diamètre tronc
a <- replace_zeros_with_predictions(data, "haut_tot", "age_estim")
a <- data.frame(a)


summary(a)

for (variable in dim(a)[1]) {
  data$haut_tot = unlist(data$haut_tot)
  data$haut_tot[variable] <- a[variable]
}


for (var in a) {
  data$haut_tot[data$haut_tot == 0] <- var
}

indices <- c()
for (i in c(1:length(data$haut_tot))) {
  if (data$haut_tot == 0 || is.na(data$haut_tot)) {
    indices <- append(indices, i)
  }
}
indices

View(data)

data$haut_tot[data$haut_tot == 0] <- a$
  
  data <- replace_zeros_with_predictions(data, "haut_tronc", "age_estim")
data <- replace_zeros_with_predictions(data, "tronc_diam", "age_estim")

head(data)



pred_val <- function(data, target, predictor){
  non_zero_data <- data[data[[target]] != 0, ]
  lm_model <- lm(non_zero_data[[target]] ~ non_zero_data[[predictor]])
  zero_data <- data[data[[target]] == 0, ]
  
  # predicted_values <- predict(lm_model, newdata = data.frame(zero_data[[predictor]]))
  predicted_values <- predict.lm(lm_model, newdata = data.frame(zero_data[[predictor]]))
  return(predicted_values)
}
a <- pred_val(data, "haut_tot", "age_estim")
a

non_zero_data <- data[data[["haut_tot"]] != 0, ]
lm_model <- lm(non_zero_data[["haut_tot"]] ~ non_zero_data[["age_estim"]], method = "lm")
zero_data <- data[data[["haut_tot"]] == 0, ]
  
predicted_values <- predict(lm_model, newdata = data.frame(zero_data[["age_estim"]]))
# predicted_values <- predict.lm(lm_model, newdata = data.frame(zero_data[["age_estim"]][1]) )


# predicted_values <- pred_val(lm_model, data.frame(zero_data[["age_estim"]]))
# View(data[data[["haut_tot"]] != 0, ])
