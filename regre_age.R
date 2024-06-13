# source("traitement.R")

# data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec = ".",sep = ","))


# View(data)

calcul_clear_age <- function(data){
    #lm age_estim ~ haut_tot + haut_tronc + tronc_diam  (fk_port + fk_pied + fk_situation + fk_revetement + feuillage + clc_quartier)
    data_1 <- data[data$age_estim != 0 & data$haut_tot != 0 & data$haut_tronc != 0 & data$tronc_diam !=0 & data$fk_arb_etat == "EN PLACE", ]


    model_1 <- lm(age_estim ~ haut_tot + haut_tronc + tronc_diam , data = data_1)
    # summary(model_1)

    age_estim <- predict(model_1, data[data$age_estim == 0 & data$haut_tot != 0 & data$haut_tronc != 0 & data$tronc_diam !=0 & data$fk_arb_etat == "EN PLACE", ])

    # View(age_estim)


    data[data$age_estim == 0 & data$haut_tot != 0 & data$haut_tronc != 0 & data$tronc_diam !=0 & data$fk_arb_etat == "EN PLACE", ][["age_estim"]] <- age_estim

    data <- data[!(data$age_estim == 0 & data$fk_arb_etat == "EN PLACE"), ]

    return(data)
}






# Essaie calcul des age manquant mais valeurs obtenue negative

# noms <- c("ACESAC", "CEDATL")
# data_2 <- data[data$age_estim != 0 & 
#                data$haut_tot != 0 & 
#                data$tronc_diam !=0 & 
#                data$fk_nomtech %in% noms & 
#                data$fk_arb_etat == "EN PLACE", ]
# View(data_2)

# model_2 <- lm(age_estim ~ haut_tot + tronc_diam + fk_nomtech, data = data_2)
# summary(model_2)

# age_estim_2 <- predict(model_2, data[data$age_estim == 0 & 
#                                     data$haut_tot != 0 & 
#                                     data$tronc_diam !=0 & 
#                                     data$fk_nomtech %in% noms & 
#                                     data$fk_arb_etat == "EN PLACE", ])
# # length(age_estim_2)
# age_estim_2
# summary(age_estim)
# summary(age_estim_2)

