source("Script.R")
data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec='.',sep=',')) #séparateur décimal et séparateur de champ

View(data$fk_arb_etat)

#Boxplot de la hauteur totale des arbres par quartier
data_quartier<-data[data$clc_quartier!="inconnu",] #ne prend pas en compte les valeurs "inconnu"
data_quartier$clc_quartier<-droplevels(data_quartier$clc_quartier)

ggplot(data_quartier, aes(x = clc_quartier, y = haut_tot)) +
  geom_boxplot() + #pour les boites à moutache
  labs(title = "Boxplot de la hauteur totale des arbres par quartier",
       x = "Quartiers",
       y = "Hauteurs Totales") +
  theme_minimal() + #pour enlever les carrés gris en arrière plan
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #pour que nos titres de quartiers ne se superposent pas

#distribution des types de feuillage par quartier
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



# Calculer l'âge moyen des arbres par quartier
library(ggplot2)
moyenne_ages <- numeric(length(levels(data$clc_quartier)))
# Parcourir chaque niveau de clc_quartier et calculer la moyenne d'âge
for(i in seq_along(levels(data$clc_quartier))) {
  niveau <- levels(data$clc_quartier)[i]
  moyenne_ages[i] <- mean(data$age_estim[data$clc_quartier == niveau], na.rm = TRUE)
}
data_summary <- data.frame(clc_quartier = levels(data$clc_quartier), moyenne_ages = moyenne_ages)

# Représentation graphique
ggplot(data_summary, aes(x = clc_quartier, y = age_moyen, fill = clc_quartier)) +
geom_bar(stat = "identity") + #mets des couleurs différentes pour chaque barre
labs(title = "Âges moyens des arbres par quartier",
  x = "Quartiers",
  y = "Âges moyens estimés") +
theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
guides(fill = FALSE) #supprime la légende pour les couleurs des quartiers



#Situations des arbres en fonction de leur quartier
ggplot(data, aes(x = clc_quartier, fill = fk_situation)) +
  geom_bar(position = "dodge") + 
  labs(title = "Situations des arbres en fonction de leur quartier",
       x = "Quartiers",
       y = "Nombre d'arbres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#test chi2 pour corrélation entre fk_arb_etat et fk_pied
table_filtree<-data[data$fk_pied != 'inconnu', ] #enleve toutes les données inconnues pour exécuter notre chi2
contingency_table <- table(table_filtree$fk_pied, table_filtree$fk_arb_etat)
print(contingency_table)

testchi2 <- chisq.test(table_filtree$fk_pied, table_filtree$fk_arb_etat)
testchi2

#mosaicplot du premier test chi2
mosaicplot(contingency_table, main = "Mosaicplot des états des arbres en fonction du type de sol",
           xlab = "Type de sol (fk_pied)", ylab = "État de l'arbre (fk_arb_etat)",
           color = rainbow(length(unique(data$fk_arb_etat))),
           las=2)#place correctement les labels horizontalement/verticalement 


#test chi2 pour corrélation entre le quartier et le feuillage
table_filtree2<-data[data$feuillage != 'inconnu', ] #enleve toutes les données inconnues pour exécuter notre chi2
contingency_table2 <- table(table_filtree2$clc_quartier, table_filtree2$feuillage)
print(contingency_table2)

testchi2_2 <- chisq.test(data$clc_quartier, data$feuillage)
testchi2_2


#mosaicplot du premier test chi2
mosaicplot(contingency_table2, main = "Mosaicplot des feuillages en fonction des quartiers",
           xlab = "Quartiers (clc_quartier)", ylab = "Feuillages (feuillage) ",
           color = rainbow(length(unique(data$fk_arb_etat))),
           las=2)


#régression logistique => quels sont les arbres à abbatre

lst_abattre <- function(data){
  model<- glm(data$fk_arb_etat != "EN PLACE" ~ data$age + data$fk_stadedev + data$remarquable, family="binomial")
  # summary(model)
  resultat<-predict(model, data.frame(data),type="response")
  plot(resultat)

  arbre_a_abbatre<-resultat[resultat>0.6]

  data$abattre <- FALSE
  data$abattre[resultat > 0.6] <- TRUE

  return(data)
}
# View(arbre_a_abbatre)
# length(arbre_a_abbatre)

# data$abattre <- FALSE

# data$abattre[resultat > 0.6] <- TRUE
#View(data$abattre)
data <- lst_abattre(data)











