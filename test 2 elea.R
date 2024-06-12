source("Script.R")
data <- traitement(read.csv("Patrimoine_Arbore_modif.csv", dec='.',sep=','))

View(data$fk_arb_etat)

#Boxplot de la hauteur totale des arbres par quartier
ggplot(data, aes(x = clc_quartier, y = haut_tot)) +
  geom_boxplot() +
  labs(title = "Boxplot de la hauteur totale des arbres par quartier",
       x = "Quartiers",
       y = "Hauteurs Totales") +
  theme_minimal() + #pour enlever les carrés gris en arrière plan
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #pour que nos titres de quartiers ne se superposent pas

#distribution des types de feuillage par quartier
ggplot(data, aes(x = clc_quartier, fill = feuillage)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution des types de feuillage par quartier",
       x = "Quartiers",
       y = "Nombre d'arbres",
       fill = "Types de feuillage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calculer l'âge moyen des arbres par quartier
moyenne_ages<-c(rep(NA,lenght(levels(data$clc_quartier))))
for(i in list(levels(data$clc_quartier))){
  moyenne_ages[i]<-mean(data$age_estim[data$clc_quartier==i])
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
           las=3, par(mai=3))


#test chi2 pour corrélation entre le quartier et le feuillage
contingency_table2 <- table(data$clc_quartier, data$feuillage)
print(contingency_table2)

testchi2_2 <- chisq.test(data$clc_quartier, data$feuillage)
testchi2_2


#mosaicplot du premier test chi2
mosaicplot(contingency_table2, main = "Mosaicplot des feuillages en fonction des quartiers",
           xlab = "Quartiers (clc_quartier)", ylab = "Feuillages (feuillage) ",
           color = rainbow(length(unique(data$fk_arb_etat))),
           las=3)












  


