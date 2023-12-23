library("readxl");
Data <- read_excel("src/ANAD.xlsx")
Cleaned <- Data
Cleaned[Cleaned=="En ligne"]<- "EL"
Cleaned[Cleaned=="Deplacement physique"]<- "DP"
Cleaned[Cleaned=="Deplacement physique, En ligne"]<- "AL"
Cleaned[Cleaned=="Ouedkniss"]<- "O"
Cleaned[Cleaned=="Facebook"]<- "F"
Cleaned[Cleaned=="Facebook, Ouedkniss"]<- "FO"
Cleaned[Cleaned=="Ouedkniss, Instagram"]<- "OA"
Cleaned[Cleaned=="Facebook, Ouedkniss, Instagram"]<- "FOA"
Cleaned$Convivialité <- as.character(Cleaned$Convivialité)
Cleaned[Cleaned=="1"]<- "TM"
Cleaned[Cleaned=="2"]<- "M"
Cleaned[Cleaned=="3"]<- "Moy"
Cleaned[Cleaned=="4"]<- "B"
Cleaned[Cleaned=="Type de véhicule, Référence, Spécifications, Fournisseur, Prix"]<- "TRSFP"
Cleaned[Cleaned=="Type de véhicule, Spécifications, Prix"]<- "TSP"
Cleaned[Cleaned=="Type de véhicule, Spécifications, Fournisseur, Prix"]<- "TSFP"
Cleaned[Cleaned=="Type de véhicule, Référence, Spécifications, Prix"]<- "TRSP"
Cleaned[Cleaned=="Type de véhicule"]<- "T"
Cleaned[Cleaned=="Type de véhicule, Référence, Prix"]<- "TRP"
Cleaned[Cleaned=="Référence, Spécifications, Fournisseur, Prix"]<- "RSFP"
Cleaned[Cleaned=="Type de véhicule, Référence, Spécifications, Prix, État de la pièce : neuve, usée, bonne occasion, d'origine.."]<- "TRSPA"
Cleaned[Cleaned=="Type de véhicule, Référence, Spécifications, Fournisseur, Prix, Avis de la clientèle sur la qualité des pièces d'un fournisseur"]<- "TRSFPA"
Cleaned[Cleaned=="Type de véhicule, Référence, Spécifications, Fournisseur, Prix, location"]<- "TRSFPA"
Cleaned[Cleaned=="Type de véhicule, Référence, Spécifications, Fournisseur, Prix, Commentaires et évaluation (⭐) d'autres clients sur ces produits"]<- "TRSFPA"
Var_frequency <- lapply(Cleaned, table)
 
for (i in seq_along(Var_frequency)) {
  barplot(Var_frequency[[i]], main = names(Var_frequency)[i],
          ylab = "Frequency", col = "skyblue")
}

for (i in seq_along(Var_frequency)) {
  pie(Var_frequency[[i]], labels = names(Var_frequency[[i]]),
  main = names(Var_frequency)[i], col = rainbow(length(Var_frequency[[i]])))
}

library(FactoMineR)
library(factoextra)
Z = tab.disjonctif.prop(Cleaned, seed=NULL, row.w=NULL)
#Partie AFCM
Cleaned.MCA = MCA(Cleaned)
#Valeurs Propres
val.propres = get_eigenvalue(Cleaned.MCA)
fviz_screeplot(Cleaned.MCA, addlabels=TRUE)
#Biplot Individus-Variables
fviz_mca_biplot(Cleaned.MCA, repel=TRUE, ggtheme= theme_minimal())
#Contributions des variables au 1er axe
fviz_contrib(Cleaned.MCA, choice="var", axes=1, top=15)
#Contributions des variables au 2ème axe
fviz_contrib(Cleaned.MCA, choice="var", axes=2, top=15)
#Contributions des variables au 1er plan factoriel
fviz_contrib(Cleaned.MCA, choice="var", axes=1:2, top=15)
#Contributions des individus au 1er axe
fviz_contrib(Cleaned.MCA, choice="ind", axes=1, top=15)
#Contributions des individus au 2ème axe
fviz_contrib(Cleaned.MCA, choice="ind", axes=2, top=15)
#Contributions des individus au 1er plan factoriel
fviz_contrib(Cleaned.MCA, choice="ind", axes=1:2, top=15)
#Corrélation entre les variables
fviz_mca_var(Cleaned.MCA, repel = TRUE, choice="mca.cor", ggtheme = theme_minimal())
#Qualité de représentation
rep_var <- get_mca_var(Cleaned.MCA)
rep_var$cos2
#Visualisation des Variables colorés selon les qualités de représentation
fviz_mca_var(Cleaned.MCA, col.var = "cos2", gradient.cols= c("#00AFBB","#E7B800","#FC4E07"))
#Les variables les mieux representées
View(Cleaned.MCA$var$eta2)
#Partie AFC
#Croiser 2 questions
X <- Z[,c(4:8,13:21)]
X <- matrix(data = X, nrow=15,ncol=14, dimnames = list(NULL, c("F", "FO", "FOA","O","OA", "RSFP", "T", "TRP", "TRSFP", "TRSFPA","TRSP","TRSPA", "TSFP", "TSP")))
#Matrice Burt de contingence
Burt <- t(X)%*%X
Burt.useful <- Burt[1:5, c(6:14)]
Burt.ca <- CA(Burt.useful)
#Contributions & comparaisons avec les poids des lignes
View(Burt.ca$row$contrib)
View(Burt.ca$call$marge.row * 100)
#Contribtuions & comparaisons avec les poids des colonnes
View(Burt.ca$col$contrib)
View(Burt.ca$call$marge.col * 100)
