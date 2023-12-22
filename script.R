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