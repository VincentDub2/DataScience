# Chargement des bibliothèques nécessaires
library(shiny)
library(dplyr)
library(readr)
library(tidyr)

# Définir les chemins vers vos dossiers de données
chemin_vers_donnees_feuille <- "./Leaf_with_ID"
chemin_vers_donnees_graine <- "./Seed_with_ID"

# Liste des minéraux
mineraux <- c("As75", "Ca43", "Cd114", "Co59", "Cu65", "Fe57", "K39", "Li7", "Mg25", 
              "Mn55", "Mo98", "Na23", "P31", "Rb85", "S34", "Se82", "Sr88", "Zn66")

# Fonction pour lire les données et calculer les statistiques sommaires
calculer_stats <- function(element, chemin_vers_donnees) {
  chemin_fichier <- file.path(chemin_vers_donnees, paste0(element, ".txt"))
  donnees <- read_delim(chemin_fichier, delim = ",", col_names = c("ID", "Concentration"), col_types = cols())
  
  stats <- donnees %>%
    summarise(
      Moyenne = mean(Concentration, na.rm = TRUE),
      SD = sd(Concentration, na.rm = TRUE),  # Ajout de l'écart-type (SD)
      RSD = (sd(Concentration, na.rm = TRUE) / mean(Concentration, na.rm = TRUE)) * 100
    )
  
  return(stats)
}

# Initialiser un dataframe pour stocker les résultats
tableau_final <- data.frame(Mineral = character(),
                            Type = character(),
                            Moyenne = numeric(),
                            SD = numeric(),  # Ajout de la colonne pour l'écart-type (SD)
                            RSD = numeric(),
                            stringsAsFactors = FALSE)

# Boucle à travers chaque minéral et calcule les statistiques pour les données de feuille et de graine
for (mineral in mineraux) {
  
  # Calcul pour les données de feuille
  stats_feuille <- calculer_stats(mineral, chemin_vers_donnees_feuille)
  tableau_final <- rbind(tableau_final, 
                         data.frame(Mineral = mineral,
                                    Type = "Feuille",
                                    Moyenne = stats_feuille$Moyenne,
                                    SD = stats_feuille$SD,  # Ajout de l'écart-type
                                    RSD = stats_feuille$RSD))
  
  # Calcul pour les données de graine
  stats_graine <- calculer_stats(mineral, chemin_vers_donnees_graine)
  tableau_final <- rbind(tableau_final, 
                         data.frame(Mineral = mineral,
                                    Type = "Graine",
                                    Moyenne = stats_graine$Moyenne,
                                    SD = stats_graine$SD,  # Ajout de l'écart-type
                                    RSD = stats_graine$RSD))
}

# Enregistrer le tableau final dans un fichier CSV
write.csv(tableau_final, "statistiques_mineraux.csv", row.names = FALSE)

