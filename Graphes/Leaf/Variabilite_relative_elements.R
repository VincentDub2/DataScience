generateGraph <- function(data) {
  library(ggplot2)
  
  # Définir les couleurs pour chaque catégorie de RSD
  colors <- c("Faible" = "#00AFBB", "Modéré" = "#E7B800", "Fort" = "#FC4E07")
  
  # Créer le graphique à barres
  ggplot(data, aes(x = Element, y = RSD, fill = RSD_Category)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colors) +
    theme_minimal() +
    labs(title = "Variabilité Relative des Éléments (RSD)", 
         x = "Élément", 
         y = "Valeur RSD (%)", 
         fill = "Catégorie de RSD") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}