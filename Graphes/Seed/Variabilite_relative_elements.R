generateGraph <- function(data) {
  library(ggplot2)

  # Ajoutons une nouvelle colonne 'RSD_Category' selon les seuils définis
  df$RSD_Category <- ifelse(df$RSD < 10, "Faible",
                            ifelse(df$RSD < 50, "Modéré", "Fort"))
  
  # Définir les couleurs pour chaque catégorie de RSD
  colors <- c("Faible" = "#00AFBB", "Modéré" = "#E7B800", "Fort" = "#FC4E07")
  
  # Créer le graphique à barres avec les nouvelles catégories
  ggplot(df, aes(x = Element, y = RSD, fill = RSD_Category)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colors) +
    theme_minimal() +
    labs(title = "Variabilité Relative des Éléments (RSD)", 
         x = "Élément", 
         y = "Valeur RSD (%)", 
         fill = "Catégorie de RSD") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}