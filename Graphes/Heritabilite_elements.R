generateGraph <- function(heritability_data) {
  library(ggplot2)
  
  # Catégorisation des niveaux d'héritabilité
  heritability_data$Category <- with(heritability_data, ifelse(Heritability < 10, "Faible",
                                                               ifelse(Heritability < 30, "Modéré", "Élevé")))
  
  # Palette de couleurs
  colors <- c("Faible" = "#00AFBB", "Modéré" = "#E7B800", "Élevé" = "#FC4E07")
  
  # Créer un graphique à barres avec code couleur
  ggplot(heritability_data, aes(x = Element, y = Heritability, fill = Category)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colors) +
    theme_minimal() +
    labs(title = "Héritabilité des éléments dans les feuilles d'Arabidopsis thaliana",
         x = "Élément",
         y = "Héritabilité (%)",
         fill = "Niveau d'héritabilité") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}