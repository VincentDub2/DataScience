generateGraph <- function(data) {
  library(ggplot2)
  
  # Créer un graphique en nuage de points
  ggplot(data, aes(x = RSD, y = Heritability, label = Element)) +
    geom_point() +
    geom_text(vjust = 1.5, hjust = 1.5) +
    xlab("Variabilité Relative des Éléments (RSD %)") +
    ylab("Héritabilité (%)") +
    ggtitle("Corrélation entre RSD et Héritabilité des minéraux dans les racines chez Arabidopsis thaliana") +
    theme_minimal()
}