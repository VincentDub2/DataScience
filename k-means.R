# Charger les bibliothèques nécessaires
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(cluster)

# Chargement des données
donnees <- read.csv("seed_data.csv")

# Normalisation des données
donnees_normalisees <- scale(donnees)

# Convertir les données normalisées en dataframe
donnees_normalisees <- as.data.frame(donnees_normalisees)

# K-means clustering
set.seed(123)
resultats_kmeans <- kmeans(donnees_normalisees, centers = 3)

# Ajouter la colonne de cluster
donnees_normalisees$cluster <- as.factor(resultats_kmeans$cluster) # Conversion en facteur

# Exclure la colonne de cluster pour la PCA
donnees_pour_pca <- donnees_normalisees[, -ncol(donnees_normalisees)]

# Exécuter l'analyse PCA
resultat_pca <- PCA(donnees_pour_pca, graph = FALSE)

# Créer le biplot
fviz_pca_biplot(resultat_pca, label = "var", col.ind = donnees_normalisees$cluster,
                addEllipses = TRUE, ellipse.level = 0.95)


wcss <- sapply(1:10, function(k){
  kmeans(donnees_normalisees, centers = k, nstart = 20)$tot.withinss
})

plot(1:10, wcss, type = "b", xlab = "Nombre de Clusters", ylab = "WCSS")


silhouette_scores <- numeric(9)  # Initialisation d'un vecteur pour stocker les scores

for (k in 2:10) {
  clustering <- kmeans(donnees_normalisees, centers = k, nstart = 20)
  sil_score <- silhouette(clustering$cluster, dist(donnees_normalisees))
  silhouette_scores[k - 1] <- mean(sil_score[, 3])
}

plot(2:10, silhouette_scores, type = "b", xlab = "Nombre de Clusters", ylab = "Score de Silhouette Moyen")


# Assurez-vous que toutes les colonnes, sauf 'cluster', sont numériques
donnees_pour_gap <- donnees_normalisees[, sapply(donnees_normalisees, is.numeric)]

# Exécutez l'analyse des gaps
gap_stat <- clusGap(donnees_pour_gap, FUN = kmeans, nstart = 20, K.max = 10, B = 50)

# Tracez la statistique de gap
plot(gap_stat, main = "Gap Statistic")