#Etape 1
- Regroupement données en un seul fichier csv 

Les trois graphiques que vous avez fournis représentent les résultats de différentes méthodes pour évaluer le nombre optimal de clusters pour une analyse K-means.

Méthode du Coude (Elbow Method) :
Le premier graphique montre la somme des carrés des distances intra-cluster (WCSS) en fonction du nombre de clusters.
Vous recherchez un point où l'augmentation du nombre de clusters n'entraîne pas de diminution substantielle de la WCSS, ce qui indiquerait un bon équilibre entre le nombre de clusters et la variance intra-cluster.
Dans votre graphique, il semble que le "coude" soit situé autour de 4 clusters, où la réduction de la WCSS commence à ralentir.
Score de Silhouette Moyen :
Le deuxième graphique montre le score de silhouette moyen pour différents nombres de clusters.
Le score de silhouette mesure la cohésion au sein des clusters et la séparation entre les clusters, avec des valeurs plus élevées indiquant de meilleurs clusters.
Sur la base de ce graphique, le score de silhouette est le plus élevé pour 2 clusters. Cependant, il y a aussi un pic local à 5 clusters. Généralement, vous recherchez le pic le plus élevé pour sélectionner le nombre de clusters.
Analyse des Gaps (Gap Statistic) :
Le troisième graphique montre la statistique de gap pour différents nombres de clusters.
Cette méthode compare la variation intra-cluster pour différents nombres de clusters avec celle attendue sous une distribution de référence nulle.
Vous cherchez le nombre de clusters où la statistique de gap est la plus élevée. Dans votre graphique, il semble que la statistique de gap augmente constamment, indiquant que l'augmentation du nombre de clusters continue d'améliorer la structure des clusters par rapport à la distribution de référence.