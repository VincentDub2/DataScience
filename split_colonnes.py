import pandas as pd
import os

# Charger le fichier CSV initial
df = pd.read_csv('X.csv')

# Créer un dossier pour stocker les fichiers CSV résultants
dossier_cible = 'resultats_chromosomes'
os.makedirs(dossier_cible, exist_ok=True)

print("Chargement des données fini")

new_column_name = 'ID'
genotypes = df.rename(columns={df.columns[0]: new_column_name})

print("Colonnes renommées")

# Diviser le DataFrame en groupes en fonction du numéro de chromosome
groupes = genotypes.groupby(genotypes.columns.str.split('_').str[0], axis=1)

# Enregistrer chaque groupe dans un fichier CSV séparé dans le dossier cible
for chromosome, groupe in groupes:
    # Créer un nouveau DataFrame pour chaque chromosome
    chromosome_df = genotypes[['ID']].join(groupe)

    # Enregistrement dans un fichier CSV portant le nom du chromosome dans le dossier cible
    nom_fichier = f'chromosome_{chromosome}.csv'
    chemin_fichier = os.path.join(dossier_cible, nom_fichier)
    chromosome_df.to_csv(chemin_fichier, index=False)
    print(f'Fichier {nom_fichier} enregistré')
