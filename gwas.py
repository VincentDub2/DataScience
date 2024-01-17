import pandas as pd
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt
import dask.dataframe as dd
import time

start_time = time.time()

# Utilisation de Dask pour charger de grands ensembles de données
phenotypes = dd.read_csv('leaf_data.csv')
genotypes = dd.read_csv('X.csv', sample=10000000)

# Renommer la première colonne pour la fusion
new_column_name = 'ID'
genotypes = genotypes.rename(columns={genotypes.columns[0]: new_column_name})

# Fusionner les données sur la colonne 'ID'
merged_data = dd.merge(phenotypes, genotypes, on='ID')

# Normalisation des données génotypiques
genotype_columns = [col for col in genotypes.columns if col != 'ID']  # Toutes les colonnes sauf 'ID'
for col in genotype_columns:
    merged_data[col] = (merged_data[col] - merged_data[col].mean()) / merged_data[col].std()

# Préparation pour le modèle linéaire mixte pour chaque trait
traits_columns = phenotypes.columns.tolist()
traits_columns.remove('ID')  # Retirer 'ID' de la liste des traits

results = []  # Stocker les résultats de chaque modèle
for trait in traits_columns:
    # Formule du modèle, remplacez 'trait' par la colonne de trait actuelle
    formula = f'{trait} ~ 1 + ' + ' + '.join(genotype_columns)

    # Création et ajustement du modèle
    model = smf.mixedlm(formula, merged_data.compute(), groups='ID')
    result = model.fit()

    # Stockage des résultats
    results.append((trait, result))

    # Impression du résumé pour chaque trait
    print(f'Analyse pour le trait: {trait}')
    print(result.summary())

    # Extraction et calcul des composantes de variance
    variance_components = result.variance_components
    Vg = variance_components['Group Var']
    Vp = Vg + result.resid.var()
    heritability = Vg / Vp
    print(f'Narrow-sense heritability (h^2) for {trait}: {heritability}\n')

# Traitement des résultats stockés
# Ici, vous pouvez ajouter du code pour enregistrer ou analyser plus avant les résultats
with open('model_results.txt', 'w') as file:
    for trait, result in results:
        file.write(f'Analyse pour le trait: {trait}\n')
        file.write(result.summary().as_text())
        file.write('\n\n')

# Vous pouvez également enregistrer les valeurs d'héritabilité dans un fichier CSV
heritability_results = pd.DataFrame({
    'Trait': [trait for trait, _ in results],
    'Heritability': [result.variance_components['Group Var'] / (result.variance_components['Group Var'] + result.resid.var()) for _, result in results]
})

heritability_results.to_csv('heritability_results.csv', index=False)

# Exemple : Analyse descriptive des valeurs d'héritabilité
print(heritability_results['Heritability'].describe())


heritability_results['Heritability'].hist(bins=20)
plt.title('Distribution de l\'Héritabilité')
plt.xlabel('Héritabilité')
plt.ylabel('Nombre de Traits')
plt.show()

end_time = time.time()
elapsed_time = end_time - start_time
print(f"Temps d'exécution du programme : {elapsed_time} secondes")
