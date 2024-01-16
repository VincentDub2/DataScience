import os
import pandas as pd

def merge_data(folder_path):
    # Dictionnaire pour stocker les DataFrames de chaque élément
    data_frames = {}

    # Parcourir tous les fichiers dans le dossier
    for filename in os.listdir(folder_path):
        if filename.endswith('.txt'):
            # Construire le chemin complet du fichier
            file_path = os.path.join(folder_path, filename)

            # Extraire le nom de l'élément du nom du fichier
            element = filename.split('.')[0]

            # Lire le fichier dans un DataFrame
            df = pd.read_csv(file_path, header=None, names=['ID', element])
            # Définir l'ID comme index du DataFrame
            df.set_index('ID', inplace=True)

            # Ajouter le DataFrame à notre dictionnaire
            data_frames[element] = df

    # Fusionner tous les DataFrames en un seul sur l'index 'ID'
    combined_data = pd.concat(data_frames.values(), axis=1, join='outer')

    return combined_data

#Chemins des dossiers
leaf_folder = 'Leaf_with_id'
seed_folder = 'Seed_with_id'

# Fusionner les données pour les feuilles et les graines

leaf_data = merge_data(leaf_folder)
seed_data = merge_data(seed_folder)

# Enregistrer les données fusionnées dans un fichier CSV
leaf_data.to_csv('leaf_data.csv')
seed_data.to_csv('seed_data.csv')