import pandas as pd

# Paths to the leaf and seed data CSV files
leaf_data_path = 'leaf_data.csv'
seed_data_path = 'seed_data.csv'

# Reading the data from both CSV files
leaf_data = pd.read_csv(leaf_data_path)
seed_data = pd.read_csv(seed_data_path)

# Adding a 'TYPE' column to distinguish between leaf and seed data
# 'Feuille' is French for leaf, 'Graine' is French for seed
leaf_data.insert(1, 'TYPE', 'Feuille')
seed_data.insert(1, 'TYPE', 'Graine')

# Combining both datasets into one DataFrame
combined_data = pd.concat([leaf_data, seed_data], ignore_index=True)

# Save the combined data to a new CSV file
combined_csv_path = 'leaf_seed_data.csv'
combined_data.to_csv(combined_csv_path, index=False)

#%%
