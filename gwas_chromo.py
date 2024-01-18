import os
import pandas as pd
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt
import dask.dataframe as dd
import time
import concurrent.futures

numero_chromosome = '1'

def normalize(df):
    for col in df.columns:
        if col != 'ID':
            df[col] = (df[col] - df[col].mean()) / df[col].std()
    return df

def fit_model(trait, merged_data, genotype_columns):
    formula = f'{trait} ~ 1 + ' + ' + '.join(genotype_columns)
    model = smf.mixedlm(formula, merged_data, groups='ID')
    result = model.fit()
    print("Fit_model has been called")
    return trait, result.summary(), result.variance_components

def main():
    start_time = time.time()
    print("START")

    phenotypes = dd.read_csv('seed_data.csv')

    output_file = 'genotypes_normalized_'+numero_chromosome+'.csv'

    if not os.path.isfile(output_file):
        print("Normalisation des données de génotypes")
        genotypes = dd.read_csv('resultats_chromosomes/chromosome_'+numero_chromosome+'.csv', sample=10000000)
        genotypes = genotypes.rename(columns={genotypes.columns[0]: 'ID'})
        genotypes = genotypes.map_partitions(normalize)
        genotypes.to_csv(output_file, single_file=True)
    else:
        print("Chargement des données de génotypes normalisées")
        genotypes = dd.read_csv(output_file,sample=10000000)

    print("Fusion des données")
    merged_data = dd.merge(phenotypes, genotypes, on='ID').compute()  # Convertit en DataFrame pandas

    genotype_columns = [col for col in genotypes.columns if col != 'ID']
    traits_columns = phenotypes.columns.tolist()
    traits_columns.remove('ID')

    # Convertir merged_data en une forme sérialisable si nécessaire
    merged_data_pandas = merged_data.compute()  # Si ce n'est pas déjà un DataFrame pandas

    with concurrent.futures.ProcessPoolExecutor() as executor:
        print("Parallélisation des processus")
        # Assurez-vous de passer un DataFrame pandas sérialisable
        futures = [executor.submit(fit_model, trait, merged_data_pandas, genotype_columns) for trait in traits_columns]
        results = [future.result() for future in concurrent.futures.as_completed(futures)]

    heritability_results = pd.DataFrame({'Trait': [], 'Heritability': []})
    with open('model_results.txt', 'w') as file:
        for trait, summary, variance_components in results:
            file.write(f'Analyse pour le trait: {trait}\n')
            file.write(summary.as_text())
            file.write('\n\n')
            Vg = variance_components['Group Var']
            Vp = Vg + variance_components.resid.var()
            heritability = Vg / Vp if Vp != 0 else 0
            heritability_results = heritability_results.append({'Trait': trait, 'Heritability': heritability}, ignore_index=True)

    heritability_results.to_csv('heritability_results.csv', index=False)
    heritability_results['Heritability'].hist(bins=20)
    plt.title('Distribution de l\'Héritabilité')
    plt.xlabel('Héritabilité')
    plt.ylabel('Nombre de Traits')
    plt.show()

    end_time = time.time()
    elapsed_time = end_time - start_time
    print(f"Temps d'exécution du programme : {elapsed_time} secondes")

if __name__ == "__main__":
    main()
