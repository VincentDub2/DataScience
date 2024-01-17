if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("GWASTools")
BiocManager::install("SNPRelate")
BiocManager::install("gdsfmt")


# Load necessary libraries
library(SNPRelate)
library(GWASTools)
library(data.table)
library(qqman)


# Assurez-vous que les chemins des fichiers sont correctement définis pour votre environnement
genotype_filepath <- "X.csv"
phenotype_filepath <- "leaf_data.csv"

# Step 1: Create a GDS file from your genotype data
# Assuming your genotype data is in a CSV file and formatted correctly
genotype_data <- fread(genotype_filepath)
phenotype_data <- fread(phenotype_filepath)

head(phenotype_data[, 1:10])
head(genotype_data[, 1:10])
setnames(genotype_data, old = names(genotype_data)[1], new = "ecotype_id")

genotype_matrix <- as.matrix(genotype_data[, 3:ncol(genotype_data)])

#afficher que les 10 premières lignes et les 10 premières colonnes
head(genotype_matrix[, 1:10])
head(genotype_data[, 1:10])




snpgdsCreateGeno("genotype.gds", genmat = genotype_matrix,
                 sample.id = genotype_data$sample_id,
                 snp.id = genotype_data$snp_id,
                 snp.chromosome = genotype_data$snp_chromosome,
                 snp.position = genotype_data$snp_position,
                 snp.allele = genotype_data$snp_allele,
                 snpfirstdim = TRUE)


# Step 2: Linkage Disequilibrium (LD) Pruning
genofile <- snpgdsOpen("genotype.gds")
snpset <- snpgdsLDpruning(genofile, ld.threshold = 0.2)

# Step 3: Principal Component Analysis (PCA)
pca <- snpgdsPCA(genofile, snp.id = snpset$id, num.thread = 2)

# Step 4: SNP Correlation Analysis
chr <- read.gdsn(index.gdsn(genofile, "snp.chromosome"))
CORR <- snpgdsPCACorr(pca, genofile, eig.which = 1:4)

# Step 5: Fst Estimation (if applicable)
# Assuming you have population information in your genotype data
pop_code <- genotype_data$population
sample.id <- genotype_data$sample_id
v <- snpgdsFst(genofile, sample.id = sample.id, population = as.factor(pop_code), method = "W&C84")

# Step 6: Relatedness Analysis
ibd <- snpgdsIBDMoM(genofile, sample.id = sample.id, snp.id = snpset$id, maf = 0.05, missing.rate = 0.05, num.thread = 2)

# Step 7: Merge Genotype and Phenotype Data and Conduct GWAS
phenotype_data <- fread(phenotype_filepath)
merged_data <- merge(genotype_data, phenotype_data, by = "ecotype_id", all = TRUE)

# Perform GWAS (example using a linear model, you might need to adjust based on your specific needs)
gwas_results <- lm(phenotype ~ genotype, data = merged_data)
summary(gwas_results)

# Generate Manhattan Plot
manhattan(gwas_results)

# Close GDS file
snpgdsClose(genofile)
