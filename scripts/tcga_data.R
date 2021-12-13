require(TCGAbiolinks)
require(SummarizedExperiment)
require(magrittr)
query <- GDCquery(
  project = "TCGA-BRCA", #"TCGA-GBM",
  data.category = "Gene expression",
  data.type = "Gene expression quantification",
  platform = "Illumina HiSeq", 
  file.type  = "normalized_results",
  experimental.strategy = "RNA-Seq",
  legacy = TRUE
)

GDCdownload(query)
data <- GDCprepare(query)

dim(data)
dim(colData(data))
dim(rowData(data))
dim(assay(data))

# colnames(colData(data))
# colnames(rowData(data))

colData(data)[c("sample_type", "tissue_type", "last_known_disease_status", "primary_diagnosis", "classification_of_tumor", "tumor_grade", "icd_10_code", "gender", "race", "ethnicity", "paper_BRCA_Subtype_PAM50", "paper_PARADIGM Clusters")] %>%
  as.data.frame() %>%
  dplyr::mutate(dplyr::across(where(is.character), as.factor)) %>%
  # summary()
  str()

table(colData(data)[c("paper_PARADIGM Clusters", "paper_BRCA_Subtype_PAM50")])
rowSums(table(colData(data)[c("sample_type", "paper_BRCA_Subtype_PAM50")]))

y_data <- as.data.frame(colData(data))
X_data <- as.data.frame(t(assay(data)))

keep_samples <- rownames(y_data)[!is.na(y_data$paper_BRCA_Subtype_PAM50)]
y_data <- y_data[keep_samples, ]
X_data <- X_data[keep_samples, ]
identical(rownames(y_data), rownames(X_data))

table(y_data$paper_BRCA_Subtype_PAM50)

y <- as.factor(y_data$paper_BRCA_Subtype_PAM50)
X <- X_data

saveRDS(y, "../data/tcga_brca_subtypes.rds")
saveRDS(X, "../data/tcga_brca_array_data.rds")

length(unique(colnames(X)))
length(colnames(X))

tmp <- as.data.frame(rowData(data))
length(unique(tmp$ensembl_gene_id))
length(unique(tmp$gene_id))
nrow(tmp)

source("~/My Documents/R-Utility-Toolbox/clean_functions.R", chdir = T)
source("~/My Documents/R-Utility-Toolbox/filter_features.R", chdir = T)

sum(is.na(X_data))
X <- log(removeConstantCols(X_data, verbose = 2) + 1)
saveRDS(X, "../data/tcga_brca_array_data.rds")
length(unique(colnames(X)))
length(colnames(X))

summary(apply(X, 2, var))
X <- filterByVar(X, max.p = 10000)
summary(apply(X, 2, var))

saveRDS(X, "../data/tcga_brca_array_filtered_data.rds")





