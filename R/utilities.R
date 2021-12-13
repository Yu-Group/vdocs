uploadFile <- function(file_path) {
  if (is.null(file_path)) {
    stop("Cannot upload file. Must provide file path.")
  } else if (!file.exists(file_path)) {
    if (file_path == "data/tcga_brca_array_data.rds") {
      data <- loadBRCAData()
      return(data$X)
    }
    stop("Cannot find file. ",
         "Please check that the file path is specified correctly.")
  }
  file_type <- stringr::str_extract(file_path, ".[^.]*$")
  if (identical(file_type, ".rds")) {
    data <- readRDS(file_path)
  } else {
    data <- data.table::fread(file_path, data.table = FALSE)
  }
  return(data)
}

validateDataSplit <- function(train_prop, valid_prop, test_prop) {
  if (train_prop + valid_prop + test_prop != 1) {
    stop("Training, validation, test proportions must sum to 1.")
  }
}

validateData <- function(X, y) {
  if (nrow(X) != length(y)) {
    stop("The number of rows in X do not match the length of the response y.")
  }
}

loadBRCAData <- function() {
  require(TCGAbiolinks)
  require(SummarizedExperiment)
  require(magrittr)
  query <- GDCquery(
    project = "TCGA-BRCA",
    data.category = "Gene expression",
    data.type = "Gene expression quantification",
    platform = "Illumina HiSeq", 
    file.type  = "normalized_results",
    experimental.strategy = "RNA-Seq",
    legacy = TRUE
  )
  
  GDCdownload(query)
  data <- GDCprepare(query)
  y_data <- as.data.frame(colData(data))
  X_data <- as.data.frame(t(assay(data)))
  
  keep_samples <- rownames(y_data)[!is.na(y_data$paper_BRCA_Subtype_PAM50)]
  y <- as.factor(y_data$paper_BRCA_Subtype_PAM50[keep_samples])
  X <- X_data[keep_samples, ]

  if (!dir.exists("data")) {
    dir.create("data", recursive = TRUE)
  }
  saveRDS(y, "data/tcga_brca_subtypes.rds")
  saveRDS(X, "data/tcga_brca_array_data.rds")
  return(list(X = X, y = y))
}