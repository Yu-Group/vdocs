#' Load in data file
#'
#' @description Loads in files using `readRDS()` for .rds files and
#'   `data.table::fread()` for all other file types.
#'
#' @param file_path Path to file to load in.
#'
#' @returns Output of `readRDS()` for .rds files and `data.table::fread()` for
#'   all other file types.
#'
#' @export
loadFile <- function(file_path) {
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

#' Validate inputs for data splitting proportions
#'
#' @description Checks whether data splitting proportion are valid (i.e.,
#'   are non-negative, less than 1, and sum to 1).
#'
#' @param train_prop Proportion of data to put in training set
#' @param valid_prop Proportion of data to put in validation set
#' @param test_prop Proportion of data to put in test set
#'
#' @export
validateDataSplit <- function(train_prop, valid_prop, test_prop) {
  props <- c(train_prop, valid_prop, test_prop)
  if (sum(props) != 1) {
    stop("Training, validation, test proportions must sum to 1.")
  }
  if (any(props < 0)) {
    stop("Training, validation, test proportions must be >=0.")
  }
  if (any(props > 1)) {
    stop("Training, validation, test proportions must be <= 1.")
  }
}

#' Validate data inputs for (X, y)
#'
#' @description Checks whether data dimensions for X and y match.
#'
#' @param X A data matrix or data frame.
#' @param y A response vector.
#'
#' @export
validateData <- function(X, y) {
  if (nrow(X) != length(y)) {
    stop("The number of rows in X do not match the length of the response y.")
  }
}

#' Loads toy TCGA BRCA data
#'
#' @description Downloads and loads in TCGA BRCA data as a toy example for the
#'   PCS lab notebook
#'
#' @returns A list of two:
#' \describe{
#' \item{X}{A data frame of gene expression features, measured via RNA-Seq.}
#' \item{y}{Response vector; PAM50 BRCA subtypes.}
#' }
#'
#' @export
loadBRCAData <- function() {
  query <- TCGAbiolinks::GDCquery(
    project = "TCGA-BRCA",
    data.category = "Gene expression",
    data.type = "Gene expression quantification",
    platform = "Illumina HiSeq",
    file.type  = "normalized_results",
    experimental.strategy = "RNA-Seq",
    legacy = TRUE
  )

  TCGAbiolinks::GDCdownload(query)
  data <- TCGAbiolinks::GDCprepare(query)
  y_data <- data.frame(SummarizedExperiment::colData(data))
  X_data <- data.frame(t(SummarizedExperiment::assay(data)))

  keep_samples <- !is.na(y_data$paper_BRCA_Subtype_PAM50)
  y <- as.factor(y_data$paper_BRCA_Subtype_PAM50[keep_samples])
  X <- X_data[keep_samples, ]

  if (!dir.exists("data")) {
    dir.create("data", recursive = TRUE)
  }
  saveRDS(y, "data/tcga_brca_subtypes.rds")
  saveRDS(X, "data/tcga_brca_array_data.rds")
  return(list(X = X, y = y))
}

#' Copy of `subchunkify()` function from simChef
#' @param ... Arguments to pass to simChef:::subchunkify()
#' @export
subchunkify <- function(...) {
  func <- get("subchunkify", asNamespace("simChef"))
  return(func(...))
}

#' Copy of `get_aesthetics()` function from simChef
#' @param ... Arguments to pass to simChef:::get_aesthetics()
#' @keywords internal
get_aesthetics <- function(...) {
  func <- get("get_aesthetics", asNamespace("simChef"))
  return(func(...))
}
