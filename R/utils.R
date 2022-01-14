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

