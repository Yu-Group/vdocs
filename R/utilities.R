uploadFile <- function(file_path) {
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