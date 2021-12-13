basicDataSplit <- function(X, y, 
                           train_prop = 0.6, valid_prop = 0.2, test_prop = 0.2,
                           stratified_by = NULL) {
  n <- nrow(X)
  if (is.null(stratified_by)) {
    split_labels <- sample(
      cut(
        1:n, n * cumsum(c(0, train_prop, valid_prop, test_prop)), 
        labels = c("train", "validate", "test") 
      )
    )
  } else {
    split_labels <- data.frame(.y = y) %>%
      dplyr::group_by(.y) %>%
      dplyr::mutate(
        .split = sample(
          cut(
            1:n(), n() * cumsum(c(0, train_prop, valid_prop, test_prop)), 
            labels = c("train", "validate", "test") 
          )
        )
      ) %>%
      dplyr::pull(.split)
  }
  X_split <- split(X, split_labels)
  y_split <- split(y, split_labels)
  return(list(X = X_split, y = y_split))
}

dataOverview <- function(Xtrain, Xvalid, Xtest, ytrain, yvalid, ytest,
                         training_only = TRUE) {
  if (training_only) {
    text_out <- paste(
      sprintf("Number of features: %s", ncol(Xtrain)),
      sprintf("Number of training samples: %s", nrow(Xtrain)),
      sprintf("Number of NAs in training y: %s", sum(is.na(ytrain))),
      sprintf("Number of NAs in training X: %s", sum(is.na(Xtrain))),
      sprintf("Number of columns in training X with NAs: %s", 
              sum(apply(Xtrain, 2, FUN = function(x) any(is.na(x))))),
      sprintf("Number of constant columns in training X: %s",
              sum(apply(Xtrain, 2,
                        FUN = function(x) {
                          all(x == x[!is.na(x)][1], na.rm = T)
                        }), na.rm = T)),
      sep = "\n"
    )
  } else {
    text_out <- paste(
      sprintf("Number of features: %s", ncol(Xtrain)),
      sprintf("Number of samples in training/validation/test: %s/%s/%s", 
              nrow(Xtrain), nrow(Xvalid), nrow(Xtest)),
      sprintf("Number of NAs in training/validation/test y: %s/%s/%s", 
              sum(is.na(ytrain)), sum(is.na(yvalid)), sum(is.na(ytest))),
      sprintf("Number of NAs in training/validation/test X: %s/%s/%s", 
              sum(is.na(Xtrain)), sum(is.na(Xvalid)), sum(is.na(Xtest))),
      sprintf("Number of columns in training/validation/test X with NAs: %s/%s/%s", 
              sum(apply(Xtrain, 2, FUN = function(x) any(is.na(x)))),
              sum(apply(Xvalid, 2, FUN = function(x) any(is.na(x)))),
              sum(apply(Xtest, 2, FUN = function(x) any(is.na(x))))),
      sprintf("Number of constant columns in training/validation/test X: %s/%s/%s",
              sum(apply(Xtrain, 2,
                        FUN = function(x) {
                          all(x == x[!is.na(x)][1], na.rm = T)
                        }), na.rm = T),
              sum(apply(Xvalid, 2,
                        FUN = function(x) {
                          all(x == x[!is.na(x)][1], na.rm = T)
                        }), na.rm = T),
              sum(apply(Xtest, 2,
                        FUN = function(x) {
                          all(x == x[!is.na(x)][1], na.rm = T)
                        }), na.rm = T)),
      sep = "\n"
    )
  }
  
  return(text_out)
}

removeNACols <- function(X, verbose = 0) {
  ####### Function Description ########
  # function to remove all columns with at least one NA value
  # 
  # inputs:
  # - X = data matrix or data frame
  # - verbose = integer (0-2); level of written output
  #   
  # output: cleaned data matrix or frame without NAs
  ####### 
  
  col_nas <- apply(X, 2, function(x) sum(is.na(x)))
  
  if (verbose >= 1) {
    print(table(col_nas, dnn = "Frequency Table: #NAs per Column"))
  }
  if (verbose >= 2) {
    cat("Columns with NAs: \n")
    if (!is.null(names(col_nas))) {
      cat(names(col_nas)[col_nas > 0], sep = "\n")
    } else {
      cat(which(col_nas > 0), sep = "\n")
    }
  }
  if (verbose >= 1) {
    cat(paste("Removed", sum(col_nas > 0), "features\n"))
  }
  
  X_cleaned <- X[, col_nas == 0]
  
  return(X_cleaned)
}


removeConstantCols <- function(X, verbose = 0) {
  ####### Function Description ########
  # function to remove all columns that are constant (i.e., have 0 variance)
  # 
  # inputs:
  # - X = data matrix or data frame
  # - verbose = integer (0-2); level of written output
  #   
  # output: cleaned data matrix or frame without constant columns
  #######
  
  col_vars <- apply(X, 2, var, na.rm = T)
  
  if (verbose >= 2) {
    cat("Constant columns: \n")
    if (!is.null(names(col_vars))) {
      cat(paste(names(col_vars)[col_vars == 0], X[1, col_vars == 0],
                sep = " with value "), sep = "\n")
    } else {
      cat(paste(which(col_vars == 0), X[1, col_vars == 0],
                sep = " with value "), sep = "\n")
    }
    hist(col_vars, main = "Histogram of Column Variances", xlab = "Variance")
  }
  if (verbose >= 1) {
    cat(paste("Removed", sum(col_vars == 0), "features\n"))
  }
  
  X_cleaned <- X[, col_vars > 0]
  
  return(X_cleaned)
}

removeDuplicateCols <- function(X, verbose = 0) {
  ####### Function Description ########
  # function to remove duplicate columns
  # 
  # inputs:
  # - X = data matrix or data frame
  # - verbose = integer (0-2); level of written output
  #   
  # output: cleaned data matrix or frame without duplicate columns
  #######
  
  dup_cols <- duplicated(as.list(X))
  
  if (verbose >= 2) {
    cat("Duplicated columns: \n")
    if (!is.null(colnames(X))) {
      cat(paste(colnames(X)[dup_cols], sep = "\n"))
    } else {
      cat(paste(which(dup_cols), sep = "\n"))
    }
  }
  if (verbose >= 1) {
    cat(paste("Removed", sum(dup_cols), "features\n"))
  }
  
  X_cleaned <- X[, !dup_cols]
  
  return(X_cleaned)
}

filterByVar <- function(X, min.var, max.p) {
  ####### Function Description ########
  # function to filter number of features in data by keeping those with the
  # largest variance
  # 
  # inputs:
  # - X = n x p data matrix or data frame
  # - min.var = minimum threshold for variance
  # - max.p = maximum number of features to keep
  # 
  # output: filtered data matrix or data frame with reduced number of features
  ####### 
  
  vars <- apply(X, 2, var, na.rm = T)
  
  if (!missing(min.var)) {
    fdat <- X[, vars >= min.var]
  } else if (!missing(max.p)) {
    var_cutoff <- sort(vars, decreasing = T)[max.p]
    fdat <- X[, vars >= var_cutoff]
  } else {
    stop("Must input either min.var or max.p.")
  }
  
  return(fdat)
}