#' Splits data into training, validation, and test sets
#'
#' @description Given (X, y) data, splits the data into training, validation,
#'   and test partitions according to the specified proportions. Can also
#'   perform stratified (or clustered) data splitting if provided.
#'
#' @param X A data matrix or data frame.
#' @param y A response vector.
#' @param stratified_by An optional vector of group IDs to stratify by. That is,
#'   the random paritioning occurs within each group so that the group
#'   proportions are similar across the training, validation, and test sets.
#'   Vector must be the same length as `y`. If \code{NULL} (default), the full
#'   data set is randomly partitioned into training, validation, and test sets.
#' @param train_prop Proportion of data in training set. Default is 0.6.
#' @param valid_prop Proportion of data in validation set. Default is 0.2.
#' @param test_prop Proportion of data in test set. Default is 0.2.
#'
#' @returns A list of two:
#' \describe{
#' \item{X}{A list of three data matrices or data frames named `train`,
#'   `validate`, and `test` containing the training, validation, and test X
#'   partitions, respectively.}
#' \item{y}{A list of three vectors named `train`, `validate`, and `test`
#'   containing the training, validation, and test y partitions, respectively.}
#' }
#'
#' @examples
#' # splits iris data into training (60%), validation (20%), and test (20%) sets
#' data_split <- split_data(X = iris %>% dplyr::select(-Species),
#'                          y = iris$Species,
#'                          train_prop = 0.6, valid_prop = 0.2, test_prop = 0.2)
#'
#' # splits iris data into training, validation, and test sets while keeping
#' # `Species` distribution constant across partitions
#' stratified_data_split <- split_data(X = iris %>% dplyr::select(-Species),
#'                                     y = iris$Species,
#'                                     stratified_by = iris$Species,
#'                                     train_prop = 0.6, valid_prop = 0.2,
#'                                     test_prop = 0.2)
#'
#' @export
split_data <- function(X, y, stratified_by = NULL,
                       train_prop = 0.6, valid_prop = 0.2, test_prop = 0.2) {
  .group <- NULL  # to fix no visible binding for global variable error
  .split <- NULL

  prop_vec <- c(train_prop, valid_prop, test_prop)
  prop_names_vec <- c("train", "validate", "test")
  if (all(prop_vec == 0)) {
    stop("At least one of train_prop, valid_prop, test_prop must be non-zero.")
  } else if (any(prop_vec == 0)) {
    prop_names_vec <- prop_names_vec[prop_vec != 0]
    prop_vec <- prop_vec[prop_vec != 0]
  }

  n <- nrow(X)
  if (is.null(stratified_by)) {
    split_labels <- sample(
      cut(1:n, n * cumsum(c(0, prop_vec)), labels = prop_names_vec)
    )
  } else {
    split_labels <- data.frame(.group = stratified_by) %>%
      dplyr::group_by(.group) %>%
      dplyr::mutate(
        .split = sample(
          cut(
            1:dplyr::n(),
            dplyr::n() * cumsum(c(0, prop_vec)),
            labels = prop_names_vec
          )
        )
      ) %>%
      dplyr::pull(.split)
  }
  X_split <- split(X, split_labels)
  y_split <- split(y, split_labels)
  return(list(X = X_split, y = y_split))
}

#' Basic cleaning functions to remove columns in data.
#'
#' @name remove_cols
#' @description Given data X, removes columns in X according to various
#'   data preprocessing/cleaning procedures. `remove_na_cols` removes all
#'   columns in the data with at least one NA value. `remove_constant_cols`
#'   removes all columns in the data that are a constant value (ignoring NAs).
#'   `remove_duplicate_cols` removes columns in the data that are duplicates of
#'   another column in the data so that each column in the resulting cleaned
#'   data is unique.
#'
#' @param X A data matrix or data frame.
#' @param verbose Integer (0-2) indicating the level of written output.
#'
#' @returns A cleaned data matrix or data frame.
#'
NULL

#' @rdname remove_cols
#' @export
remove_na_cols <- function(X, verbose = 0) {

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
    cat(paste("Removed", sum(col_nas > 0), "features with NAs\n"))
  }

  X_cleaned <- X[, col_nas == 0, drop = FALSE]

  return(X_cleaned)
}

#' @rdname remove_cols
#' @export
remove_constant_cols <- function(X, verbose = 0) {
  const_cols <- apply(X, 2, function(x) all(duplicated(x)[-1L]))

  if (verbose >= 2) {
    if (any(const_cols)) {
      cat("Constant columns: \n")
      if (!is.null(names(const_cols))) {
        cat(paste(names(const_cols)[const_cols], X[1, const_cols],
                  sep = " with value "), sep = "\n")
      } else {
        cat(paste(which(const_cols), X[1, const_cols],
                  sep = " with value "), sep = "\n")
      }
    }
  }
  if (verbose >= 1) {
    cat(paste("Removed", sum(const_cols), "features with constant values\n"))
  }

  X_cleaned <- X[, !const_cols, drop = FALSE]

  return(X_cleaned)
}

#' @rdname remove_cols
#' @export
remove_duplicate_cols <- function(X, verbose = 0) {

  if (is.data.frame(X)) {
    dup_cols <- duplicated(as.list(X))
  } else if (is.matrix(X)) {
    dup_cols <- duplicated(t(X))
  }

  if (verbose >= 2) {
    cat("Duplicated columns: \n")
    if (!is.null(colnames(X))) {
      cat(paste(colnames(X)[dup_cols], sep = "\n"))
    } else {
      cat(paste(which(dup_cols), sep = "\n"))
    }
  }
  if (verbose >= 1) {
    cat(paste("Removed", sum(dup_cols), "features that are duplicated\n"))
  }

  X_cleaned <- X[, !dup_cols]

  return(X_cleaned)
}

#' Filter out columns in data to reduce dimension.
#'
#' @name filter_cols
#' @description Given data X, filters out columns in X according to various
#'   data preprocessing/cleaning procedures. `filter_cols_by_var` reduces the
#'   number of features in the data by keeping those with the largest variance.
#'
#' @param X A data matrix or data frame.
#' @param min_var (Optional) minimum variance threshold. All columns with
#'   variance lower than `min_var` are removed. If \code{NULL} (default), no
#'   variance threshold is applied.
#' @param max_p (Optional) maximum number of features to keep. Only features
#'   with the top `max_p` highest variances are kept. If \code{NULL} (default),
#'   there is no limit on the maximum number of features to keep.
#'
#' @returns A cleaned data matrix or data frame.
#'
NULL

#' @rdname filter_cols
#' @export
filter_cols_by_var <- function(X, min_var = NULL, max_p = NULL) {
  if (is.null(min_var) & is.null(max_p)) {
    return(X)
  }

  vars <- apply(X, 2, stats::var, na.rm = T)

  if (!is.null(min_var)) {
    X <- X[, vars >= min_var, drop = FALSE]
    vars <- vars[vars >= min_var]
  }
  if (!is.null(max_p)) {
    if (ncol(X) > max_p) {
      var_cutoff <- sort(vars, decreasing = T)[max_p]
      X <- X[, vars >= var_cutoff, drop = FALSE]
    }
  }

  return(X)
}
