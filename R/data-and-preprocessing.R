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
#' data_split <- dataSplit(X = iris %>% dplyr::select(-Species),
#'                         y = iris$Species,
#'                         train_prop = 0.6, valid_prop = 0.2, test_prop = 0.2)
#'
#' # splits iris data into training, validation, and test sets while keeping
#' # `Species` distribution constant across partitions
#' stratified_data_split <- dataSplit(X = iris %>% dplyr::select(-Species),
#'                                    y = iris$Species,
#'                                    stratified_by = iris$Species,
#'                                    train_prop = 0.6, valid_prop = 0.2,
#'                                    test_prop = 0.2)
#'
#' @export
dataSplit <- function(X, y, stratified_by = NULL,
                      train_prop = 0.6, valid_prop = 0.2, test_prop = 0.2) {
  .group <- NULL  # to fix no visible binding for global variable error
  .split <- NULL

  n <- nrow(X)
  if (is.null(stratified_by)) {
    split_labels <- sample(
      cut(
        1:n, n * cumsum(c(0, train_prop, valid_prop, test_prop)),
        labels = c("train", "validate", "test")
      )
    )
  } else {
    split_labels <- data.frame(.group = stratified_by) %>%
      dplyr::group_by(.group) %>%
      dplyr::mutate(
        .split = sample(
          cut(
            1:dplyr::n(),
            dplyr::n() * cumsum(c(0, train_prop, valid_prop, test_prop)),
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

#' Basic cleaning functions to remove columns in data.
#'
#' @name removeCols
#' @description Given data X, removes columns in X according to various
#'   data preprocessing/cleaning procedures. `removeNACols` removes all
#'   columns in the data with at least one NA value. `removeConstantCols`
#'   removes all columns in the data that are a constant value (ignoring NAs).
#'   `removeDuplicateCols` removes columns in the data that are duplicates of
#'   another column in the data so that each column in the resulting cleaned
#'   data is unique.
#'
#' @param X A data matrix or data frame.
#' @param verbose Integer (0-2) indicating the level of written output.
#'
#' @returns A cleaned data matrix or data frame.
#'
NULL

#' @rdname removeCols
#' @export
removeNACols <- function(X, verbose = 0) {

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

#' @rdname removeCols
#' @export
removeConstantCols <- function(X, verbose = 0) {
  col_vars <- apply(X, 2, stats::var, na.rm = T)

  if (verbose >= 2) {
    cat("Constant columns: \n")
    if (!is.null(names(col_vars))) {
      cat(paste(names(col_vars)[col_vars == 0], X[1, col_vars == 0],
                sep = " with value "), sep = "\n")
    } else {
      cat(paste(which(col_vars == 0), X[1, col_vars == 0],
                sep = " with value "), sep = "\n")
    }
    graphics::hist(col_vars, main = "Histogram of Column Variances",
                   xlab = "Variance")
  }
  if (verbose >= 1) {
    cat(paste("Removed", sum(col_vars == 0), "features with constant values\n"))
  }

  X_cleaned <- X[, col_vars > 0, drop = FALSE]

  return(X_cleaned)
}

#' @rdname removeCols
#' @export
removeDuplicateCols <- function(X, verbose = 0) {

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
    cat(paste("Removed", sum(dup_cols), "features that are duplicated\n"))
  }

  X_cleaned <- X[, !dup_cols]

  return(X_cleaned)
}

#' Filter out columns in data to reduce dimension.
#'
#' @name filterCols
#' @description Given data X, filters out columns in X according to various
#'   data preprocessing/cleaning procedures. `filterColsByVar` reduces the
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

#' @rdname filterCols
#' @export
filterColsByVar <- function(X, min_var = NULL, max_p = NULL) {
  if (is.null(min_var) & is.null(max_p)) {
    return(X)
  }

  vars <- apply(X, 2, stats::var, na.rm = T)

  if (!is.null(min_var)) {
    X <- X[, vars >= min_var, drop = FALSE]
    vars <- vars[vars >= min_var]
  }
  if (!is.null(max_p) & (ncol(X) > max_p)) {
    var_cutoff <- sort(vars, decreasing = T)[max_p]
    X <- X[, vars >= var_cutoff, drop = FALSE]
  }

  return(X)
}
