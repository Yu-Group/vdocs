basicDataSplit <- function(X, y, train_prop = 0.6, valid_prop = 0.2, 
                             test_prop = 0.2) {
  n <- nrow(X)
  split_labels <- sample(
    cut(
      1:n, n * cumsum(c(0, train_prop, valid_prop, test_prop)), 
      labels = c("train", "validate", "test") 
    )
  )
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