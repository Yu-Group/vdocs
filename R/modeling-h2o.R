#' @rdname fitModel
#' @export
fitH2O <- function(Xtrain, ytrain, model_name, model_options = list(), 
                   cv_options = list(), train_options = list()) {
  require(h2o)
  train_df <- as.h2o(dplyr::bind_cols(as.data.frame(Xtrain), .y = ytrain))
  
  if (identical(model_options, list())) {
    model_options <- NULL
  }
  
  if (!is.null(cv_options$nfolds) &&
      ("nfolds" %in% formalArgs(paste0("h2o.", model_name)))) {
    train_options$nfolds <- cv_options$nfolds
  }
  if (!is.null(cv_options$foldids) &&
      ("fold_column" %in% formalArgs(paste0("h2o.", model_name)))) {
    foldids <- purrr::map(cv_options$foldids, ~!(1:nrow(Xtrain) %in% .x)) %>%
      setNames(paste0("Fold", 1:length(cv_options$foldids))) %>%
      dplyr::bind_cols()
    if (any(rowSums(foldids) != 1)) {
      stop("h2o can only perform cross-validation. Each row/observation index should belong to one fold so that its index appears in all but one list element in cv_options$foldids.")
    }
    fold_column <- apply(foldids, 1, which)
    train_df <- as.h2o(dplyr::bind_cols(as.data.frame(Xtrain), .y = ytrain,
                                        .fold_column = fold_column))
    train_options$fold_column <- ".fold_column"
  }
  
  if (".tune_params" %in% names(model_options)) {
    model_options$hyper_params <- model_options$.tune_params
    model_options$.tune_params <- NULL
    fit <- do.call("h2o.grid",
                   args = c(list(x = colnames(Xtrain),
                                 y = ".y",
                                 training_frame = train_df,
                                 algorithm = model_name),
                            train_options, model_options))
    if (is.null(cv_options$metric)) {
      fit_ids <- h2o.getGrid(fit@grid_id)
    } else {
      fit_ids <- h2o.getGrid(
        fit@grid_id, 
        sort_by = cv_options$metric, 
        decreasing = cv_options$metric %in% c("auc", "accuracy", "precision",
                                              "recall", "f1"))
    }
    attr(fit, "best_fit") <- h2o.getModel(fit_ids@model_ids[[1]])
  } else {
    fit <- do.call(paste0("h2o.", model_name), 
                   args = c(list(x = colnames(Xtrain),
                                 y = ".y",
                                 training_frame = train_df),
                            model_options))
  }
  return(fit)
}

#' @rdname predictModel
#' @export
predictH2O <- function(fit, Xtest, ...) {
  require(h2o)
  
  if (inherits(fit, "H2OGrid")) {
    fit <- attr(fit, "best_fit")
  }
  
  Xtest <- as.h2o(as.data.frame(Xtest))
  pred <- h2o.predict(fit, Xtest, ...)
  if (ncol(as.data.frame(pred)) > 1) {
    pred <- tibble::tibble(predictions = as.data.frame(pred)$predict,
                           prob_predictions = as.data.frame(pred) %>%
                             dplyr::select(-predict))
  } else {
    pred <- tibble::tibble(predictions = as.data.frame(pred)$predict)
  }
  return(pred)
}

#' @rdname interpretModel
#' @export
interpretH2O <- function(fit, ...) {
  require(h2o)
  
  if (inherits(fit, "H2OGrid")) {
    fit <- attr(fit, "best_fit")
  }
  
  imp <- h2o.varimp(fit, ...) %>%
    dplyr::rename("Importance" = "relative_importance",
                  "Variable" = "variable")
  return(imp)
}

#' @rdname printFit
#' @export
printH2OFit <- function(fit) {
  cat(sprintf("Fitting time taken: %s min\n\n", attr(fit, "time_taken")))
  
  if (!is.null(attr(fit, "best_fit"))) {
    cat("=====================================================\n", 
        "================ Tuned Model Summary ================\n", 
        "=====================================================\n\n", sep = "")
    print(attr(fit, "best_fit"))
    cat("\n\n======================================================\n", 
        "==== Summary of CV Fit for Tuning Hyperparameters ====\n", 
        "======================================================\n\n", sep = "")
    print(fit)
  } else {
    cat("=====================================================\n", 
        "=================== Model Summary ===================\n", 
        "=====================================================\n\n", sep = "")
    print(fit)
  }
}