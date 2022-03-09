#' Wrappers for fitting a model using common modeling backends
#'
#' @name fit_model
#' @family fit_models_family
#' @description `fit_caret`, `fit_tidymodels`, and `fit_h2o` are wrappers for
#'   fitting a model using caret, tidymodels, and h2o backends,
#'   respectively. These wrapper functions provide uniformity of input
#'   arguments to easily switch between the different modeling packages
#'   (see `fit_models()`).
#'
#' @param Xtrain Training data matrix or data frame.
#' @param ytrain Training response vector.
#' @param model_name Name of model to fit. See caret, h2o, or tidymodels
#'   for a list of available models.
#' @param model_options List of named arguments to input into the model as
#'   arguments. See details below.
#' @param cv_options List of cross-validation options to use for tuning
#'   hyperparameters. Possible options are `nfolds` (default is 10), `foldids`,
#'   and `metric`. `nfolds` gives the number of folds in the cross-validation
#'   scheme. `foldids` is a list with elements for each cross-validation fold,
#'   where each list element is a vector of integers corresponding to the rows
#'   used for training in that fold. `metric` is a string that specifies which
#'   metric to use to select the best hyperparameters. See details below.
#' @param train_options List of additional training control options. See details
#'   below.
#'
#' @details
#'   To specify a set of hyperparameters to tune in the model, add an element
#'   in the `model_options` list named `.tune_params` with the list of named
#'   hyperparameters to tune. `fit_caret()` and `fit_tidymodels()` can also take
#'   in a data frame of hyperparameters to tune. In `fit_caret()`,
#'   `model_options$.tune_params` is passed to the `tuneGrid` argument in
#'   `train()`. In `fit_tidymodels()`, `model_options$.tune_params` is passed to
#'   the `grid` argument in `tune::tune_grid()`. In `fit_h2o()`,
#'   `model_options$.tune_params` is passed to the `hyper_params` argument in
#'   `h2o.grid()`.
#'
#'   For `fit_caret()`, `train_options` should be a list of named arguments to
#'   pass to `trainControl()`, and `model_options` should be a list of named
#'   arguments to pass to `train()`. Further, see the `metric` argument in
#'   `train()` for possible options to set for `cv_options$metric`.
#'
#'   For `fit_tidymodels()`, `train_options` should be a list of named arguments
#'   to pass to `tune::tune_grid()`, which is only used if hyperparameter tuning
#'   is needed. `model_options` should be a list of named arguments to pass to
#'   the `parsnip` model function given by `model_name` (e.g.,
#'   `parsnip::rand_forest()`). Note that if additional arguments need to be
#'   set in the `engine`, then `model_options$engine` can be list with these
#'   arguments (e.g.,
#'   `model_options$engine = list(engine = "ranger", importance = "impurity")`).
#'   Further, see the `metric` argument in `tune::select_best()` for possible
#'   options to set for `cv_options$metric`.
#'
#'   For `fit_h2o()`, `train_options` should be a list of named arguments to pass
#'   to `h2o.grid()`, which is only used if hyperparameter tuning is needed.
#'   `model_options` should be a list of named arguments to pass to `h2o.grid()`
#'   if tuning is needed or to the `h2o` model function given by `model_name`
#'   (e.g., `h2o.randomForest()`). Further, see the `sort_by` argument in
#'   `h2o.getGrid()` for possible options to set for `cv_options$metric`. Note
#'   that the `decreasing` argument in `h2o.getGrid()` is set to `FALSE` unless
#'   `cv_options$metric` is one of "auc", "accuracy", "precision", "recall", or
#'   "f1".
#'
#' @returns The trained model fit. Specifically, `fit_caret()` returns a trained
#'   model fit of class `train` (see output of `train()`). `fit_tidymodels()`
#'   returns a trained model fit of class `workflow` (see output of
#'   `fit-workflow`) if hyperparameter tuning is not needed. If hyperparameter
#'   tuning is performed, then `fit_tidymodels()` returns the trained CV model
#'   fit of class `tune_results` (see output of `tune::tune_grid()`) with the
#'   additional attribute "best_fit" that holds the trained finalized
#'   `workflow` fit using the best hyperparameters. `fit_h2o` returns a trained
#'   h2o model fit (see output of `h2o.[model_name]`) if hyperparameter tuning
#'   is not needed. If hyperparameter tuning is performed, then `fit_h2o()`
#'   returns the trained CV model fit (see output of `h2o.grid()`) with the
#'   additional attribute "best_fit" that holds the trained finalized h2o model
#'   fit using the best hyperparmeters.
NULL

#' Wrappers for making predictions from a fitted model using common modeling
#' backends
#'
#' @name predict_model
#' @family predict_models_family
#' @description `predict_caret`, `predict_tidymodels`, and `predict_h2o` are
#'   wrappers for making predictions from a model using caret, tidymodels, and
#'   h2o backends, respectively. These wrapper functions provide uniformity of
#'   input arguments to easily switch between the different modeling packages
#'   (see `predict_models()`).
#'
#' @param fit Model fit. Typically the output of `fit_caret()`, `fit_h2o()`, or
#'   `fit_tidymodels()`.
#' @param Xtest Data matrix or data frame on which to make predictions.
#' @param ... Additional arguments to pass to `predict.train()` in `fit_caret()`,
#'   `h2o.predict()` in `fit_h2o()`, or `predict.workflow()` in
#'   `fit_tidymodels()`.
#'
#' @returns A tibble with the following columns:
#' \describe{
#' \item{predictions}{Raw predicted value (e.g., the predicted class in a
#'   classification problem and the predicted continuous value in a
#'   regression problem.)}
#' \item{prob_predictions}{In a classification problem, this is a tibble with
#'   the predicted probabilities for each class. This column is omitted for
#'   regression problems.}
#' }
NULL

#' Wrappers for extracting feature importances from a fitted model using common
#' modeling backends
#'
#' @name interpret_model
#' @family interpret_models_family
#' @description `interpret_caret`, `interpret_tidymodels`, and `interpret_h2o` are
#'   wrappers for extracting feature importances from a fitted model using
#'   caret, tidymodels, and h2o backends, respectively. These wrapper functions
#'   provide uniformity of input arguments to easily switch between the
#'   different modeling packages (see `interpret_models()`).
#'
#' @param fit Model fit. Typically the output of `fit_caret()`, `fit_h2o()`, or
#'   `fit_tidymodels()`.
#' @param ... Additional arguments to pass to `varImp()` in `fit_caret()`,
#'   `h2o.varimp()` in `fit_h2o()`, or `vip::vi()` in `fit_tidymodels()`.
#'
#' @returns A tibble with the following columns:
#' \describe{
#' \item{Variable}{Name of feature/variable.}
#' \item{Importance}{Feature importance score.}
#' }
NULL

#' Wrappers for printing model fits from common modeling backends
#'
#' @name print_fit
#' @family print_fit_family
#' @description `print_caret_fit`, `print_tidymodels_fit`, and `print_h2o_fit` are
#'   wrappers for printing a summary of the model fit from models that were
#'   trained using caret, tidymodels, and h2o backends, respectively. These
#'   wrapper functions provide uniformity of input arguments to easily switch
#'   between the different modeling packages (see `print_fit_results()`).
#'
#' @param fit Model fit. Typically the output of `fit_caret()`, `fit_h2o()`, or
#'   `fit_tidymodels()`.
#'
#' @returns Print fit summary to the console.
NULL

#' @rdname fit_model
#' @export
fit_caret <- function(Xtrain, ytrain, model_name, model_options = list(),
                     cv_options = list(), train_options = list()) {
  if (identical(model_options, list())) {
    model_options <- NULL
  }

  if (is.null(train_options$classProbs)) {
    train_options$classProbs <- if (is.factor(ytrain)) TRUE else FALSE
  }
  if (is.null(train_options$method)) {
    train_options$method <- "cv"
  }
  if (is.null(train_options$number) && !is.null(cv_options$nfolds)) {
    train_options$number <- cv_options$nfolds
  }
  if (is.null(train_options$index) && !is.null(cv_options$foldids)) {
    train_options$index <- cv_options$foldids
  }
  train_options <- do.call(caret::trainControl, args = train_options)

  if (".tune_params" %in% names(model_options)) {
    if (is.data.frame(model_options$.tune_params)) {
      model_options$tuneGrid <- model_options$.tune_params
    } else {
      model_options$tuneGrid <- expand.grid(model_options$.tune_params)
    }
    model_options$.tune_params <- NULL

    if (is.null(model_options$metric) && !is.null(cv_options$metric)) {
      model_options$metric <- cv_options$metric
    }
  }

  fit <- do.call(caret::train,
                 args = c(list(x = as.data.frame(Xtrain),
                               y = ytrain,
                               trControl = train_options,
                               method = model_name),
                          model_options))
  return(fit)
}

#' @rdname predict_model
#' @export
predict_caret <- function(fit, Xtest, ...) {
  pred <- stats::predict(fit, as.data.frame(Xtest), type = "raw", ...)
  if (fit$modelType == "Classification") {
    prob_pred <- stats::predict(fit, as.data.frame(Xtest), type = "prob", ...)
    pred <- tibble::tibble(predictions = pred, prob_predictions = prob_pred)
  } else {
    pred <- tibble::tibble(predictions = pred)
  }
  return(pred)
}

#' @rdname interpret_model
#' @export
interpret_caret <- function(fit, ...) {
  imp <- caret::varImp(fit, ...)$importance %>%
    tibble::rownames_to_column("Variable") %>%
    dplyr::rename("Importance" = "Overall")
  return(imp)
}

#' @rdname print_fit
#' @export
print_caret_fit <- function(fit) {
  cat(sprintf("Fitting time taken: %s min\n\n", attr(fit, "time_taken")))
  print(fit)
}
