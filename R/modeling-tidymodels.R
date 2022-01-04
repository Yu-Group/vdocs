#' @rdname fitModel
#' @export
fitTidymodels <- function(Xtrain, ytrain, model_name, model_options = list(), 
                          cv_options = list(), train_options = list()) {
  require(parsnip)
  if (identical(train_options, list())) {
    train_options <- NULL
  }
  if (is.null(model_options$mode)) {
    if (is.numeric(ytrain)) {
      model_options$mode <- "regression"
    } else {
      model_options$mode <- "classification"
    }
  }
  
  if (!is.null(cv_options$nfolds)) {
    nfolds <- cv_options$nfolds
  } else {
    nfolds <- 10
  }
  foldids <- cv_options$foldids
  metric <- cv_options$metric
  
  train_df <- dplyr::bind_cols(as.data.frame(Xtrain), .y = ytrain)
  
  wf <- workflows::workflow() %>%
    workflows::add_formula(.y ~ .)
  
  if (".tune_params" %in% names(model_options)) {
    if (is.null(foldids)) {
      train_cvfolds <- rsample::vfold_cv(train_df, v = nfolds)
    } else {
      train_cv_folds <- rsample::vfold_cv(train_df, v = length(foldids))
      # assign fold ids manually
      train_cv_folds$splits <- purrr::map2(
        train_cv_folds$splits, foldids,
        function(fold, ids) {
          fold$in_id <- ids
          return(fold)
        }
      )
    }
    if (is.data.frame(model_options$.tune_params)) {
      param_grid <- model_options$.tune_params
    } else {
      param_grid <- expand.grid(model_options$.tune_params)
    }
    model_options$.tune_params <- NULL
    model_args <- purrr::map(param_grid, ~tune::tune())
    if (is.list(model_options$engine)) {
      engine_args <- model_options$engine
      model_options$engine <- NULL
      model <- do.call(model_name, args = c(model_options, model_args))
      model <- do.call("set_engine", 
                       args = c(list(object = model), engine_args))
    } else {
      model <- do.call(model_name, args = c(model_options, model_args))
    }
    wf <- wf %>% workflows::add_model(model)
    fit <- do.call(tune::tune_grid, 
                   args = c(list(object = wf,
                                 resamples = train_cvfolds,
                                 grid = param_grid),
                            train_options))
    attr(fit, "best_fit") <- wf %>%
      tune::finalize_workflow(tune::select_best(fit, metric = metric)) %>%
      fit(data = train_df)
  } else {
    if (is.list(model_options$engine)) {
      engine_args <- model_options$engine
      model_options$engine <- NULL
      model <- do.call(model_name, args = model_options)
      model <- do.call("set_engine", 
                       args = c(list(object = model), engine_args))
    } else {
      model <- do.call(model_name, args = model_options)
    }
    fit <- wf %>%
      workflows::add_model(model) %>%
      fit(data = train_df)
  }
  return(fit)
}

#' @rdname predictModel
#' @export
predictTidymodels <- function(fit, Xtest, ...) {
  if (inherits(fit, "tune_results")) {
    fit <- attr(fit, "best_fit")
  }
  
  pred <- predict(fit, as.data.frame(Xtest), ...)
  if (workflows::extract_fit_parsnip(fit)$spec$mode == "classification") {
    prob_pred <- predict(fit, as.data.frame(Xtest), type = "prob", ...)
    colnames(prob_pred) <- stringr::str_remove(colnames(prob_pred), 
                                               "^\\.pred\\_")
    pred <- tibble::tibble(predictions = pred$.pred_class, 
                           prob_predictions = prob_pred)
  } else {
    pred <- tibble::tibble(predictions = pred$.pred_class)
  }
  return(pred)
}

#' @rdname interpretModel
#' @export
interpretTidymodels <- function(fit, ...) {
  
  if (inherits(fit, "tune_results")) {
    fit <- attr(fit, "best_fit")
  }
  
  fit <- fit %>%
    workflows::extract_fit_parsnip()
  imp <- vip::vi(fit, ...)
  
  return(imp)
}

#' @rdname printFit
#' @export
printTidymodelsFit <- function(fit) {
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
    cat("\n# Metrics Summary\n")
    fit_df <- fit %>%
      dplyr::select(id, .metrics) %>%
      tidyr::unnest(.metrics) %>%
      dplyr::select(-.estimator, -.config) %>%
      tidyr::pivot_wider(names_from = "id", values_from = ".estimate")
    fit_df$Mean <- rowMeans(fit_df %>% dplyr::select(starts_with("Fold")))
    fit_df$SD <- apply(fit_df %>% dplyr::select(starts_with("Fold")), 1, sd)
    fit_df <- fit_df %>%
      dplyr::relocate(Mean, SD, .after = .metric) %>%
      dplyr::arrange(.metric)
    print(fit_df, n = nrow(fit_df))
  } else {
    cat("=====================================================\n", 
        "=================== Model Summary ===================\n", 
        "=====================================================\n\n", sep = "")
    print(fit)
  }
}
