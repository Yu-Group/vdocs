fitCaret <- function(Xtrain, ytrain, Xtest, ytest, model_list, tr_control,
                     response_type) {
  
  model_fits <- list()
  model_preds <- list()
  model_errs <- list()
  model_vimps <- list()
  for (model_name in names(model_list)) {
    mod <- model_list[[model_name]]
    if (identical(mod, list())) {
      mod <- NULL
    }
    mod_fit <- do.call(caret::train, args = c(list(x = as.data.frame(Xtrain),
                                                   y = ytrain,
                                                   trControl = tr_control,
                                                   method = model_name),
                                              mod))
    model_fits[[model_name]] <- mod_fit
    model_preds[[model_name]] <- predict(mod_fit, as.data.frame(Xtest),
                                         type = response_type)
    model_errs[[model_name]] <- caret::postResample(
      pred = model_preds[[model_name]], obs = ytest
    )
    model_vimps[[model_name]] <- caret::varImp(mod_fit)
  }
  model_preds <- dplyr::bind_rows(model_preds, .id = "model")
  model_errs <- dplyr::bind_rows(model_errs, .id = "model")
  model_vimps <- purrr::map_dfr(model_vimps, 
                                ~.x[["importance"]] %>% 
                                  tibble::rownames_to_column("variable") %>%
                                  dplyr::rename("importance" = "Overall"),
                                .id = "model")
  
  return(list(errors = model_errs,
              predictions = model_preds,
              importance = model_vimps))
}

fitTidyModels <- function(Xtrain, ytrain, Xtest, ytest, model_list, 
                          kfolds = 5) {
  
  train_df <- dplyr::bind_cols(Xtrain, .y = ytrain)
  test_df <- dplyr::bind_cols(Xtest, .y = ytest)
  splits <- rsample::make_splits(
    ind = list(analysis = 1:nrow(Xtrain),
               assessment = (nrow(Xtrain) + 1):(nrow(Xtrain) + nrow(Xtest))),
    data = dplyr::bind_rows(train_df, test_df)
  )
  
  mod_recipe <- recipes::recipe(.y ~ ., data = train_df)
  
  model_fits <- list()
  model_preds <- list()
  model_errs <- list()
  model_vimps <- list()
  for (model_name in names(model_list)) {
    mod <- model_list[[model_name]]$model
    grid <- model_list[[model_name]]$grid
    if (!is.null(grid)) {
      mod_fit <- workflows::workflow() %>%
        workflows::add_recipe(mod_recipe) %>%
        workflows::add_model(mod)
      best_params <- mod_fit %>%
        tune::tune_grid(resamples = rsample::vfold_cv(train_df, v = kfolds), 
                        grid = grid) %>%
        tune::select_best(metric = "accuracy")
      mod_fit <- mod_fit %>%
        tune::finalize_workflow(best_params) %>%
        tune::last_fit(splits)
    } else {
      mod_fit <- workflows::workflow() %>%
        workflows::add_recipe(mod_recipe) %>%
        workflows::add_model(mod) %>%
        tune::last_fit(splits)
    }
    model_fits[[model_name]] <- mod_fit
    model_preds[[model_name]] <- mod_fit %>%
      tune::collect_predictions()
    model_errs[[model_name]] <- mod_fit %>%
      tune::collect_metrics()
    model_vimps[[model_name]] <- tryCatch({
      # model-specific variable importance
      mod_fit %>%
        workflows::extract_fit_parsnip() %>%
        vip::vi()
    }, error = function(e) {
      # model-agnostic permutation variable importance
      mod_fit %>%
        workflows::extract_fit_parsnip() %>%
        vip::vi(method = "permute", train = train_df, target = ".y",
                feature_names = setdiff(colnames(train_df), ".y"), 
                pred_wrapper = predict, metric = "accuracy")
    })
  }
  model_preds <- dplyr::bind_rows(model_preds, .id = "model")
  model_errs <- dplyr::bind_rows(model_errs, .id = "model") %>%
    dplyr::select(-.estimator, -.config) %>%
    tidyr::pivot_wider(names_from = .metric, values_from = .estimate)
  model_vimps <- dplyr::bind_rows(model_vimps, .id = "model")
  
  return(list(errors = model_errs,
              predictions = model_preds,
              importance = model_vimps))
}

fith2o <- function(Xtrain, ytrain, Xtest, ytest, model_list) {
  require(h2o)
  
  h2o.init(nthreads = 1)
  train_df <- as.h2o(dplyr::bind_cols(Xtrain, .y = ytrain))
  test_df <- as.h2o(dplyr::bind_cols(Xtest, .y = ytest))
  
  model_fits <- list()
  model_preds <- list()
  model_errs <- list()
  model_vimps <- list()
  for (model_name in names(model_list)) {
    mod <- model_list[[model_name]]
    if (identical(mod, list())) {
      mod <- NULL
    }
    mod_fit <- do.call(paste0("h2o.", model_name),
                       args = c(list(x = colnames(Xtrain),
                                     y = ".y",
                                     training_frame = train_df,
                                     model_id = model_name),
                                mod))
    model_fits[[model_name]] <- mod_fit
    model_preds[[model_name]] <- h2o.predict(mod_fit, test_df)
    model_errs[[model_name]] <- h2o.performance(mod_fit, test_df)
    model_vimps[[model_name]] <- h2o.varimp(mod_fit)
  }
  model_preds <- purrr::map_dfr(model_preds, ~attr(.x, "data"), .id = "model")
  model_errs <- purrr::map_dfr(
    model_errs, 
    function(err) {
      rm_objs <- c("model", "model_checksum", "frame", "frame_checksum",
                   "description", "scoring_time", "predictions")
      simChef:::simplify_tibble(simChef:::list_to_tibble_row(
        err@metrics[setdiff(names(err@metrics), rm_objs)]
      ))
    }, 
    .id = "model"
  )
  model_vimps <- dplyr::bind_rows(model_vimps, .id = "model")
  
  return(list(errors = model_errs,
              predictions = model_preds,
              importance = model_vimps))
}