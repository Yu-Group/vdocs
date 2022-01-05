#' Wrapper for fitting models using common modeling backends
#' 
#' @name fitModels
#' @family fit_models_family
#' @description `fitModels` is a wrapper function for fitting multiple models 
#'   using caret, tidymodels, or h2o backends. This wrapper function provides 
#'   uniformity of input arguments to easily switch between the different 
#'   modeling packages.
#' 
#' @param Xtrain Training data matrix or data frame.
#' @param ytrain Training response vector.
#' @param model_list List of models to train. Each name in the list should
#'   correspond to the name of the model to fit (see caret, h2o, or tidymodels
#'   for a list of available models). Each list element is a list of named
#'   model options. See `model_options` arugument and details of `fitModel()` 
#'   for possible options and more information.
#' @param cv_options List of cross-validation options to use for tuning
#'   hyperparameters. Possible options are `nfolds` (default is 10), `foldids`,
#'   and `metric`. `nfolds` gives the number of folds in the cross-validation 
#'   scheme. `foldids` is a list with elements for each cross-validation fold, 
#'   where each list element is a vector of integers corresponding to the rows 
#'   used for training in that fold. `metric` is a string that specifies which
#'   metric to use to select the best hyperparameters. See details below.
#' @param train_options List of additional training control options. See details 
#'   of `fitModel()` for possible options and more information.
#' @param use One of "caret", "h2o", "tidymodels", indicating the modeling
#'   package to use.
#' @param verbose Level of verbosity (0-2).
#' 
#' @returns A list of the same length as `model_list` with the fitted models.
#'   See `fitModel` for details of specified outputs for each of the modeling
#'   backends.
#' 
#' @export
fitModels <- function(Xtrain, ytrain, model_list, 
                      cv_options = list(), train_options = list(), 
                      use = c("caret", "h2o", "tidymodels"),
                      verbose = 0) {
  use <- match.arg(use)
  if (any(duplicated(names(model_list)))) {
    stop("Cannot handle duplicate names in model_list. Please specify unique model names.")
  }
  if (verbose == 1) {
    start_time <- Sys.time()
  }
  fit_list <- purrr::imap(
    model_list,
    function(model_options, model_name) {
      if (use == "caret") {
        fit_func <- fitCaret
      } else if (use == "h2o") {
        fit_func <- fitH2O
      } else if (use == "tidymodels") {
        fit_func <- fitTidymodels
      }
      fit_start_time <- Sys.time()
      fit <- fit_func(Xtrain, ytrain, model_name, 
                      model_options, cv_options, train_options)
      attr(fit, "use") <- use
      attr(fit, "time_taken") <- difftime(Sys.time(), fit_start_time, 
                                          units = "mins")
      if (verbose == 2) {
        cat(sprintf("Fitting time taken for %s: %f minutes", model_name,
                    attr(fit, "time_taken")), 
            sep = "\n")
      }
      return(fit)
    }
  )
  if (verbose == 1) {
    cat(sprintf("Fitting time taken: %f minutes/n", 
                difftime(Sys.time(), start_time, units = "mins")),
        sep = "\n")
  }
  return(fit_list)
}

#' Wrapper for making predictions from fitted models using common modeling
#' backends
#' 
#' @name predictModels
#' @family predict_models_family
#' @description `predictModels` is a wrapper function for making predictions
#'   from multiple fitted models that were trained using caret, tidymodels, or 
#'   h2o backends. This wrapper function provides uniformity of input arguments 
#'   to easily switch between the different modeling packages.
#' 
#' @param fit_list List of fitted models for which to make predictions. 
#'   Typically the output of `fitModels()`.
#' @param Xtest Data matrix or data frame on which to make predictions.
#' @param options Named list of additional arguments to pass to 
#'   `predict.train()` if using caret backend, `h2o.predict()` if using h2o 
#'   backend, or `predict.workflow()` if using tidymodels backend. If different 
#'   arguments need to be passed in for different fitted models, `options` 
#'   should be a list where each element is named after the model (i.e., the
#'   same name as in `fit_list`) and is a list of named arguments. If `options`
#'   is not a list of list, then all list elements are passed to the predict
#'   function for every model.
#' @param use One of "caret", "h2o", "tidymodels", indicating the modeling
#'   package to use. If \code{NULL} (default), the modeling package is inferred
#'   from the fit.
#' 
#' @returns A tibble with the following columns:
#' \describe{
#' \item{Method}{Name of method that made the associated prediction.}
#' \item{predictions}{Raw predicted value (e.g., the predicted class in a 
#'   classification problem and the predicted continuous value in a 
#'   regression problem.)}
#' \item{prob_predictions}{In a classification problem, this is a tibble with
#'   the predicted probabilities for each class. This column is omitted for
#'   regression problems.}
#' }
#' 
#' Note that the order of the predictions within each method are aligned with 
#' that of the Xtest input.
#' 
#' @export
predictModels <- function(fit_list, Xtest, options = list(), use = NULL) {
  if (!all(names(options) %in% names(fit_list))) {
    # repeat/use same options across all models
    options_list <- purrr::map(fit_list, ~options)
  } else {
    options_list <- options
  }
  pred_list <- purrr::imap_dfr(
    fit_list,
    function(fit, fit_name) {
      options <- options_list[[fit_name]]
      if (!is.null(attr(fit, "use"))) {
        use_pkg <- attr(fit, "use")
      } else {
        use_pkg <- match.arg(use, choices = c("caret", "h2o", "tidymodels"))
      }
      if (use_pkg == "caret") {
        pred_fun <- predictCaret
      } else if (use_pkg == "h2o") {
        pred_fun <- predictH2O
      } else if (use_pkg == "tidymodels") {
        pred_fun <- predictTidymodels
      }
      preds <- do.call(pred_fun, 
                       args = c(list(fit = fit, Xtest = Xtest), options))
      return(preds)
    },
    .id = "Method"
  )
  return(pred_list)
}

#' Wrapper for evaluating predictions from fitted models using common modeling
#' backends
#' 
#' @name evaluateModels
#' @description `evaluateModels` is a wrapper function for evaluating 
#'   predictions from multiple fitted models that were trained using caret, 
#'   tidymodels, or h2o backends. This wrapper function provides uniformity of 
#'   input arguments to easily switch between the different modeling packages.
#' 
#' @param pred_df Data frame of predictions to evaluate against `ytest`. 
#'   Typically the output of `predictModels()`.
#' @param ytest Test response vector for which to evaluate against the 
#'   predictions.
#' @param metrics A `metric_set` object indicating the metrics to evaluate. See
#'   `yardstick::metric_set()` for more details. Default `NULL` will use a 
#'   default set of metrics that depends on the type of problem (e.g., 
#'   classification vs regression).
#' @param na_rm Logical indicating whether `NA` values should be stripped before
#'   the computation proceeds.
#' 
#' @returns A list with the following elements:
#' \describe{
#' \item{metrics}{Name of method that made the associated prediction.}
#' \item{conf}{In a classification problem, this is a tibble with the confusion
#'   matrices for each method (see output of `yardstick::conf_mat()`). This
#'   element is omitted for regression problems.}
#' \item{roc_plot}{In a classification problem with the predicted probabilities
#'   provided in `pred_df`, this is a ggplot object with the ROC evaluation 
#'   plot. This element is omitted for regression problems or if the predicted
#'   probabilities are not provided.}
#' \item{pr_plot}{In a classification problem with the predicted probabilities
#'   provided in `pred_df`, this is a ggplot object with the PR evaluation 
#'   plot. This elemented is omitted for regression problems or if the predicted
#'   probabilities are not provided.}
#' }
#'
#' @export
evaluateModels <- function(pred_df, ytest, metrics = NULL, na_rm = TRUE) {
  if (!is.null(metrics) && !inherits(metrics, "metric_set")) {
    stop("Unknown metrics. ",
         "metrics must be of class 'yardstick::metric_set' or NULL.")
  }
  
  prob_cols <- NULL
  if ("prob_predictions" %in% names(pred_df)) {
    prob_cols <- colnames(pred_df$prob_predictions)
    if (length(prob_cols) == 2) {
      prob_cols <- prob_cols[1]
    }
    pred_df <- pred_df %>%
      tidyr::unnest(cols = "prob_predictions")
  }
  
  pred_df <- pred_df %>%
    dplyr::mutate(y = rep(ytest, length.out = dplyr::n())) %>%
    dplyr::group_by(Method)
  
  is_class <- !is.numeric(ytest)
  if (is.null(metrics)) {
    if (!is_class) {
      metrics <- yardstick::metric_set(
        yardstick::rmse, yardstick::rsq, yardstick::mae,
        yardstick::msd, yardstick::ccc
      )
    } else if (!is.null(prob_cols)) {
      metrics <- yardstick::metric_set(
        yardstick::roc_auc, yardstick::pr_auc, yardstick::mn_log_loss,
        yardstick::sens, yardstick::spec, yardstick::f_meas, 
        yardstick::kap, yardstick::accuracy, yardstick::bal_accuracy,
        yardstick::ppv, yardstick::npv
      )
    } else {
      metrics <- yardstick::metric_set(
        yardstick::sens, yardstick::spec, yardstick::f_meas, 
        yardstick::kap, yardstick::accuracy, yardstick::bal_accuracy,
        yardstick::ppv, yardstick::npv
      )
    }
  } 
  
  if (!is_class) {
    # evaluate regression evaluation metrics
    res <- metrics(
      data = pred_df, truth = y, estimate = predictions, na_rm = na_rm
    ) %>%
      dplyr::rename(Metric = .metric) %>%
      tidyr::pivot_wider(
        id_cols = Metric, names_from = Method, values_from = .estimate, 
      )
    attr(res, "metrics_info") <- metrics
    return(list(metrics = res))
  } else {
    # evaluate classification evaluation metrics
    res <- metrics(
      data = pred_df, truth = y, estimate = predictions,
      tidyselect::all_of(prob_cols), na_rm = na_rm
    ) %>%
      dplyr::rename(Metric = .metric) %>%
      tidyr::pivot_wider(
        id_cols = Metric, names_from = Method, values_from = .estimate, 
      )
    attr(res, "metrics_info") <- metrics
      
    # evaluate confusion matrix
    conf <- yardstick::conf_mat(
      data = pred_df, truth = y, estimate = predictions
    )
    
    # evaluate roc/pr curves
    if (!is.null(prob_cols)) {
      roc_df <- yardstick::roc_curve(
        data = pred_df, truth = y, tidyselect::all_of(prob_cols)
      )
      roc_plot <- ggplot2::autoplot(roc_df) +
        ggplot2::labs(x = "FPR", y = "TPR", color = "Method") +
        simChef::pretty_ggplot_color(color = as.factor(roc_df$Method)) +
        simChef::pretty_ggplot_theme()
      pr_df <- yardstick::pr_curve(
        data = pred_df, truth = y, tidyselect::all_of(prob_cols)
      )
      pr_plot <- ggplot2::autoplot(pr_df) +
        ggplot2::labs(x = "Recall", y = "Precision", color = "Method") +
        simChef::pretty_ggplot_color(color = as.factor(pr_df$Method)) +
        simChef::pretty_ggplot_theme()
    } else {
      roc_plot <- NULL
      pr_plot <- NULL
    }
    return(list(metrics = res, conf = conf, 
                roc_plot = roc_plot, pr_plot = pr_plot))
  }
}

#' Wrapper for extracting feature importances from fitted models using common 
#' modeling backends
#' 
#' @name interpretModels
#' @family interpret_models_family
#' @description `interpretModels` is a wrapper function for extracting feature
#'   importances from multiple fitted models that were trained using caret, 
#'   tidymodels, or h2o backends. This wrapper function provides uniformity of 
#'   input arguments to easily switch between the different modeling packages.
#' 
#' @param fit_list List of fitted models for which to make predictions. 
#'   Typically the output of `fitModels()`.
#' @param options Named list of additional arguments to pass to 
#'   `varImp()` if using caret backend, `h2o.varimp()` if using h2o 
#'   backend, or `vip::vi()` if using tidymodels backend. If different 
#'   arguments need to be passed in for different fitted models, `options` 
#'   should be a list where each element is named after the model (i.e., the
#'   same name as in `fit_list`) and is a list of named arguments. If `options`
#'   is not a list of list, then all list elements are passed to the predict
#'   function for every model.
#' @param use One of "caret", "h2o", "tidymodels", indicating the modeling
#'   package to use. If \code{NULL} (default), the modeling package is inferred
#'   from the fit.
#' 
#' @returns A tibble with the following columns:
#' \describe{
#' \item{Method}{Name of method that made the associated feature importance.}
#' \item{Variable}{Name of feature/variable.}
#' \item{Importance}{Feature importance score.}
#' }
#' 
#' @export
interpretModels <- function(fit_list, options = list(), use = NULL) {
  if (!all(names(options) %in% names(fit_list))) {
    # repeat/use same options across all models
    options_list <- purrr::map(fit_list, ~options)
  } else {
    options_list <- options
  }
  
  imp_list <- purrr::imap_dfr(
    fit_list,
    function(fit, fit_name) {
      options <- options_list[[fit_name]]
      if (!is.null(attr(fit, "use"))) {
        use_pkg <- attr(fit, "use")
      } else {
        use_pkg <- match.arg(use, choices = c("caret", "h2o", "tidymodels"))
      }
      if (use_pkg == "caret") {
        imp_fun <- interpretCaret
      } else if (use_pkg == "h2o") {
        imp_fun <- interpretH2O
      } else if (use_pkg == "tidymodels") {
        imp_fun <- interpretTidymodels
      }
      imp <- do.call(imp_fun, args = c(list(fit = fit), options))
      return(imp)
    },
    .id = "Method"
  )
  return(imp_list)
}

#' Print fit results summary
#' 
#' @family print_fit_family
#' @description `printFitResults` is a wrapper function for printing a summary
#'   of the model fit from models that were trained using caret, 
#'   tidymodels, or h2o backends. This wrapper function provides uniformity of 
#'   input arguments to easily switch between the different modeling packages.
#'
#' @param fit_list List of fitted models for which to make predictions. 
#'   Typically the output of `fitModels()`.
#' @param use One of "caret", "h2o", "tidymodels", indicating the modeling
#'   package to use. If \code{NULL} (default), the modeling package is inferred
#'   from the fit.
#' @param html Logical indicating whether the output should be in html format.
#'   If \code{FALSE}, output is in latex format.
#' 
#' @returns Print fit summary to the console.
#' 
#' @export
printFitResults <- function(fit_list, use = NULL,
                            html = knitr::is_html_output()) {
  
  for (fit_idx in 1:length(fit_list)) {
    cat(sprintf("\n\n#### %s {.unnumbered}\n\n", names(fit_list)[fit_idx]))
    fit <- fit_list[[fit_idx]]
    if (!is.null(attr(fit, "use"))) {
      use_pkg <- attr(fit, "use")
    } else {
      use_pkg <- match.arg(use, choices = c("caret", "h2o", "tidymodels"))
    }
    if (use_pkg == "caret") {
      print_func <- printCaretFit
    } else if (use_pkg == "h2o") {
      print_func <- printH2OFit
    } else if (use_pkg == "tidymodels") {
      print_func <- printTidymodelsFit
    }
    simChef:::subchunkify(print_func(fit), i = chunk_idx, 
                          other_args = "results = 'markup'")
    chunk_idx <<- chunk_idx + 1
  }
  return(invisible(fit_list))
}

#' Display evaluation results summary in Rmd
#' 
#' @description `showEvalResults` transforms the evaluation results from 
#'   `evaluateModels()` for output in an R Markdown document.
#'
#' @inheritParams prettyTable
#' @param eval_results Output of `evaluateModels()`
#' @param test_set Logical indicating whether or not this evaluation is for the
#'   test set. If `FALSE`, output is assumed to be associated with validation
#'   set.
#' @param fig_height Height of plot (in inches). See `fig.height` in the knitr
#'   chunk options.
#' @param fig_width Width of plot (in inches). See `fig.width` in the knitr
#'   chunk options.
#' 
#' @returns Outputs html or latex text for the R Markdown document.
#' 
#' @export
showEvalResults <- function(eval_results, test_set = FALSE,
                            html = knitr::is_html_output(),
                            digits = 2, sigfig = TRUE, na_disp = "NA",
                            html_options = NULL, latex_options = NULL,
                            fig_height = 6, fig_width = 10) {
  results_type <- ifelse(test_set, "Test", "Validation")
  
  res_tab <- eval_results$metrics
  metrics <- attr(res_tab, "metrics_info")
  metrics_direction <- purrr::map_chr(
    res_tab$Metric,
    ~attr(attr(metrics, "metrics")[[.x]], "direction")
  )
  
  bold_func <- dplyr::case_when(
    metrics_direction == "maximize" ~ ". == max(., na.rm = TRUE)",
    metrics_direction == "minimize" ~ ". == min(., na.rm = TRUE)",
    metrics_direction == "zero" ~ "abs(.) == min(abs(.), na.rm = TRUE)"
  )
  
  res_kab <- res_tab %>%
    tibble::column_to_rownames("Metric") %>%
    prettyTable(
      html = html, bold_function = bold_func, bold_margin = 1, 
      digits = digits, sigfig = sigfig, na_disp = na_disp, 
      caption = sprintf("%s Prediction Accuracies", results_type),
      html_options = html_options, latex_options = latex_options
    )
  simChef:::subchunkify(res_kab, i = chunk_idx, other_args = "results='asis'")
  chunk_idx <<- chunk_idx + 1
  
  if ("conf" %in% names(eval_results)) {
    cat("\n\n### Confusion Tables {.unnumbered}\n\n")
    for (model_idx in 1:nrow(eval_results$conf)) {
      conf_tab <- eval_results$conf$conf_mat[[model_idx]]$table
      diag(conf_tab) <- kableExtra::cell_spec(diag(conf_tab), bold = TRUE)
      conf_kab <- as.data.frame.matrix(conf_tab) %>%
        tibble::rownames_to_column("Prediction") %>%
        ## uncomment if using kableExtra dev version from github
        # tibble::add_column(.row_head = "Prediction", 
        #                    .before = "Prediction") %>%
        # dplyr::mutate(
        #   .row_head = kableExtra::cell_spec(.row_head, angle = -90)
        # ) %>%
        simChef::pretty_kable(
          caption = sprintf("%s Confusion Matrix on %s Set", 
                            eval_results$conf$Method[[model_idx]], 
                            results_type), 
          # col.names = c(" ", " ", colnames(conf_tab)),
          format = ifelse(html, "html", "latex"), full_width = FALSE
        ) %>%
        kableExtra::add_header_above(c(" " = 1, "Truth" = ncol(conf_tab))) %>%
        # kableExtra::add_header_above(c(" " = 2, "Truth" = ncol(conf_tab))) %>%
        # kableExtra::column_spec(1, bold = TRUE, background = "white") %>%
        # kableExtra::column_spec(2, bold = TRUE) %>%
        # kableExtra::collapse_rows(columns = 1, valign = "middle",
        #                           headers_to_remove = 1)
        kableExtra::column_spec(1, bold = TRUE)
      simChef:::subchunkify(conf_kab, i = chunk_idx, 
                            other_args = "results='asis'")
      chunk_idx <<- chunk_idx + 1
    }
    if (!is.null(eval_results$roc_plot)) {
      cat("\n\n### ROC Plot {.unnumbered}\n\n")
      plt <- eval_results$roc_plot +
        ggplot2::labs(title = sprintf("%s ROC Plot", results_type))
      simChef:::subchunkify(plt, i = chunk_idx, 
                            fig_height = fig_height, fig_width = fig_width)
      chunk_idx <<- chunk_idx + 1
    }
    if (!is.null(eval_results$pr_plot)) {
      cat("\n\n### PR Plot {.unnumbered}\n\n")
      plt <- eval_results$pr_plot +
        ggplot2::labs(title = sprintf("%s PR Plot", results_type))
      simChef:::subchunkify(plt, i = chunk_idx, 
                            fig_height = fig_height, fig_width = fig_width)
      chunk_idx <<- chunk_idx + 1
    }
  }
  return(invisible(eval_results))
}
