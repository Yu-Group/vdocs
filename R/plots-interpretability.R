#' Feature importance plots
#'
#' @name plot_feature_importance
#' @description Various plotting functions to visualize feature importances.
#'   `plot_feature_importances` creates bar plots of the feature importances
#'   across different models. `plot_feature_importance_pair` create pair plots,
#'   comparing the feature importances between all pairs of models.
#'   `plot_feature_importance_stability` creates boxplots of the
#'   distribution of feature importances across the many data perturbations for
#'   each model.
#'
#' @param data A data frame with the feature importance information. At a
#'   minimum, this data frame should contain columns with the names specified by
#'   `feature_col`, `importance_col`, and `model_col`.
#' @param feature_col Character string. Name of the column in `data` with the
#'   feature names.
#' @param importance_col Character string. Name of the column in `data` with the
#'   feature importance scores.
#' @param model_col Character string. Name of the column in `data` with the
#'   model names.
#' @param max_features Maximum number of features to display in the plot.
#' @param use_rankings Logical indicating whether or not to use the feature
#'   importance rankings, rather than the raw feature importance scores, for
#'   plotting.
#' @param use_facets Logical indicating whether or not to use faceting to
#'   separate the feature importances by model.
#' @param interactive Logical indicating whether or not to return an interactive
#'   plot.
#' @param ... Additional arguments to pass to `plot_pairs()`. Only used in
#'   `plot_feature_importance_pair()`.
#'
#' @returns If `interactive = FALSE`, returns a ggplot object. If
#'   `interactive = TRUE`, returns an interactive plotly object.
#'
NULL

#' @rdname plot_feature_importance
#' @importFrom rlang .data
#' @export
plot_feature_importance <- function(data, feature_col = "Variable",
                                  importance_col = "Importance",
                                  model_col = "Method",
                                  max_features = 50,
                                  use_rankings = FALSE,
                                  use_facets = TRUE,
                                  interactive = FALSE, ...) {
  .ranking <- NULL  # to fix no visible binding for global variable error
  .keep_features <- NULL  # to fix no visible binding for global variable error

  data <- data %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(model_col))) %>%
    dplyr::mutate(.ranking = rank(-.data[[importance_col]]))
  if (!is.null(max_features) &
      (max_features < length(unique(data[[feature_col]])))) {
    keep_features <- data %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(c(model_col,
                                                         feature_col)))) %>%
      dplyr::summarise(.keep_features = any(.ranking <= max_features),
                       .groups = "keep") %>%
      dplyr::filter(.keep_features)
    data <- data %>%
      dplyr::filter(.data[[feature_col]] %in% unique(keep_features[[feature_col]]))
  }

  if (use_rankings) {
    importance_col <- ".ranking"
    importance_label <- "Importance Ranking"
  } else {
    importance_label <- "Importance Score"
  }

  if (use_facets) {
    plt <- plot_bar(data, x_str = feature_col, y_str = importance_col,
                       stat = "identity") +
      ggplot2::facet_grid(.data[[model_col]] ~ ., scales = "free")
  } else {
    plt <- plot_bar(data, x_str = feature_col, y_str = importance_col,
                       fill_str = model_col, stat = "identity")
  }
  plt <- plt +
    vthemes::theme_vmodern(x_text_angle = TRUE) +
    ggplot2::labs(x = "Feature", y = importance_label, fill = "Method")
  if (interactive) {
    plt <- plotly::ggplotly(plt)
  }
  return(plt)
}

#' @rdname plot_feature_importance
#' @importFrom rlang .data
#' @export
plot_feature_importance_pair <- function(data, feature_col = "Variable",
                                      importance_col = "Importance",
                                      model_col = "Method",
                                      use_rankings = FALSE,
                                      interactive = FALSE, ...) {
  if (use_rankings) {
    data <- data %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(model_col))) %>%
      dplyr::mutate({{importance_col}} := rank(.data[[importance_col]]))
    plt_title <- "Comparison of Feature Importance Scores Across Methods"
  } else {
    plt_title <- "Comparison of Feature Importance Rankings Across Methods"
  }

  keep_cols <- c(feature_col, importance_col, model_col)
  data <- data %>%
    dplyr::select(tidyselect::all_of(keep_cols)) %>%
    tidyr::pivot_wider(names_from = tidyselect::all_of(model_col),
                       values_from = tidyselect::all_of(importance_col),
                       values_fill = 0)
  plt <- plot_pairs(data = data,
                   columns = which(!(colnames(data) %in% feature_col)),
                   ...) +
    ggplot2::labs(title = plt_title)
  if (interactive) {
    plt <- plotly::ggplotly(plt)
  }
  return(plt)
}

#' @rdname plot_feature_importance
#' @importFrom rlang .data
#' @export
plot_feature_importance_stability <- function(data, feature_col = "Variable",
                                           importance_col = "Importance",
                                           model_col = "Method",
                                           max_features = 50,
                                           use_rankings = FALSE,
                                           use_facets = TRUE,
                                           interactive = FALSE, ...) {
  bootstrap_id <- NULL  # to fix no visible binding for global variable error
  .ranking <- NULL
  .keep_features <- NULL

  data <- data %>%
    dplyr::group_by(dplyr::across(c(bootstrap_id,
                                    tidyselect::all_of(model_col)))) %>%
    dplyr::mutate(.ranking = rank(-.data[[importance_col]]))
  if (!is.null(max_features)) {
    keep_features <- data %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(c(model_col,
                                                         feature_col)))) %>%
      dplyr::summarise(.keep_features = mean(.ranking <= max_features) >= 0.5,
                       .groups = "keep") %>%
      dplyr::filter(.keep_features)
    data <- data %>%
      dplyr::filter(.data[[feature_col]] %in% unique(keep_features[[feature_col]]))
  }
  if (use_rankings) {
    importance_col <- ".ranking"
    importance_label <- "Importance Ranking"
  } else {
    importance_label <- "Importance Score"
  }
  if (use_facets) {
    plt <- ggplot2::ggplot(data) +
      ggplot2::aes(x = .data[[feature_col]], y = .data[[importance_col]]) +
      ggplot2::geom_boxplot() +
      ggplot2::facet_grid(.data[[model_col]] ~ ., scales = "free")
  } else {
    plt <- ggplot2::ggplot(data) +
      ggplot2::aes(x = .data[[feature_col]], y = .data[[importance_col]],
                   fill = .data[[model_col]]) +
      ggplot2::geom_boxplot() +
      vthemes::scale_fill_vmodern(discrete = TRUE)
  }
  plt <- plt +
    vthemes::theme_vmodern(x_text_angle = TRUE) +
    ggplot2::labs(x = "Feature", y = importance_label, fill = "Method")
  if (interactive) {
    plt <- plotly::ggplotly(plt)
  }
  return(plt)
}
