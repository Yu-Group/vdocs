plotFeatureImportance <- function(data, feature_col = "variable", 
                                  importance_col = "importance", 
                                  model_col = "model",
                                  use_rankings = FALSE,
                                  use_facets = TRUE,
                                  interactive = FALSE) {
  if (use_rankings) {
    data <- data %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(model_col))) %>%
      dplyr::mutate(.ranking = rank(-.data[[importance_col]]))
    importance_col <- ".ranking"
    importance_label <- "Importance Ranking"
  } else {
    importance_label <- "Importance Score"
  }
  if (use_facets) {
    plt <- plotBarplot(data, x_str = feature_col, y_str = importance_col,
                       stat = "identity") + 
      ggplot2::facet_wrap(~ .data[[model_col]], scales = "free")
  } else {
    plt <- plotBarplot(data, x_str = feature_col, y_str = importance_col,
                       fill_str = model_col, stat = "identity")
  }
  plt <- plt +
    simChef::pretty_ggplot_theme(x_text_angle = TRUE) +
    ggplot2::labs(x = "Feature", y = importance_label, fill = "Method")
  if (interactive) {
    plt <- plotly::ggplotly(plt)
  }
  return(plt)
}

plotFeatureImportancePair <- function(data, feature_col = "variable", 
                                      importance_col = "importance", 
                                      model_col = "model",
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
  
  data <- data %>%
    tidyr::pivot_wider(names_from = model_col, values_from = importance_col)
  plt <- plotPairs(data = data,
                   columns = which(!(colnames(data) %in% feature_col)), 
                   ...) +
    ggplot2::labs(title = plt_title)
  if (interactive) {
    plt <- plotly::ggplotly(plt)
  }
  return(plt)
}

plotFeatureImportanceStability <- function(data, feature_col = "variable", 
                                           importance_col = "importance", 
                                           model_col = "model",
                                           use_rankings = FALSE,
                                           use_facets = TRUE,
                                           interactive = FALSE) {
  if (use_rankings) {
    data <- data %>%
      dplyr::group_by(dplyr::across(c(bootstrap_id, 
                                      tidyselect::all_of(model_col)))) %>%
      dplyr::mutate(.ranking = rank(-.data[[importance_col]]))
    importance_col <- ".ranking"
    importance_label <- "Importance Ranking"
  } else {
    importance_label <- "Importance Score"
  }
  if (use_facets) {
    plt <- ggplot2::ggplot(data) +
      ggplot2::aes(x = .data[[feature_col]], y = .data[[importance_col]]) +
      ggplot2::geom_boxplot() +
      ggplot2::facet_wrap(~ .data[[model_col]], scales = "free")
  } else {
    plt <- ggplot2::ggplot(data) +
      ggplot2::aes(x = .data[[feature_col]], y = .data[[importance_col]],
                   fill = .data[[model_col]]) +
      ggplot2::geom_boxplot() +
      simChef::pretty_ggplot_fill(fill = as.factor(data[[model_col]]))
  }
  plt <- plt +
    simChef::pretty_ggplot_theme(x_text_angle = TRUE) +
    ggplot2::labs(x = "Feature", y = importance_label, fill = "Method")
  if (interactive) {
    plt <- plotly::ggplotly(plt)
  }
  return(plt)
}
