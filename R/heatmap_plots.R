#' Plot (clustered) heatmaps using pretty ggplot theme.
#'
#' @name plotHeatmap
#' @description `plotHeatmap` generates nice heatmaps of a data matrix (or
#'   frame) with ease. `plotHclustHeatmap` provides additional functionality
#'   beyond `plotHeatmap` that allows for clustering the rows and columns of the
#'   heatmap via hierarchical clustering. `plotCorHeatmap` generates nice
#'   (clustered) heatmaps for the feature correlation matrix of X. These heatmap
#'   functions also allow for row/column groups in the plotting display, colored
#'   axis text labels, and customization of the color/theme.
#'
#' @param X Data matrix or data frame to use for heatmap.
#' @param cor_type Type of correlation. Must be one of "pearson", "kendall",
#'   or "spearman"
#' @param y_groups Data vector of group ids to use for grouping rows in heatmap.
#' @param x_groups Data vector of group ids to use for grouping columns in
#'   heatmap.
#' @param ytext_labels y-axis labels in heatmap.
#' @param xtext_labels x-axis labels in heatmap.
#' @param xytext_labels x- and y-axis labels for correlation heatmap.
#' @param ytext_num Logical; whether or not y labels are numeric/continuous.
#' @param xtext_num Logical; whether or not x labels are numeric/continuous.
#' @param ytext_colors (Optional) Data vector to use for coloring y-axis text.
#' @param xtext_colors (Optional) Data vector to use for coloring x-axis text.
#' @param xytext_colors (Optional) Data vector to use for coloring x- and y-axis
#'   text labels in correlation heatmap.
#' @param show_ytext Logical; whether or not to show y-axis text labels.
#' @param show_xtext Logical; whether or not to show x-axis text labels.
#' @param clust_x Logical; whether or not to cluster columns.
#' @param clust_y Logical; whether or not to cluster rows.
#' @param clust Logical; whether or not to cluster columns and rows in
#'   correlation heatmap
#' @param clust_x_wi_group Logical; whether or not cluster within x_groups.
#' @param clust_y_wi_group Logical; whether or not cluster within y_groups.
#' @param dist_metric_x Distance metric for clustering columns (see
#'   [stats::dist()]).
#' @param dist_metric_y Distance metric for clustering rows (see
#'   [stats::dist()]).
#' @param dist_matrix_x Distance matrix for clustering columns (optional). Must
#'   provide either \code{dist_metric} or \code{dist_mat} if \code{clust_x = T}.
#' @param dist_matrix_y Distance matrix for clustering rows (optional). Must
#'   provide either \code{dist_metric} or \code{dist_mat} if \code{clust_y = T}.
#' @param linkage_x Type of linkage for clustering columns (see
#'   [stats::hclust()]).
#' @param linkage_y Type of linkage for clustering rows (see [stats::hclust()]).
#' @param linkage Type of linkage for clustering rows and columns in
#'   correlation heatmap (see [stats::hclust()]).
#' @param center Logical; whether or not to center columns of \code{X}.
#' @param scale Logical; whether or not to scale columns of \code{X}.
#' @param z_range Vector of length 2 with the min and max of the fill color
#'   range for heatmap. Used to set bounds for fill legend.
#' @param text_size Numeric; size of text on heatmap. If \code{text_size} = 0
#'   (default), no text is shown.
#' @param y_orient One of "identity" or "ordered". If "identity", plots heatmap
#'   of \code{X} as if it is an image. If "ordered", plots first row of \code{X}
#'   at the bottom of the heatmap and the last row of \code{X} on top.
#' @param size Size argument in [ggplot2::geom_tile()] to avoid white lines in
#'   continuous heatmap.
#' @param color_by_quantile Logical; whether or not to use quantiles to
#'   construct fill color scale. Default is \code{FALSE}.
#' @param n_quantiles Number of quantiles for color scheme. Used only if
#'   \code{color_by_quantile = TRUE}.
#' @param color_scheme One of "viridis", "temperature", "cor_temperature", or a
#'   data vector of colors to use for the fill color scale.
#' @param viridis_option Argument indicating viridis palette name. Only used if
#'   `color_scheme = "viridis"`.
#' @param theme_function Function which adds theme() to ggplot object. If
#'   \code{NULL}, adds \code{simChef::pretty_ggplot_theme()} to ggplot.
#' @param show_plot Logical. Should this plot be printed? Default \code{FALSE}.
#' @param ... Other arguments to pass to \code{simChef::pretty_ggplot_theme()}
#'   or \code{theme_function()}.
#'
#' @return A ggplot object.
#'
#' @examples
#' df <- as.data.frame(matrix(1:100, nrow = 10, byrow = 10))
#' plotHeatmap(df,  y_orient = "identity")
#' plotHeatmap(df,  y_orient = "ordered")
#' plotHclustHeatmap(df)
#' plotCorHeatmap(df)
#'
NULL

#' @rdname plotHeatmap
#' @export
plotHeatmap <- function(X, y_groups = NULL, x_groups = NULL,
                        ytext_labels = rownames(X), xtext_labels = colnames(X),
                        ytext_num = FALSE, xtext_num = FALSE,
                        ytext_colors = NULL, xtext_colors = NULL,
                        show_ytext = TRUE, show_xtext = TRUE,
                        center = FALSE, scale = FALSE, z_range = NULL,
                        text_size = 0, y_orient = "identity", size = 0,
                        color_by_quantile = FALSE, n_quantiles = 5,
                        color_scheme = "viridis", viridis_option = "C",
                        theme_function = NULL, show_plot = FALSE, ...) {
  y <- NULL  # to fix no visible binding for global variable error
  x <- NULL
  x.group <- NULL
  y.group <- NULL
  fill <- NULL

  if (!all(sapply(X, is.numeric))) {
    stop("X must contain only numeric data. Please remove non-numeric columns.")
  }

  ytext_labels <- ytext_labels
  xtext_labels <- xtext_labels
  ytext_colors <- ytext_colors
  xtext_colors <- xtext_colors

  if (any(duplicated(ytext_labels)) | any(duplicated(xtext_labels))) {
    stop("xtext_labels and ytext_labels cannot contain duplicates.")
  }

  # center/scale X if specified
  if (center | scale) {
    X <- scale(X, center = center, scale = scale)
  }

  # get range
  if (is.null(z_range)) {
    z.min <- min(X, na.rm = T)
    z.max <- max(X, na.rm = T)
  } else {
    z.min <- z_range[1]
    z.max <- z_range[2]
  }

  # convert to long df to plot
  X_long <- as.data.frame(X) %>%
    stats::setNames(xtext_labels)
  if (ytext_num) {
    X_long <- X_long %>%
      dplyr::mutate(y = as.numeric(ytext_labels)) %>%
      tidyr::gather(key = "x", value = "fill", -y)
  } else {
    X_long <- X_long %>%
      dplyr::mutate(y = factor(ytext_labels, levels = rev(ytext_labels))) %>%
      tidyr::gather(key = "x", value = "fill", -y)
  }
  if (xtext_num) {
    X_long <- X_long %>% dplyr::mutate(x = as.numeric(x))
  } else {
    X_long <- X_long %>% dplyr::mutate(x = factor(x, levels = xtext_labels))
  }

  # add group labels to data frame
  if (!is.null(x_groups)) {
    X_long$x.group <- X_long %>%
      dplyr::left_join(y = data.frame(x = xtext_labels, x.group = x_groups),
                       by = "x") %>%
      dplyr::pull(x.group)
  }
  if (!is.null(y_groups)) {
    X_long$y.group <- X_long %>%
      dplyr::left_join(y = data.frame(y = ytext_labels, y.group = y_groups),
                       by = "y") %>%
      dplyr::pull(y.group)
  }

  # base plot
  if (xtext_num | ytext_num) {
    plt <- ggplot2::ggplot(X_long) +
      ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = fill, color = fill),
                         size = size) +
      ggplot2::guides(color = FALSE)
  } else {
    plt <- ggplot2::ggplot(X_long) +
      ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = fill))
  }

  # add group labels to plot
  if (!is.null(x_groups) & !is.null(y_groups)) {
    plt <- plt +
      ggplot2::facet_grid(y.group ~ x.group, space = "free", scales = "free")
  } else if (!is.null(x_groups)) {
    plt <- plt +
      ggplot2::facet_grid(~ x.group, space = "free", scales = "free")
  } else if (!is.null(y_groups)) {
    plt <- plt +
      ggplot2::facet_grid(y.group ~., space = "free", scales = "free")
  }

  # add text if specified
  if (text_size > 0) {
    plt <- plt +
      ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = fill),
                         color = "black", size = text_size)
  }

  # add theme
  if (is.null(theme_function)) {
    plt <- plt + simChef::pretty_ggplot_theme(...)
  } else {
    plt <- theme_function(plt, ...)
  }
  plt <- plt +
    ggplot2::theme(panel.spacing = grid::unit(0, "lines"),
                   panel.border = ggplot2::element_rect(color = "black",
                                                        fill = NA, size = 1))

  # add color
  if (!color_by_quantile | is.factor(X_long$fill)) {
    if (is.null(color_scheme) | identical(color_scheme, "viridis")) {
      plt <- plt +
        simChef::pretty_ggplot_fill(fill = X_long$fill,
                                    viridis = TRUE, option = viridis_option) +
        simChef::pretty_ggplot_color(color = X_long$fill,
                                     viridis = TRUE, option = viridis_option)
    } else if (identical(color_scheme, "temperature")) {
      plt <- plt +
        ggplot2::scale_fill_gradient2(
          low = "blue", high = "red", mid = "white",
          midpoint = (z.min + z.max) / 2, limit = c(z.min, z.max)
        ) +
        ggplot2::scale_color_gradient2(
          low = "blue", high = "red", mid = "white",
          midpoint = (z.min + z.max) / 2,
          limit = c(z.min, z.max)
        )
    } else if (identical(color_scheme, "cor_temperature")) {
      plt <- plt +
        ggplot2::scale_fill_gradient2(
          low = "blue", high = "red", mid = "white",
          midpoint = 0, limit = c(-1, 1)
        ) +
        ggplot2::scale_color_gradient2(
          low = "blue", high = "red", mid = "white",
          midpoint = 0, limit = c(-1, 1)
        )
    } else {
      plt <- plt +
        ggplot2::scale_fill_manual(values = color_scheme) +
        ggplot2::scale_color_manual(values = color_scheme)
    }
  } else {
    if (!is.null(color_scheme)) {
      if (color_scheme == "temperature") {
        heat_pal <- c("blue", "white", "red")
      } else {
        heat_pal <- color_scheme
      }
    } else {
      heat_pal <- viridis::viridis(n = n_quantiles, option = viridis_option)
    }
    probs <- seq(0, 1, length = length(heat_pal))
    quantiles <- stats::quantile(X_long$fill, probs, na.rm = T)
    heat_pal_values <- (quantiles - min(quantiles)) /
      (max(quantiles) - min(quantiles))
    plt <- plt +
      ggplot2::scale_fill_gradientn(values = heat_pal_values,
                                    colors  = heat_pal) +
      ggplot2::scale_color_gradientn(values = heat_pal_values,
                                     colors = heat_pal)
  }

  # y axis orientation
  if (!xtext_num) {
    plt <- plt + ggplot2::scale_x_discrete(expand = c(0, 0))
  } else {
    plt <- plt + ggplot2::scale_x_continuous(expand = c(0, 0))
  }
  if (!ytext_num) {
    if (identical(y_orient, "identity")) {
      plt <- plt + ggplot2::scale_y_discrete(expand = c(0, 0))
    } else if (identical(y_orient, "ordered")) {
      plt <- plt + ggplot2::scale_y_discrete(expand = c(0, 0), limits = rev)
    }
  } else {
    plt <- plt + ggplot2::scale_y_continuous(expand = c(0, 0))
  }

  # add color to x and y axis text
  plt <- plt %>%
    add_axis_text_colors(labels = ytext_labels, label_colors = ytext_colors,
                         axis = "y") %>%
    add_axis_text_colors(labels = xtext_labels, label_colors = xtext_colors,
                         axis = "x")

  # remove axis text
  if (!show_xtext) {
    plt <- plt +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank())
  }
  if (!show_ytext) {
    plt <- plt +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
  }

  if (show_plot) {
    print(plt)
  }
  return(plt)
}

#' @rdname plotHeatmap
#' @export
plotHclustHeatmap <- function(X, y_groups = NULL, x_groups = NULL,
                              ytext_labels = rownames(X),
                              xtext_labels = colnames(X),
                              ytext_num = FALSE, xtext_num = FALSE,
                              ytext_colors = NULL, xtext_colors = NULL,
                              show_ytext = TRUE, show_xtext = TRUE,
                              clust_x = TRUE, clust_y = TRUE,
                              clust_x_wi_group = TRUE, clust_y_wi_group = TRUE,
                              dist_metric_x = "euclidean",
                              dist_metric_y = "euclidean",
                              dist_matrix_x = NULL, dist_matrix_y = NULL,
                              linkage_x = "ward.D", linkage_y = "ward.D",
                              center = FALSE, scale = FALSE, z_range = NULL,
                              text_size = 0, size = 0,
                              color_by_quantile = FALSE, n_quantiles = 5,
                              color_scheme = "viridis", viridis_option = "C",
                              theme_function = NULL, show_plot = FALSE, ...) {

  if (!all(sapply(X, is.numeric))) {
    stop("X must contain only numeric data. Please remove non-numeric columns.")
  }
  if (any(is.na(X)) & (clust_x | clust_y)) {
    stop("NAs found in data. Please remove NAs.")
  }

  ytext_labels <- ytext_labels
  xtext_labels <- xtext_labels
  ytext_colors <- ytext_colors
  xtext_colors <- xtext_colors

  if (center | scale) {
    X <- scale(X, center = center, scale = scale)
  }

  # helper function to do the clustering and returns index order
  get_clust_order <- function(dist_mat, dist_metric, linkage, groups,
                              clust_wi_group, axis = c("x", "y")) {
    axis <- match.arg(axis)
    if (is.null(dist_mat)) {
      if (identical(axis, "x")) {
        Dmat <- stats::dist(t(X), method = dist_metric)
      } else if (identical(axis, "y")) {
        Dmat <- stats::dist(X, method = dist_metric)
      }
    } else {
      if (!("dist" %in% class(dist_mat))) {
        Dmat <- stats::as.dist(dist_mat)
      } else {
        Dmat <- dist_mat
      }
    }
    if (is.null(groups)) {
      hclust_out <- stats::hclust(Dmat, method = linkage)
      order_idx <- hclust_out$order
    } else if (!clust_wi_group) {
      hclust_out <- stats::hclust(Dmat, method = linkage)
      order_idx <- hclust_out$order
    } else {
      order_idx <- c()
      for (group in unique(groups)) {
        group_idx <- groups == group
        if (sum(group_idx) > 1) {
          Dmat_sub <- stats::as.dist(as.matrix(Dmat)[group_idx, group_idx])
          hclust_out <- stats::hclust(Dmat_sub, method = linkage)
          col_idx <- data.frame(idx = 1:sum(group_idx), col = which(group_idx))
          order_idx <- c(order_idx,
                         col_idx$col[match(hclust_out$order, col_idx$idx)])
        } else {
          order_idx <- c(order_idx, which(group_idx))
        }
      }
    }
    return(order_idx)
  }

  if (clust_x) {
    order_idx <- get_clust_order(dist_matrix_x, dist_metric_x, linkage_x,
                                 x_groups, clust_x_wi_group, axis = "x")
    X <- X[, order_idx]
    x_groups <- x_groups[order_idx]
    xtext_labels <- xtext_labels[order_idx]
    xtext_colors <- xtext_colors[order_idx]
  }
  if (clust_y) {
    order_idx <- get_clust_order(dist_matrix_y, dist_metric_y, linkage_y,
                                 y_groups, clust_y_wi_group, axis = "y")
    X <- X[order_idx, ]
    y_groups <- y_groups[order_idx]
    ytext_labels <- ytext_labels[order_idx]
    ytext_colors <- ytext_colors[order_idx]
  }

  plt <- plotHeatmap(X = X, y_groups = y_groups, x_groups = x_groups,
                     ytext_labels = ytext_labels, xtext_labels = xtext_labels,
                     ytext_num = ytext_num, xtext_num = xtext_num,
                     ytext_colors = ytext_colors, xtext_colors = xtext_colors,
                     show_ytext = show_ytext, show_xtext = show_xtext,
                     z_range = z_range, text_size = text_size,
                     y_orient = "identity", size = size,
                     color_by_quantile = color_by_quantile,
                     n_quantiles = n_quantiles,
                     color_scheme = color_scheme,
                     viridis_option = viridis_option,
                     theme_function = theme_function,
                     show_plot = show_plot, ...)
  return(plt)
}

#' @name plotHeatmap
#' @export
plotCorHeatmap <- function(X, cor_type = "pearson",
                           xytext_labels = colnames(X), xytext_colors = NULL,
                           show_ytext = TRUE, show_xtext = TRUE,
                           clust = TRUE, linkage = "ward.D",
                           z_range = c(-1, 1), text_size = 0,
                           color_by_quantile = FALSE, n_quantiles = 5,
                           color_scheme = "cor_temperature",
                           viridis_option = "C",
                           theme_function = NULL, show_plot = FALSE, ...) {

  if (!all(sapply(X, is.numeric))) {
    stop("X must contain only numeric data. Please remove non-numeric columns.")
  }

  xytext_labels <- xytext_labels
  xytext_colors <- xytext_colors

  cor_mat <- stats::cor(X, method = cor_type, use = "pairwise.complete.obs")
  cor_dist <- stats::as.dist(1 - abs(cor_mat))

  # cluster
  if (clust) {
    hclust_out <- stats::hclust(cor_dist, method = linkage)
    cor_mat <- cor_mat[hclust_out$order, hclust_out$order]
    xytext_labels <- xytext_labels[hclust_out$order]
    xytext_colors <- xytext_colors[hclust_out$order]
  }

  cor_mat <- round(cor_mat, 2)

  plt <- plotHeatmap(X = cor_mat,
                     ytext_labels = xytext_labels, xtext_labels = xytext_labels,
                     ytext_colors = xytext_colors, xtext_colors = xytext_colors,
                     show_ytext = show_ytext, show_xtext = show_xtext,
                     z_range = z_range, text_size = text_size,
                     y_orient = "identity",
                     color_by_quantile = color_by_quantile,
                     n_quantiles = n_quantiles,
                     color_scheme = color_scheme,
                     viridis_option = viridis_option,
                     theme_function = theme_function,
                     show_plot = show_plot, ...)
  return(plt)
}
