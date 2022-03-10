#' Plots summary of data split distributions
#'
#' @description Given the training, validation, and test data, plots a summary
#'   of the feature distributions (either together or separately per feature)
#'   to quickly examine any distributional shifts between partitions. Only
#'   continuous (i.e., numeric) and categorical (i.e., character or factor)
#'   features are used for plotting.
#'
#' @param train Training data matrix, data frame, or vector.
#' @param valid Validation data matrix, data frame, or vector.
#' @param test Test data matrix, data frame, or vector.
#' @param by_feature Logical. If \code{TRUE}, plots distributions for each
#'   feature separately. If \code{FALSE}, plots distribution of all features
#'   together. Default is \code{TRUE} if there are <10 features and \code{FALSE}
#'   otherwise.
#' @param plot_type Type of plot. Default is "auto", which uses a kernel
#'   density plot for continuous features and a bar plot for categorical
#'   features. If not "auto", `plot_type` should be a list with two named
#'   elements: `continuous` and `categorical`. The `continuous` element must be
#'   one of "density", "histogram", and "boxplot" while the `categorical`
#'   element must be "bar" (with more options to come), indicating the type of
#'   plot to use for continuous and categorical features, respectively.
#' @param xlab X-axis label.
#' @param title Plot title.
#' @param plot_heights (Optional) numeric vector of relative row heights of
#'   subplots. Only used if both continuous and categorical features are found
#'   in the data. For example, heights = c(2, 1) would make the first row twice
#'   as tall as the second row.
#' @param theme_options (Optional) list of arguments to pass to
#'   vthemes::theme_vmodern().
#' @param ... Additional arguments to pass to ggplot2::geom_*().
#'
#' @returns A ggplot object.
#'
#' @export
plot_data_split <- function(train = NULL, valid = NULL, test = NULL,
                            by_feature = NULL, plot_type = "auto",
                            xlab = "Value", title = NULL,
                            plot_heights = 1, theme_options = NULL, ...) {
  .split <- NULL  # to fix no visible binding for global variable error

  if (is.null(train) & is.null(valid) & is.null(test)) {
    stop("Must specify at least one of train, valid, or test data sets.")
  }

  if (identical(plot_type, "auto")) {
    plot_type <- list(continuous = "density",
                      categorical = "bar")
  } else if (!is.list(plot_type)) {
    stop("plot_type must be either 'auto' or a list. ",
         "See ? plot_data_split for details.")
  } else {
    plot_type$categorical <- match.arg(plot_type$categorical, choices = "bar")
    plot_type$continuous <- match.arg(
      plot_type$continuous,
      choices = c("density", "histogram", "boxplot")
    )
  }

  if (is.null(dim(train))) {
    train <- data.frame(.y = train)
    valid <- data.frame(.y = valid)
    test <- data.frame(.y = test)
    by_feature <- FALSE
  } else {
    train <- as.data.frame(train)
    valid <- as.data.frame(valid)
    test <- as.data.frame(test)
    if (is.null(by_feature)) {
      by_feature <- if (ncol(train) < 10) TRUE else FALSE
    }
  }

  data <- dplyr::bind_rows(train, valid, test) %>%
    dplyr::mutate(.split = c(rep("Training", nrow(train)),
                             rep("Validation", nrow(valid)),
                             rep("Test", nrow(test))))

  dtypes <- sapply(train, class)
  ncols_continuous <- sum(dtypes == "numeric")
  ncols_categorical <- sum(dtypes %in% c("factor", "character"))

  data_ls <- NULL
  if (ncols_continuous > 0) {
    data_ls$continuous <- data %>%
      tidyr::pivot_longer(
        cols = c(where(is.numeric), -.split),
        names_to = "name", values_to = "value"
      )
  }
  if (ncols_categorical > 0) {
    data_ls$categorical <- data %>%
      tidyr::pivot_longer(
        cols = c(where(~is.character(.x) | is.factor(.x)),
                 -.split),
        names_to = "name", values_to = "value"
      )
  }

  if (is.null(title)) {
    if (by_feature) {
      title <- "Feature Distribution"
    } else {
      title <- "Overall Distribution"
    }
  }

  plt_ls <- purrr::map(
    names(data_ls),
    function(dtype) {
      if (plot_type[[dtype]] == "histogram") {
        plt <- plot_histogram(data = data_ls[[dtype]],
                             x_str = "value", fill_str = ".split",
                             theme_options = theme_options, ...) +
          ggplot2::labs(x = xlab, y = "Frequency", fill = "Split",
                        title = paste(title, "(Continuous)"))
      } else if (plot_type[[dtype]] == "density") {
        plt <- plot_density(data = data_ls[[dtype]],
                           x_str = "value", fill_str = ".split",
                           theme_options = theme_options, ...) +
          ggplot2::labs(x = xlab, y = "Density", fill = "Split",
                        title = paste(title, "(Continuous)"))
      } else if (plot_type[[dtype]] == "boxplot") {
        plt <- plot_boxplot(data = data_ls[[dtype]],
                           y_str = "value", fill_str = ".split",
                           theme_options = theme_options, ...) +
          ggplot2::labs(y = xlab, y = "", fill = "Split",
                        title = paste(title, "(Continuous)"))
      } else if (plot_type[[dtype]] == "bar") {
        if (length(unique(data_ls[[dtype]]$name)) > 1) {
          facet_formula <- name ~ .split
        } else {
          facet_formula <- ~ .split
        }
        plt <- plot_bar(data = data_ls[[dtype]], x_str = "value",
                           theme_options = theme_options, ...) +
          ggplot2::facet_grid(facet_formula, scales = "free") +
          ggplot2::labs(x = xlab, y = "Frequency",
                        title = paste(title, "(Categorical)"))
        return(plt)
      }
      if (by_feature) {
        plt <- plt + ggplot2::facet_wrap(~ name, scales = "free")
      }
      return(plt)
    }
  )

  if (length(plt_ls) > 1) {
    plt <- cowplot::plot_grid(plotlist = plt_ls, nrow = length(plt_ls),
                              ncol = 1, rel_heights = plot_heights)
  } else {
    plt <- plt_ls[[1]] + ggplot2::labs(title = title)
  }

  return(plt)
}

#' Plots summary of data distribution
#'
#' @description Plots a summary of the feature distributions (either together or
#'   separately per feature) in the data. Only continuous (i.e., numeric) and
#'   categorical (i.e., character or factor) features are used for plotting.
#'
#' @inheritParams plot_data_split
#' @param data A data matrix, data frame, or vector.
#'
#' @returns A ggplot object.
#'
#' @export
plot_data_distribution <- function(data, by_feature = NULL, plot_type = "auto",
                                 xlab = "Value", title = NULL,
                                 plot_heights = 1, theme_options = NULL, ...) {
  if (identical(plot_type, "auto")) {
    plot_type <- list(continuous = "density",
                      categorical = "bar")
  } else if (!is.list(plot_type)) {
    stop("plot_type must be either 'auto' or a list. ",
         "See ? plot_data_split for details.")
  } else {
    plot_type$categorical <- match.arg(plot_type$categorical, choices = "bar")
    plot_type$continuous <- match.arg(
      plot_type$continuous,
      choices = c("density", "histogram", "boxplot")
    )
  }

  if (is.null(dim(data))) {
    data <- data.frame(.data = data)
    by_feature <- FALSE
  } else {
    data <- as.data.frame(data)
    if (is.null(by_feature)) {
      by_feature <- if (ncol(data) < 10) TRUE else FALSE
    }
  }

  dtypes <- sapply(data, class)
  ncols_continuous <- sum(dtypes == "numeric")
  ncols_categorical <- sum(dtypes %in% c("factor", "character"))

  data_ls <- NULL
  if (ncols_continuous > 0) {
    data_ls$continuous <- data %>%
      tidyr::pivot_longer(
        cols = where(is.numeric),
        names_to = "name", values_to = "value"
      )
  }
  if (ncols_categorical > 0) {
    data_ls$categorical <- data %>%
      tidyr::pivot_longer(
        cols = where(~is.character(.x) | is.factor(.x)),
        names_to = "name", values_to = "value"
      )
  }

  if (is.null(title)) {
    if (by_feature) {
      title <- "Feature Distribution"
    } else {
      title <- "Overall Distribution"
    }
  }

  plt_ls <- purrr::map(
    names(data_ls),
    function(dtype) {
      if (plot_type[[dtype]] == "histogram") {
        plt <- plot_histogram(data = data_ls[[dtype]], x_str = "value",
                             theme_options = theme_options, ...) +
          ggplot2::labs(x = xlab, y = "Frequency",
                        title = paste(title, "(Continuous)"))
      } else if (plot_type[[dtype]] == "density") {
        plt <- plot_density(data = data_ls[[dtype]], x_str = "value",
                           theme_options = theme_options, ...) +
          ggplot2::labs(x = xlab, y = "Density",
                        title = paste(title, "(Continuous)"))
      } else if (plot_type[[dtype]] == "boxplot") {
        plt <- plot_boxplot(data = data_ls[[dtype]], y_str = "value",
                           theme_options = theme_options, ...) +
          ggplot2::labs(y = xlab, x = "",
                        title = paste(title, "(Continuous)"))
      } else if (plot_type[[dtype]] == "bar") {
        plt <- plot_bar(data = data_ls[[dtype]], x_str = "value",
                           theme_options = theme_options, ...) +
          ggplot2::labs(x = xlab, y = "Frequency",
                        title = paste(title, "(Categorical)"))
      }
      if (by_feature) {
        plt <- plt + ggplot2::facet_wrap(~ name, scales = "free")
      }
      return(plt)
    }
  )

  if (length(plt_ls) > 1) {
    plt <- cowplot::plot_grid(plotlist = plt_ls, nrow = length(plt_ls),
                              ncol = 1, rel_heights = plot_heights)
  } else {
    plt <- plt_ls[[1]] + ggplot2::labs(title = title)
  }

  return(plt)
}

#' Plots heatmap of (X, y) data
#'
#' @description Returns a heatmap of the X data alongside the response values y.
#'   Here, the fill colors in the heatmap correspond to the values in X.
#'   If the response vector y is categorical, the rows/samples in X are grouped
#'   into their respective y categories. If the response vector is continuous,
#'   a scatter plot of the y values are plotted to the right of the heatmap.
#'   Furthermore, the rows and columns of X can be clustered via hierarchical
#'   clustering if desired.
#'
#' @param X Data matrix or data frame to use for heatmap.
#' @param y A response vector.
#' @param subsample_rows Proportion of rows to subsample and keep in plotting
#'   display. Default is 1, which does not perform any subsampling.
#' @param subsample_cols Proportion of columns to subsample and keep in plotting
#'   display. Default is 1, which does not perform any subsampling.
#' @param clust_rows Logical indicating whether or not to cluster the rows in X
#'   via hierarchical clustering.
#' @param clust_cols Logical indicating whether or not to cluster the columns
#'   in X via hierarchical clustering.
#' @param ... Additional arguments to pass to plot_hclust_heatmap()
#'
#' @returns A gglot object. Specifically, if y is a continuous response vector,
#'   then this function returns a `patchwork` object with two ggplots: one for
#'   the heatmap of X and another for the scatter plot of y values. If y is
#'   categorical, this function returns a single ggplot object with the
#'   heatmap of X with samples grouped by each category in y.
#'
#' @export
plot_data_heatmap <- function(X, y, subsample_rows = 1, subsample_cols = 1,
                            clust_rows = TRUE, clust_cols = TRUE, ...) {
  x <- NULL  # to fix no visible binding for global variable error
  if (missing(X)) {
    stop("Must provide X data.")
  }
  if (missing(y)) {
    stop("Must provide y data.")
  }

  if (subsample_rows < 1) {
    keep_rows <- sample(1:nrow(X), size = round(nrow(X) * subsample_rows))
    X <- X[keep_rows, , drop = FALSE]
    y <- y[keep_rows]
  }
  if (subsample_cols < 1) {
    keep_cols <- sample(1:ncol(X), size = round(ncol(X) * subsample_cols))
    X <- X[, keep_cols, drop = FALSE]
  }

  if (is.factor(y) | is.character(y)) {
    y_groups <- y
  } else {
    y_groups <- NULL
    if (!clust_rows) {  # if not clustering rows, then order them by y
      X <- X[order(y), ]
      y <- y[order(y)]
    }
  }

  plt <- plot_hclust_heatmap(X, y_groups = y_groups,
                           clust_x = clust_cols, clust_y = clust_rows, ...) +
    ggplot2::labs(x = "Feature", y = "Sample", fill = "Value")

  if (is.null(y_groups)) {  # plot y values to the right
    plt <- plt +
      ggplot2::theme(legend.position = "bottom")
    y_plt <- ggplot2::ggplot(data.frame(x = 1:length(y), y = y)) +
      ggplot2::aes(x = y, y = x) +
      ggplot2::geom_point() +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      vthemes::theme_vmodern() +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
    plt <- patchwork::wrap_plots(plt, y_plt, ncol = 2, nrow = 1,
                                 widths = c(4, 1))
  }

  return(plt)
}

#' Plot pretty pair plots using custom ggplot theme.
#'
#' @description Plots pretty pair plots using a custom ggplot theme and provides
#'   additional functionality beyond [GGally::ggpairs()] such as the ability to
#'   add two different color schemes (one for the lower triangular subpanels and
#'   another for the upper triangular subpanels).
#'
#' @param data Data frame to use for plot.
#' @param columns Vector of column indicies or column names to plot.
#' @param color (Optional) Data vector to use as colors for lower ggplot
#'   panels.
#' @param color_upper (Optional) Data vector to use as colors for upper ggplot
#'   panels.
#' @param color_label Character string. Label for color legend title (used
#'   in lower ggplot panels).
#' @param color_upper_label Character string for color_upper legend title (used
#'   in upper ggplot panels).
#' @param color_scheme (Optional) Vector of colors to set manual color
#'   scheme corresponding to color_lower argument (i.e., the color scheme in the
#'   lower panels). If \code{NULL} (default), viridis color scheme is used.
#' @param color_scheme_upper (Optional) Vector of colors to set manual color
#'   scheme corresponding to color_upper argument (i.e., the color scheme in the
#'   upper panels). If \code{NULL} (default), viridis color scheme is used.
#' @param column_labels Label names to be displayed on strips.
#' @param point_size Point size for [ggplot2::geom_point()].
#' @param point_alpha Alpha value for [ggplot2::geom_point()].
#' @param cor_text_size Size of correlation text.
#' @param show_upper Logical. Should we show subplots in upper panels?
#' @param subsample Proportion of rows to subsample and plot.
#' @param title Character string. Title of plot.
#' @param drop Logical. Whether or not to drop factors with no observations.
#' @param theme_function function which adds theme() to ggpairs() object. If
#'   \code{NULL}, add \code{vthemes::theme_vmodern()} to
#'   [GGally::ggpairs()] object.
#' @param show_plot Logical. Should this plot be printed? Default \code{FALSE}.
#' @param ... Other arguments to pass to \code{vthemes::theme_vmodern()}
#'   or \code{theme_function()}
#'
#' @return A [GGally::ggpairs] object.
#'
#' @examples
#' plot_pairs(data = iris, columns = 1:ncol(iris), color = iris$Species)
#'
#' @importFrom rlang .data
#' @export
plot_pairs <- function(data, columns, color = NULL, color_upper = NULL,
                      color_label = "", color_upper_label = "",
                      color_scheme = NULL, color_scheme_upper = NULL,
                      column_labels = NULL, title = "",
                      point_size = .5, point_alpha = .5, cor_text_size = 3.5,
                      subsample = 1, show_upper = TRUE, drop = TRUE,
                      theme_function = NULL, show_plot = FALSE, ...) {
  legend_x <- NULL  # to fix no visible binding for global variable error
  plt_color <- NULL
  legend_y <- NULL

  # convert character color vectors to factor for ggplot color/fill aesthetic
  if (!is.null(color)) {
    if (is.character(color)) {
      color <- as.factor(color)
    }
  }
  if (!is.null(color_upper)) {
    if (is.character(color_upper)) {
      color_upper <- as.factor(color_upper)
    }
  }
  # check if color and color_upper are identical
  if (!is.null(color) & !is.null(color_upper)) {
    if (identical(color, color_upper)) {
      color_upper <- NULL
    }
  }

  # adding labels for colors
  plt_df <- as.data.frame(data)
  if (!is.null(color)) {
    plt_df <- plt_df %>%
      dplyr::mutate(color = color)
  }
  if (!is.null(color_upper)) {
    plt_df <- plt_df %>%
      dplyr::mutate(color_upper = color_upper)
  }

  # subsample points
  if (subsample != 1) {
    plt_df <- dplyr::slice_sample(plt_df, prop = subsample, replace = FALSE)
  }

  # check if show correlations in upper panel
  if (show_upper) {
    plot_cor <- GGally::wrap("cor", size = cor_text_size)
  } else {
    plot_cor <- "blank"
  }

  # get columns for plotting
  if (!is.numeric(columns)) {
    columns <- which(colnames(data) %in% columns)
  }
  # get column labels for plotting
  if (is.null(column_labels)) {
    column_labels <- colnames(data[, columns, drop = FALSE])
  }

  # helper variables
  plot_points <- GGally::wrap("points", size = point_size, alpha = point_alpha)
  plot_density <- GGally::wrap("densityDiag", alpha = .5)

  if (is.null(color) & is.null(color_upper)) {  # no colors
    plt <- GGally::ggpairs(
      data = plt_df,
      columns = columns,
      diag = list(continuous = plot_density),
      lower = list(continuous = plot_points),
      upper = list(continuous = plot_cor),
      title = title,
      columnLabels = column_labels
    )
    if (is.null(theme_function)) {
      plt <- plt + vthemes::theme_vmodern(...)
    } else {
      plt <- theme_function(plt, ...)
    }

  } else if (is.null(color_upper)) {  # one color
    # grab subplot for legend
    if (length(columns) == 1) {
      if (is.factor(color)) {
        legend <- c(1, 1)
      } else {
        legend <- NULL
      }
    } else {
      legend_plt <- data.frame(color = color, legend_x = 1) %>%
        ggplot2::ggplot() +
        ggplot2::aes(x = legend_x, y = legend_x, color = color) +
        ggplot2::geom_point() +
        ggplot2::labs(color = color_label)
      if (is.null(color_scheme)) {
        legend_plt <- legend_plt +
          vthemes::scale_color_vmodern(discrete = !is.numeric(color),
                                       drop = drop)
      } else {
        legend_plt <- legend_plt +
          ggplot2::scale_color_manual(values = color_scheme, drop = drop)
      }
      if (is.null(theme_function)) {
        legend_plt <- legend_plt + vthemes::theme_vmodern(...)
      } else {
        legend_plt <- theme_function(legend_plt, ...)
      }
      legend <- GGally::grab_legend(legend_plt)
    }

    if (is.factor(color)) {
      upper_ls <- list(continuous = plot_cor)
    } else {
      if (show_upper) {
        upper_ls <- list(continuous = plot_points)
      } else {
        upper_ls <- list(continuous = "blank")
      }
    }
    plt <- GGally::ggpairs(
      data = plt_df,
      columns = columns,
      mapping = ggplot2::aes(color = color),
      diag = list(continuous = plot_density),
      lower = list(continuous = plot_points),
      upper = upper_ls,
      title = title,
      legend = legend,
      columnLabels = column_labels
    )

    # change color palette for all panels
    for (i in 1:plt$nrow) {
      for (j in 1:plt$ncol) {
        if (is.null(color_scheme)) {
          plt[i, j] <- plt[i, j] +
            vthemes::scale_color_vmodern(discrete = !is.numeric(color),
                                         drop = drop) +
            vthemes::scale_fill_vmodern(discrete = !is.numeric(color),
                                        drop = drop)
        } else {
          plt[i, j] <- plt[i, j] +
            ggplot2::scale_color_manual(values = color_scheme, drop = drop) +
            ggplot2::scale_fill_manual(values = color_scheme, drop = drop)
        }
      }
    }

    plt <- plt + ggplot2::labs(color = color_label, fill = color_label)
    if (is.null(theme_function)) {
      plt <- plt + vthemes::theme_vmodern(...)
    } else {
      plt <- theme_function(plt, ...)
    }

  } else {
    # make lower scatter plots and color by color for the ggpairs plots
    lowerContinuous <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      p <- ggplot2::ggplot(data) +
        ggplot2::aes(x = .data[[x_str]], y = .data[[y_str]],
                     color = color) +
        ggplot2::geom_point(size = point_size, alpha = point_alpha)
      return(p)
    }

    # make upper scatter plots and color by color_upper for the ggpairs plots
    upperContinuous <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      p <- ggplot2::ggplot(data) +
        ggplot2::aes(x = .data[[x_str]], y = .data[[y_str]],
                     color = color_upper) +
        ggplot2::geom_point(size = point_size, alpha = point_alpha)
      return(p)
    }

    # make upper boxplots and color by color_upper for the ggpairs plots
    upperCombo <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      if (is.factor(data[[x_str]])) {
        p <- ggplot2::ggplot(data) +
          ggplot2::aes(x = .data[[x_str]], y = .data[[y_str]],
                       fill = color_upper) +
          ggplot2::geom_boxplot()
      } else {
        p <- ggplot2::ggplot(data) +
          ggplot2::aes(x = .data[[y_str]], y = .data[[x_str]],
                       fill = color_upper) +
          ggplot2::geom_boxplot() +
          ggplot2::coord_flip()
      }
      return(p)
    }

    # make upper bar plots and color by color_upper for the ggpairs plots
    upperDiscrete <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      p <- ggplot2::ggplot(data) +
        ggplot2::aes(x = .data[[x_str]], fill = color_upper) +
        ggplot2::facet_grid(.data[[y_str]] ~ .) +
        ggplot2::geom_bar()
      return(p)
    }

    # number of color factors
    nfactors <- is.factor(color) + is.factor(color_upper)

    if (length(columns) == 1) {  # in this case, plot density
      if (nfactors == 0) {
        plt <- GGally::ggpairs(data = plt_df, columns = columns,
                               title = title, legend = c(1, 1),
                               columnLabels = column_labels) +
          ggplot2::labs(color = color_label, fill = color_label)
      } else {
        if (is.factor(color)) {
          plt_df$plt_color <- color
        } else {
          plt_df$plt_color <- color_upper
        }
        plt <- GGally::ggpairs(
          data = plt_df,
          columns = columns,
          mapping = ggplot2::aes(color = plt_color),
          diag = list(continuous = plot_density),
          title = title,
          legend = c(1, 1),
          columnLabels = column_labels
        ) + ggplot2::labs(color = color_label, fill = color_label)
        if (is.null(color_scheme)) {
          discrete <- !is.numeric(plt_df$plt_color)
          plt[1, 1] <- plt[1, 1] +
            vthemes::scale_color_vmodern(discrete = discrete, drop = drop) +
            vthemes::scale_fill_vmodern(discrete = discrete, drop = drop)
        } else {
          plt[1, 1] <- plt[1, 1] +
            ggplot2::scale_color_manual(values = color_scheme, drop = drop) +
            ggplot2::scale_fill_manual(values = color_scheme, drop = drop)
        }
      }

    } else {
      # grab color and color_upper legends
      legend_plt_df <- plt_df %>%
        dplyr::mutate(legend_x = data[, columns[1]],
                      legend_y = data[, columns[2]])
      legend_plt1 <- ggplot2::ggplot(legend_plt_df) +
        ggplot2::geom_point(ggplot2::aes(x = legend_x, y = legend_y,
                                         color = color)) +
        ggplot2::labs(color = color_label, fill = color_label)
      legend_plt2 <- ggplot2::ggplot(legend_plt_df) +
        ggplot2::geom_point(ggplot2::aes(x = legend_x, y = legend_y,
                                         color = color_upper)) +
        ggplot2::labs(color = color_upper_label, fill = color_upper_label)
      if (is.null(color_scheme)) {
        legend_plt1 <- legend_plt1 +
          vthemes::scale_color_vmodern(discrete = !is.numeric(color),
                                       drop = drop)
      } else {
        legend_plt1 <- legend_plt1 +
          ggplot2::scale_color_manual(values = color_scheme, drop = drop)
      }
      if (is.null(color_scheme_upper)) {
        legend_plt2 <- legend_plt2 +
          vthemes::scale_color_vmodern(discrete = !is.numeric(color_upper),
                                       viridis = TRUE, option = "D",
                                       drop = drop)
      } else {
        legend_plt2 <- legend_plt2 +
          ggplot2::scale_color_manual(values = color_scheme_upper, drop = drop)
      }
      if (is.null(theme_function)) {
        legend_plt1 <- legend_plt1 + vthemes::theme_vmodern(...)
        legend_plt2 <- legend_plt2 + vthemes::theme_vmodern(...)
      } else {
        legend_plt1 <- theme_function(legend_plt1, ...)
        legend_plt2 <- theme_function(legend_plt2, ...)
      }
      legend1 <- GGally::grab_legend(
        legend_plt1 + ggplot2::theme(legend.position = "bottom")
      )
      legend2 <- GGally::grab_legend(
        legend_plt2 + ggplot2::theme(legend.position = "bottom")
      )

      # make ggpairs
      if (nfactors == 0) {
        plt <- GGally::ggpairs(
          data = plt_df,
          columns = columns,
          mapping = ggplot2::aes(color = color),
          diag = list(continuous = plot_density),
          lower = list(continuous = plot_points),
          upper = list(continuous = upperContinuous),
          title = title,
          columnLabels = column_labels
        )
      } else if (nfactors == 2) {
        plt <- GGally::ggpairs(
          data = plt_df,
          columns = columns,
          mapping = ggplot2::aes(color = color),
          diag = list(continuous = plot_density),
          lower = list(continuous = plot_points, combo = "box_no_facet"),
          upper = list(continuous = upperContinuous,
                       combo = upperCombo,
                       discrete = upperDiscrete),
          title = title,
          columnLabels = column_labels
        )
      } else {
        if (is.factor(color)) {
          plt_df$plt_color <- color
        } else {
          plt_df$plt_color <- color_upper
        }
        plt <- GGally::ggpairs(
          data = plt_df,
          columns = columns,
          mapping = ggplot2::aes(color = plt_color),
          diag = list(continuous = plot_density),
          lower = list(continuous = lowerContinuous),
          upper = list(continuous = upperContinuous),
          title = title,
          columnLabels = column_labels
        )
      }

      # change color scheme in all panels
      for (i in 1:plt$nrow) {
        for (j in 1:plt$ncol) {
          plt_fill <- plt[i, j]$labels$fill
          plt_col <- plt[i, j]$labels$colour
          if (!is.null(plt_fill)) {
            if (plt_fill %in% names(plt_df)) {
              if (all(as.character(plt_df[, plt_fill]) ==
                      as.character(color))) {
                if (is.null(color_scheme)) {
                  plt[i, j] <- plt[i, j] +
                    vthemes::scale_fill_vmodern(discrete = !is.numeric(color),
                                                drop = drop)
                } else {
                  plt[i, j] <- plt[i, j] +
                    ggplot2::scale_fill_manual(values = color_scheme,
                                               drop = drop)
                }
              } else {
                if (is.null(color_scheme_upper)) {
                  plt[i, j] <- plt[i, j] +
                    vthemes::scale_fill_vmodern(
                      discrete = !is.numeric(color_upper),
                      viridis = TRUE, option = "D", drop = drop
                    )
                } else {
                  plt[i, j] <- plt[i, j] +
                    ggplot2::scale_fill_manual(values = color_scheme_upper,
                                               drop = drop)
                }
              }
            }
          }

          if (!is.null(plt_col)) {
            if (plt_col %in% names(plt_df)) {
              ptr <- FALSE
              if (is.numeric(plt_df[, plt_col]) & is.numeric(color)) {
                ptr <- all(plt_df[, plt_col] == color)
              } else if (is.factor(plt_df[, plt_col]) & is.factor(color)) {
                ptr <- all(as.character(plt_df[, plt_col]) ==
                             as.character(color))
              }
              if (ptr) {
                if (is.null(color_scheme)) {
                  plt[i, j] <- plt[i, j] +
                    vthemes::scale_color_vmodern(discrete = !is.numeric(color),
                                                 drop = drop)
                } else {
                  plt[i, j] <- plt[i, j] +
                    ggplot2::scale_color_manual(values = color_scheme,
                                                drop = drop)
                }
              } else {
                if (is.null(color_scheme_upper)) {
                  plt[i, j] <- plt[i, j] +
                    vthemes::scale_color_vmodern(
                      discrete = !is.numeric(color_upper),
                      viridis = TRUE, option = "D", drop = drop
                    )
                } else {
                  plt[i, j] <- plt[i, j] +
                    ggplot2::scale_color_manual(values = color_scheme_upper,
                                                drop = drop)
                }
              }
            }
          }
        }
      }
    }

    if (length(columns) != 1) {
      if (is.null(theme_function)) {
        plt <- cowplot::plot_grid(
          GGally::ggmatrix_gtable(plt + vthemes::theme_vmodern(...)),
          legend1, legend2,
          nrow = 3, rel_heights = c(10, 1, 1)
        )
      } else {
        plt <- cowplot::plot_grid(
          GGally::ggmatrix_gtable(theme_function(plt, ...)),
          legend1, legend2,
          nrow = 3, rel_heights = c(10, 1, 1)
       )
      }
    } else {
      if (is.null(theme_function)) {
        plt <- plt + vthemes::theme_vmodern(...)
      } else {
        plt <- theme_function(plt, ...)
      }
    }
  }

  if (show_plot) {
    print(plt)
  }
  return(plt)
}

#' Plot pretty PCA plots using custom ggplot theme.
#'
#' @description Run PCA on the given data matrix and generates PC plots for the
#'   specified principal components.
#'
#' @inheritParams plot_pairs
#' @param X Data matrix or data.frame on which to perform PCA. Must specify
#'   either \code{X} or \code{pca_obj}.
#' @param pca_obj Output of previous run of plot_pca() to avoid re-computing SVDs
#'   (i.e., the PC loadings and scores) again. Must specify either \code{X} or
#'   \code{pca_obj}. Ignored if \code{X} is provided.
#' @param npcs Number of top PCs to plot. Must specify either \code{npcs} or
#'   \code{pcs}.
#' @param pcs Vector of which PCs to show. Must specify either \code{npcs} or
#'   \code{pcs}. Ignored if \code{npcs} is provided.
#' @param show_var Logical. Whether or not to show the proportion of variance
#'   explained in axes labels.
#' @param center Logical. Whether or not to center data for PCA.
#' @param scale Logical. Whether or not to scale data for PCA.
#'
#' @returns A list of four:
#' \describe{
#' \item{plot}{A ggplot object of the PC pair plots.}
#' \item{scores}{A matrix with the PC scores.}
#' \item{loadings}{A matrix with the PC loadings.}
#' \item{var.explained}{A vector of the proportions of variance explained.}
#' }
#'
#' @examples
#' out <- plot_pca(X = iris[, -5], npcs = 3, color = iris$Species)
#' out$plot
#' iris2 <- data.frame(iris, z = rep(letters[1:2], length.out = nrow(iris)))
#' out <- plot_pca(X = iris2[, -c(5, 6)], npcs = 3,
#'                color = iris2$Species, color_upper = as.factor(iris2$z))
#' out$plot
#'
#' @export
plot_pca <- function(X, pca_obj, npcs, pcs,
                    color = NULL, color_upper = NULL,
                    color_label = "", color_upper_label = "",
                    color_scheme = NULL, color_scheme_upper = NULL,
                    point_size = .5, point_alpha = 1, subsample = 1,
                    show_var = TRUE, center = TRUE, scale = FALSE,
                    title = "", show_plot = FALSE, ...) {
  X1 <- NULL  # to fix no visible binding for global variable error
  X2 <- NULL

  if (!missing(X)) {
    X <- scale(as.matrix(X), center = center, scale = scale)
    if (sum(is.na(X)) > 0) { # error checking
      stop("NAs found in X")
    }
  }

  if (!missing(npcs)) { # which pcs to plot
    pcs <- 1:npcs
  } else {
    npcs <- length(pcs)
  }
  max_pcs <- max(pcs) # maximum pc

  # compute svd of X
  if (!missing(X)) {
    if (max_pcs / min(nrow(X), ncol(X)) > .25) {  # compute full svd
      X_svd <- svd(X)
    } else {  # only compute top singular vectors
      X_svd <- X %>% irlba::irlba(nu = max_pcs, nv = max_pcs)
    }
    d <- X_svd$d
  } else {
    X_svd <- list(
      u = pca_obj$scores,
      v = pca_obj$loadings,
      var_explained = pca_obj$var_explained
    )
    d <- pca_obj$d
  }

  # compute and show proportion of variance
  if (show_var) {
    if (!missing(X)) {
      total_var <- norm(X, "F")^2
      var_explained <- X_svd$d^2 / total_var
    } else {
      var_explained <- X_svd$var_explained
    }
    var_explained_str <- paste0(" (", round(var_explained, 3), ")")
  } else {
    var_explained <- NA
    var_explained_str <- rep("", times = max_pcs)
  }

  # initializing data frame for plotting
  plt_df <- data.frame(X_svd$u)

  # adding labels for colors
  if (!is.null(color)) {
    plt_df <- plt_df %>% dplyr::mutate(color = color)
  }
  if (!is.null(color_upper)) {
    plt_df <- plt_df %>% dplyr::mutate(color_upper = color_upper)
  }

  # subsample points
  if (subsample != 1) {
    plt_df <- dplyr::slice_sample(plt_df, prop = subsample, replace = FALSE)
  }

  if (npcs == 2) {  # plot ggplot object instead of ggpairs
    if (!is.null(color)) {  # plot with color
      plt <- ggplot2::ggplot(plt_df) +
        ggplot2::aes(x = X1, y = X2, color = color) +
        ggplot2::geom_point(alpha = point_alpha, size = point_size) +
        ggplot2::labs(
          x = paste0("PC1", var_explained_str[1]),
          y = paste0("PC2", var_explained_str[2]),
          color = color_label, title = title
        ) +
        vthemes::theme_vmodern(...)
      if (is.null(color_scheme)) {
        plt <- plt +
          vthemes::scale_color_vmodern(discrete = !is.numeric(plt_df$color))
      } else {
        plt <- plt + ggplot2::scale_color_manual(values = color_scheme)
      }
    } else {  # plot without color
      plt <- ggplot2::ggplot(plt_df) +
        ggplot2::aes(x = X1, y = X2) +
        ggplot2::geom_point(alpha = point_alpha, size = point_size) +
        ggplot2::labs(
          x = paste0("PC1", var_explained_str[1]),
          y = paste0("PC2", var_explained_str[2]),
          title = title
        ) +
        vthemes::theme_vmodern(...)
    }
  } else {
    plt <- plot_pairs(data = plt_df, columns = pcs, title = title,
                     point_size = point_size, point_alpha = point_alpha,
                     show_upper = !is.null(color_upper), drop = FALSE,
                     color = color, color_upper = color_upper,
                     color_label = color_label,
                     color_upper_label = color_upper_label,
                     color_scheme = color_scheme,
                     color_scheme_upper = color_scheme_upper,
                     column_labels = paste0("PC", pcs, var_explained_str[pcs]),
                     ...)
  }
  if (show_plot) {
    print(plt)
  }

  return(list(plot = plt, scores = X_svd$u, loadings = X_svd$v, d = d,
              var_explained = var_explained))
}

#' Plot pretty hierarchical clustering dendrograms.
#'
#' @description Run hierarchical clustering on the given data matrix and
#'   plot the resulting dendrogram using a pretty theme. Also allows for
#'   coloring of the leaf nodes.
#'
#' @param data Data matrix or data.frame on which to perform hierarchical
#'   clustering.
#' @param leaf_labels (Optional) Text labels for leaf nodes (e.g.,
#'   class/outcome labels).
#' @param leaf_colors (Optional) Data vector to use for coloring leaf nodes.
#' @param dist_metric Distance metric for clustering (see [stats::dist()]).
#' @param dist_matrix (Optional) Distance matrix for clustering. Must provide
#'   either \code{dist_metric} or \code{dist_matrix}.
#' @param linkage Rype of linkage for hierarchical clustering (see
#'   [stats::hclust()]).
#' @param text_size Numeric; size of text for leaf node labels.
#' @param title Character string. Title of plot.
#' @param show_plot Logical. Should this plot be printed? Default \code{FALSE}.
#'
#' @returns A list of three:
#' \describe{
#' \item{plot}{A ggplot object of the dendrogram.}
#' \item{hclust}{Output of [stats::hclust()]}
#' \item{dend}{Hierarchical clustering dendrogram data. See output of
#'   [ggdendro::dendro_data()].}
#' }
#'
#' @examples
#' out <- plot_hclust(data = iris[, -5], leaf_labels = iris$Species,
#'                   leaf_colors = iris$Species)
#' out$plot
#'
#' @export
plot_hclust <- function(data,
                       leaf_labels = rownames(data), leaf_colors = NULL,
                       dist_metric = "euclidean", dist_matrix = NULL,
                       linkage = "ward.D", text_size = 10, title = NULL,
                       show_plot = F) {
  y <- NULL  # to fix no visible binding for global variable error
  x <- NULL
  color <- NULL

  leaf_labels <- leaf_labels
  data <- as.matrix(data)
  if (sum(is.na(data)) > 0) {
    stop("NAs found in data")
  }

  # distance matrix
  if (is.null(dist_matrix)) {
    Dmat <- stats::dist(data, method = dist_metric)
  } else {
    if (!("dist" %in% class(dist_matrix))) {
      Dmat <- stats::as.dist(dist_matrix)
    } else {
      Dmat <- dist_matrix
    }
  }

  # hierarchical clustering
  hclust_out <- stats::hclust(Dmat, method = linkage)
  hclust_dend <- stats::as.dendrogram(hclust_out)

  if (!is.null(leaf_colors)) {  # annotate tree leaves
    palette_out <- get_custom_color_palette(leaf_colors, TRUE)
    lab_colors <- palette_out$colors
    color_palette <- palette_out$palette
    lab_colors <- lab_colors[stats::order.dendrogram(hclust_dend)]
    lab_df <- data.frame(x = 0, y = 0, color = leaf_colors)
  } else {
    lab_colors <- "black"
  }

  # get leaf labels
  if (is.null(leaf_labels)) {
    hclust_dend <- hclust_dend %>%
      dendextend::set_labels(
        rep("------", ncol(as.matrix(Dmat)))
      )
  } else {
    hclust_dend <- hclust_dend %>%
      dendextend::set_labels(
        leaf_labels[stats::order.dendrogram(hclust_dend)]
      )
  }

  if (is.null(title)) {
    title <- sprintf("Hierarchical Clustering: %s Linkage, %s Distance",
                     linkage, dist_metric)
  }

  # convert to ggplot object
  hclust_dend <- ggdendro::dendro_data(hclust_dend)
  hclust_plt <- suppressWarnings(
    suppressMessages(
      ggdendro::ggdendrogram(hclust_dend) +
        ggplot2::labs(title = title, color = "Label") +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        ggplot2::scale_x_continuous(breaks = seq_along(hclust_dend$labels$label),
                                    labels = hclust_dend$labels$label,
                                    expand = c(0, 0)) +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(color = lab_colors,
                                                           size = text_size))
    )
  )

  # add legend
  if (!is.null(leaf_colors)) {
    if (is.factor(leaf_colors)) {
      hclust_plt <- hclust_plt +
        ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = color),
                            data = lab_df, size = -1) +
        ggplot2::guides(
          color = ggplot2::guide_legend(override.aes = list(size = 3))
        ) +
        ggplot2::scale_color_manual(values = color_palette)
    } else {
      hclust_plt <- hclust_plt +
        ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = color),
                            data = lab_df, size = -1) +
        viridis::scale_colour_viridis(discrete = F)
    }
  }

  if (show_plot) {
    print(hclust_plt)
  }
  return(list(plot = hclust_plt, hclust = hclust_out, dend = hclust_dend))
}
