#' Plot pretty bar plots using custom ggplot theme.
#'
#' @description Wrapper around [ggplot2::geom_bar()] that plots pretty bar
#'   plots using a custom ggplot theme.
#'
#' @param data Data frame to use for plot.
#' @param x_str Character string. Name of variable in \code{data} to plot on
#'   x-axis.
#' @param y_str Character string. Name of variable in \code{data} to plot on
#'   y-axis. Used only if \code{stat = "identity"}.
#' @param fill_str Character string (optional). Name of variable in \code{data}
#'   to use as fill aesthetic in plot.
#' @param fill Fill color. Used only if \code{fill_str} is \code{NULL}.
#' @param stat See [ggplot2::geom_bar()].
#' @param position See [ggplot2::geom_bar()].
#' @param theme_options (Optional) list of arguments to pass to
#'   simChef::pretty_ggplot_theme().
#' @param show_plot Logical. Should this plot be printed? Default \code{FALSE}.
#' @param ... Other arguments to pass to [ggplot2::geom_bar()].
#'
#' @return A ggplot bar plot.
#'
#' @family pretty_ggplot_wrappers
#'
#' @examples
#' df <- data.frame(x = rep(letters[1:3], 2), y = rep(LETTERS[1:2], 3))
#' plotBarplot(data = df, x_str = "x")
#' plotBarplot(df, x_str = "x", fill_str = "y")
#'
#' @export
plotBarplot <- function(data, x_str, y_str = NULL,
                        fill_str = NULL, fill = "#6FBBE3",
                        stat = "count", position = "dodge",
                        theme_options = NULL, show_plot = FALSE, ...) {

  if (is.null(x_str) | missing(x_str)) {
    stop("Must specify x_str argument.")
  }

  if (is.null(y_str)) {
    ylab <- "Frequency"
  } else {
    ylab <- y_str
  }

  plt <- ggplot2::ggplot(data) +
    get_aesthetics(x_str = x_str, y_str = y_str,
                             fill_str = fill_str) +
    ggplot2::labs(x = x_str, y = ylab, fill = fill_str)
  if (!is.null(fill_str)) {
    if (is.character(data[[fill_str]])) {
      data[[fill_str]] <- as.factor(data[[fill_str]])
    }
    plt <- plt +
      ggplot2::geom_bar(position = position, stat = stat, color = "grey98",
                        ...) +
      simChef::pretty_ggplot_fill(fill = data[[fill_str]])
  } else {
    plt <- plt +
      ggplot2::geom_bar(position = position, stat = stat, color = "grey98",
                        fill = fill, ...)
  }
  if (is.null(theme_options)) {
    plt <- plt + simChef::pretty_ggplot_theme()
  } else {
    plt <- plt +
      do.call(simChef::pretty_ggplot_theme, args = theme_options)
  }
  if (show_plot) {
    print(plt)
  }
  return(plt)
}

#' Plot pretty boxplots using custom ggplot theme.
#'
#' @description Wrapper around [ggplot2::geom_boxplot()] that plots pretty
#'   boxplots using a custom ggplot theme.
#'
#' @param data Data frame to use for plot.
#' @param x_str Character string. Name of variable in \code{data} to plot on
#'   x-axis. If \code{NULL}, plot boxplot using all values in data frame.
#' @param y_str Character string (optional). Name of variable in \code{data} to
#'   plot on y-axis. Should be a factor variable.
#' @param fill_str Character string (optional). Name of variable in \code{data}
#'   to use as fill aesthetic in plot.
#' @param horizontal Logical. Whether the boxplots should be horizontal instead
#'   of vertical.
#' @param theme_options (Optional) list of arguments to pass to
#'   simChef::pretty_ggplot_theme().
#' @param show_plot Logical. Should this plot be printed? Default \code{FALSE}.
#' @param ... Other arguments to pass to [ggplot2::geom_boxplot()].
#'
#' @return A ggplot boxplot.
#'
#' @family pretty_ggplot_wrappers
#'
#' @examples
#' ## plot boxplot of all data in data frame
#' plotBoxplot(as.data.frame(matrix(rnorm(1000), nrow = 100)))
#' ## plot boxplot of single column in data frame
#' plotBoxplot(iris, y_str = "Sepal.Width")
#' plotBoxplot(iris, x_str = "Species", y_str = "Sepal.Width")
#' iris2 <- data.frame(iris,
#'                     z = as.factor(rep(letters[1:2],
#'                                   length.out = nrow(iris))))
#' plotBoxplot(iris2, x_str = "Species", y_str = "Sepal.Width", fill_str = "z")
#' plotBoxplot(iris2, y_str = "Sepal.Width", fill_str = "z")
#'
#' @export
plotBoxplot <- function(data, x_str = NULL, y_str = NULL, fill_str = NULL,
                        horizontal = FALSE, theme_options = NULL,
                        show_plot = FALSE, ...) {
  if (is.null(y_str)) {  # plot all data
    y_str <- "data"
    data <- tidyr::gather(data, key = "variable", value = "data",
                          -tidyselect::all_of(c(fill_str, x_str)))
  }
  group_str <- x_str

  plt <- ggplot2::ggplot(data) +
    get_aesthetics(x_str = x_str, y_str = y_str,
                             fill_str = fill_str, group_str = group_str) +
    ggplot2::geom_boxplot(...) +
    ggplot2::labs(x = x_str, y = y_str, fill = fill_str)
  if (!is.null(x_str) && !is.null(fill_str)) {
    group_str <- substitute(interaction(x_str, fill_str),
                            list(x_str = as.symbol(x_str),
                                 fill_str = as.symbol(fill_str)))
    plt <- plt + ggplot2::aes(group = !!group_str)
  }
  if (!is.null(fill_str)) {
    plt <- plt + simChef::pretty_ggplot_fill(fill = data[[fill_str]])
  }
  if (horizontal) {
    plt <- plt + ggplot2::coord_flip()
  }
  if (is.null(theme_options)) {
    plt <- plt + simChef::pretty_ggplot_theme()
  } else {
    plt <- plt +
      do.call(simChef::pretty_ggplot_theme, args = theme_options)
  }
  if (show_plot) {
    print(plt)
  }
  return(plt)
}

#' Plot pretty kernel density plots using custom ggplot theme.
#'
#' @description Wrapper around [ggplot2::geom_density()] that plots pretty
#'   density plots using a custom ggplot theme.
#'
#' @param data Data frame to use for plot.
#' @param x_str Character string. Name of variable in \code{data} to plot on
#'   x-axis. If \code{NULL}, plot density using all values in data frame.
#' @param fill_str Character string (optional). Name of variable in \code{data}
#'   to use as fill aesthetic in plot.
#' @param fill Fill color. Used only if \code{fill_str} is \code{NULL}.
#' @param alpha Alpha value for transparency.
#' @param theme_options (Optional) list of arguments to pass to
#'   simChef::pretty_ggplot_theme().
#' @param show_plot Logical. Should this plot be printed? Default \code{FALSE}.
#' @param ... Other arguments to pass to [ggplot2::geom_density()].
#'
#' @return A ggplot density.
#'
#' @family pretty_ggplot_wrappers
#'
#' @examples
#' ## plot distribution of all data in data frame
#' plotDensity(as.data.frame(rnorm(1000), nrow = 100))
#' ## plot distribution of a single column in data frame
#' plotDensity(iris, x_str = "Sepal.Width")
#' plotDensity(iris, x_str = "Sepal.Width", fill_str = "Species")
#'
#' @export
plotDensity <- function(data, x_str = NULL, fill_str = NULL, fill = "#6FBBE3",
                        alpha = 0.4, theme_options = NULL, show_plot = FALSE,
                        ...) {
  if (is.null(x_str)) {  # plot all data
    x_str <- "data"
    data <- tidyr::gather(data, key = "variable", value = "data",
                          -tidyselect::all_of(fill_str))
  }

  plt <- ggplot2::ggplot(data) +
    get_aesthetics(x_str = x_str, fill_str = fill_str) +
    ggplot2::labs(x = x_str, y = "Density", fill = fill_str)
  if (!is.null(fill_str)) {
    if (is.character(data[[fill_str]])) {
      data[[fill_str]] <- as.factor(data[[fill_str]])
    }
    plt <- plt +
      ggplot2::geom_density(color = "black", alpha = alpha, ...) +
      simChef::pretty_ggplot_fill(fill = data[[fill_str]])
  } else {
    plt <- plt +
      ggplot2::geom_density(color = "black", alpha = alpha, fill = fill, ...)
  }
  if (is.null(theme_options)) {
    plt <- plt + simChef::pretty_ggplot_theme()
  } else {
    plt <- plt +
      do.call(simChef::pretty_ggplot_theme, args = theme_options)
  }
  if (show_plot) {
    print(plt)
  }
  return(plt)
}

#' Plot pretty histogram plots using custom ggplot theme.
#'
#' @description Wrapper around [ggplot2::geom_histogram()] that plots pretty
#'   histogram plots using a custom ggplot theme.
#'
#' @param data Data frame to use for plot.
#' @param x_str Character string. Name of variable in \code{data} to plot on
#'   x-axis. If \code{NULL}, plot density using all values in data frame.
#' @param fill_str Character string (optional). Name of variable in \code{data}
#'   to use as fill aesthetic in plot.
#' @param fill Fill color. Used only if \code{fill_str} is \code{NULL}.
#' @param bins Number of histogram bins.
#' @param theme_options (Optional) list of arguments to pass to
#'   simChef::pretty_ggplot_theme().
#' @param show_plot Logical. Should this plot be printed? Default \code{FALSE}.
#' @param ... Other arguments to pass to [ggplot2::geom_histogram()].
#'
#' @return A ggplot histogram.
#'
#' @family pretty_ggplot_wrappers
#'
#' @examples
#' ## plot distribution of all data in data frame
#' plotHistogram(as.data.frame(rnorm(1000), nrow = 100))
#' ## plot distribution of a single column in data frame
#' plotHistogram(iris, x_str = "Sepal.Width")
#' plotHistogram(iris, x_str = "Sepal.Width", fill_str = "Species")
#'
#' @export
plotHistogram <- function(data, x_str = NULL, fill_str = NULL, fill = "#6FBBE3",
                          bins = 12, theme_options = NULL, show_plot = FALSE,
                          ...) {
  if (is.null(x_str)) {  # plot all data
    x_str <- "data"
    data <- tidyr::gather(data, key = "variable", value = "data",
                          -tidyselect::all_of(fill_str))
  }

  plt <- ggplot2::ggplot(data) +
    get_aesthetics(x_str = x_str, fill_str = fill_str) +
    ggplot2::labs(x = x_str, y = "Frequency", fill = fill_str)
  if (!is.null(fill_str)) {
    if (is.character(data[[fill_str]])) {
      data[[fill_str]] <- as.factor(data[[fill_str]])
    }
    plt <- plt +
      ggplot2::geom_histogram(color = "grey98", bins = bins, ...) +
      simChef::pretty_ggplot_fill(fill = data[[fill_str]])
  } else {
    plt <- plt +
      ggplot2::geom_histogram(color = "grey98", bins = bins, fill = fill, ...)
  }
  if (is.null(theme_options)) {
    plt <- plt + simChef::pretty_ggplot_theme()
  } else {
    plt <- plt +
      do.call(simChef::pretty_ggplot_theme, args = theme_options)
  }
  if (show_plot) {
    print(plt)
  }
  return(plt)
}

#' Plot pretty line plots using custom ggplot theme.
#'
#' @description Wrapper around [ggplot2::geom_line()] that plots pretty
#'   line plots using a custom ggplot theme.
#'
#' @param data Data frame to use for plot.
#' @param x_str Character string. Name of variable in \code{data} to plot on
#'   x-axis.
#' @param y_str Character string. Name of variable in \code{data} to plot on
#'   y-axis.
#' @param color_str Character string (optional). Name of variable in \code{data}
#'   to use as color aesthetic in plot.
#' @param theme_options (Optional) list of arguments to pass to
#'   simChef::pretty_ggplot_theme().
#' @param show_plot Logical. Should this plot be printed? Default \code{FALSE}.
#' @param ... Other arguments to pass to [ggplot2::geom_line()].
#'
#' @return A ggplot line plot.
#'
#' @family pretty_ggplot_wrappers
#'
#' @examples
#' df <- data.frame(time = 1:5, value = 5:9)
#' plotLine(df, x_str = "time", y_str = "value")
#' df2 <- data.frame(time = rep(1:5, 2), value = 1:10,
#'                   group = rep(letters[1:2], each = 5))
#' plotLine(df2, x_str = "time", y_str = "value", color_str = "group")
#'
#' @export
plotLine <- function(data, x_str, y_str, color_str = NULL,
                     theme_options = NULL, show_plot = FALSE, ...) {
  if (is.null(x_str) | is.null(y_str)) {
    stop("Must specify x_str and y_str argument.")
  }

  plt <- ggplot2::ggplot(data) +
    get_aesthetics(x_str = x_str, y_str = y_str,
                             color_str = color_str, group_str = color_str) +
    ggplot2::geom_line(...) +
    ggplot2::labs(x = x_str, y = y_str, color = color_str)
  if (!is.null(color_str)) {
    if (is.character(data[[color_str]])) {
      data[[color_str]] <- as.factor(data[[color_str]])
    }
    plt <- plt + simChef::pretty_ggplot_color(color = data[[color_str]])
  }
  if (is.null(theme_options)) {
    plt <- plt + simChef::pretty_ggplot_theme()
  } else {
    plt <- plt +
      do.call(simChef::pretty_ggplot_theme, args = theme_options)
  }
  if (show_plot) {
    print(plt)
  }
  return(plt)
}

#' Plot pretty scatter plots using custom ggplot theme.
#'
#' @description Wrapper around [ggplot2::geom_point()] that plots pretty
#'   scatter plots using a custom ggplot theme.
#'
#' @param data Data frame to use for plot.
#' @param x_str Character string. Name of variable in \code{data} to plot on
#'   x-axis.
#' @param y_str Character string. Name of variable in \code{data} to plot on
#'   y-axis.
#' @param color_str Character string (optional). Name of variable in \code{data}
#'   to use as color aesthetic in plot.
#' @param theme_options (Optional) list of arguments to pass to
#'   simChef::pretty_ggplot_theme().
#' @param show_plot Logical. Should this plot be printed? Default \code{FALSE}.
#' @param ... Other arguments to pass to [ggplot2::geom_point()].
#'
#' @return A ggplot scatter plot.
#'
#' @family pretty_ggplot_wrappers
#'
#' @examples
#' plotScatter(iris, x_str = "Sepal.Width", y_str = "Sepal.Length")
#' plotScatter(iris, x_str = "Sepal.Width", y_str = "Sepal.Length",
#'             color_str = "Species")
#'
#' @export
plotScatter <- function(data, x_str, y_str, color_str = NULL,
                        theme_options = NULL, show_plot = FALSE, ...) {
  if (is.null(x_str) | is.null(y_str)) {
    stop("Must specify x_str and y_str argument.")
  }

  plt <- ggplot2::ggplot(data) +
    get_aesthetics(x_str = x_str, y_str = y_str,
                             color_str = color_str) +
    ggplot2::geom_point(...) +
    ggplot2::labs(x = x_str, y = y_str, color = color_str)
  if (!is.null(color_str)) {
    if (is.character(data[[color_str]])) {
      data[[color_str]] <- as.factor(data[[color_str]])
    }
    plt <- plt + simChef::pretty_ggplot_color(color = data[[color_str]])
  }
  if (is.null(theme_options)) {
    plt <- plt + simChef::pretty_ggplot_theme()
  } else {
    plt <- plt +
      do.call(simChef::pretty_ggplot_theme, args = theme_options)
  }
  if (show_plot) {
    print(plt)
  }
  return(plt)
}

