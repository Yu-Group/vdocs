#' Plot pretty bar plots using custom ggplot theme.
#' 
#' @description Wrapper around [ggplot2::geom_bar()] that plots pretty bar
#'   plots using a custom ggplot theme.
#' 
#' @param data Data frame to use for plot.
#' @param x.str Character string. Name of variable in \code{data} to plot on 
#'   x-axis.
#' @param y.str Character string. Name of variable in \doe{data} to plot on 
#'   y-axis. Used only if \code{stat = "identity"}.
#' @param fill.str Character string (optional). Name of variable in \code{data}
#'   to use as fill aesthetic in plot.
#' @param fill Fill color. Used only if \code{fill.str} is \code{NULL}.
#' @param stat See [ggplot2::geom_bar()].
#' @param position See [ggplot2::geom_bar()].
#' @param show.plot Logical. Should this plot be printed? Default \code{FALSE}.
#' @param ... Other arguments to pass to [ggplot2::geom_bar()].
#'
#' @return A ggplot bar plot.
#' 
#' @examples 
#' df <- data.frame(x = rep(letters[1:3], 2), y = rep(LETTERS[1:2], 3))
#' plotBarplot(data = df, x.str = "x")
#' plotBarplot(df, x.str = "x", fill.str = "y")
plotBarplot <- function(data, x.str, y.str = NULL,
                        fill.str = NULL, fill = "#6FBBE3", 
                        stat = "count", position = "dodge",
                        show.plot = F, ...) {
  
  if (is.null(x.str) | missing(x.str)) {
    stop("Must specify x.str argument.")
  }
  
  if (is.null(y.str)) {
    ylab <- "Frequency"
  } else {
    ylab <- y.str
  }
  
  aesthetics <- get_aesthetics(x.str = x.str, y.str = y.str, fill.str = fill.str)
  plt <- ggplot2::ggplot(data) +
    aesthetics + 
    ggplot2::labs(x = x.str, y = ylab, fill = fill.str) +
    prettyGGplotTheme()
  if (!is.null(fill.str)) {
    if (is.character(data[[fill.str]])) {
      data[[fill.str]] <- as.factor(data[[fill.str]])
    }
    plt <- plt +
      ggplot2::geom_bar(position = position, stat = stat, color = "grey98",
                        ...) +
      prettyGGplotFill(fill = data[[fill.str]])
  } else {
    plt <- plt +
      ggplot2::geom_bar(position = position, stat = stat, color = "grey98",
                        fill = fill, ...)
  }
  if (show.plot) {
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
#' @param x.str Character string. Name of variable in \code{data} to plot on 
#'   x-axis. If \code{NULL}, plot boxplot using all values in data frame.
#' @param y.str Character string (optional). Name of variable in \doe{data} to 
#'   plot on y-axis. Should be a factor variable.
#' @param fill.str Character string (optional). Name of variable in \code{data}
#'   to use as fill aesthetic in plot.
#' @param horizontal Logical. Whether the boxplots should be horizontal instead
#'   of vertical.
#' @param show.plot Logical. Should this plot be printed? Default \code{FALSE}.
#' @param ... Other arguments to pass to [ggplot2::geom_boxplot()].
#'
#' @return A ggplot boxplot.
#' 
#' @examples 
#' ## plot boxplot of all data in data frame
#' plotBoxplot(as.data.frame(matrix(rnorm(1000), nrow = 100)))
#' ## plot boxplot of single column in data frame
#' plotBoxplot(iris, x.str = "Sepal.Width")
#' plotBoxplot(iris, x.str = "Sepal.Width", y.str = "Species")
#' iris2 <- data.frame(iris, 
#'                     z = as.factor(rep(letters[1:2], 
#'                                   length.out = nrow(iris))))
#' plotBoxplot(iris2, x.str = "Sepal.Width", y.str = "Species", fill.str = "z")
plotBoxplot <- function(data, x.str = NULL, y.str = NULL, fill.str = NULL,
                        horizontal = TRUE, show.plot = F, ...) {
  if (is.null(x.str)) {  # plot all data
    x.str <- "data"
    data <- tidyr::gather(data, key = "variable", value = "data",
                          -tidyselect::all_of(c(fill.str, y.str)))
  }
  
  group.str <- y.str
  if (!is.null(y.str) && !is.null(fill.str)) {
    group.str <- sprintf("interaction(%s, %s)", y.str, fill.str)
  }
  aesthetics <- get_aesthetics(x.str = x.str, y.str = y.str, fill.str = fill.str,
                               group.str = group.str)
  plt <- ggplot2::ggplot(data) +
    aesthetics + 
    ggplot2::geom_boxplot(...) +
    ggplot2::labs(x = x.str, y = y.str, fill = fill.str) +
    prettyGGplotTheme()
  if (!is.null(fill.str)) {
    plt <- plt + prettyGGplotFill(fill = data[[fill.str]])
  }
  if (!horizontal) {
    plt <- plt + ggplot2::coord_flip()
  }
  if (show.plot) {
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
#' @param x.str Character string. Name of variable in \code{data} to plot on 
#'   x-axis. If \code{NULL}, plot density using all values in data frame.
#' @param fill.str Character string (optional). Name of variable in \code{data}
#'   to use as fill aesthetic in plot.
#' @param fill Fill color. Used only if \code{fill.str} is \code{NULL}.
#' @param alpha Alpha value for transparency.
#' @param show.plot Logical. Should this plot be printed? Default \code{FALSE}.
#' @param ... Other arguments to pass to [ggplot2::geom_density()].
#'
#' @return A ggplot density.
#' 
#' @examples 
#' ## plot distribution of all data in data frame
#' plotDensity(as.data.frame(rnorm(1000), nrow = 100))
#' ## plot distribution of a single column in data frame
#' plotDensity(iris, x.str = "Sepal.Width")
#' plotDensity(iris, x.str = "Sepal.Width", fill.str = "Species")
#' 
plotDensity <- function(data, x.str = NULL, fill.str = NULL, fill = "#6FBBE3",
                        alpha = 0.4, show.plot = F, ...) {
  if (is.null(x.str)) {  # plot all data
    x.str <- "data"
    data <- tidyr::gather(data, key = "variable", value = "data",
                          -tidyselect::all_of(fill.str))
  }
  
  aesthetics <- get_aesthetics(x.str = x.str, fill.str = fill.str)
  plt <- ggplot2::ggplot(data) +
    aesthetics + 
    ggplot2::labs(x = x.str, y = "Density", fill = fill.str) +
    prettyGGplotTheme()
  if (!is.null(fill.str)) {
    if (is.character(data[[fill.str]])) {
      data[[fill.str]] <- as.factor(data[[fill.str]])
    }
    plt <- plt +
      ggplot2::geom_density(color = "black", alpha = alpha, ...) +
      prettyGGplotFill(fill = data[[fill.str]])
  } else {
    plt <- plt +
      ggplot2::geom_density(color = "black", alpha = alpha, fill = fill, ...)
  }
  if (show.plot) {
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
#' @param x.str Character string. Name of variable in \code{data} to plot on 
#'   x-axis. If \code{NULL}, plot density using all values in data frame.
#' @param fill.str Character string (optional). Name of variable in \code{data}
#'   to use as fill aesthetic in plot.
#' @param fill Fill color. Used only if \code{fill.str} is \code{NULL}.
#' @param bins Number of histogram bins.
#' @param show.plot Logical. Should this plot be printed? Default \code{FALSE}.
#' @param ... Other arguments to pass to [ggplot2::geom_histogram()].
#'
#' @return A ggplot histogram.
#' 
#' @examples 
#' ## plot distribution of all data in data frame
#' plotHistogram(as.data.frame(rnorm(1000), nrow = 100))
#' ## plot distribution of a single column in data frame
#' plotHistogram(iris, x.str = "Sepal.Width")
#' plotHistogram(iris, x.str = "Sepal.Width", fill.str = "Species")
plotHistogram <- function(data, x.str = NULL, fill.str = NULL, fill = "#6FBBE3",
                          bins = 12, show.plot = F, ...) {
  if (is.null(x.str)) {  # plot all data
    x.str <- "data"
    data <- tidyr::gather(data, key = "variable", value = "data",
                          -tidyselect::all_of(fill.str))
  }
  
  aesthetics <- get_aesthetics(x.str = x.str, fill.str = fill.str)
  plt <- ggplot2::ggplot(data) +
    aesthetics + 
    ggplot2::labs(x = x.str, y = "Frequency", fill = fill.str) +
    prettyGGplotTheme()
  if (!is.null(fill.str)) {
    if (is.character(data[[fill.str]])) {
      data[[fill.str]] <- as.factor(data[[fill.str]])
    }
    plt <- plt +
      ggplot2::geom_histogram(color = "grey98", bins = bins, ...) +
      prettyGGplotFill(fill = data[[fill.str]])
  } else {
    plt <- plt +
      ggplot2::geom_histogram(color = "grey98", bins = bins, fill = fill, ...)
  }
  if (show.plot) {
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
#' @param x.str Character string. Name of variable in \code{data} to plot on 
#'   x-axis.
#' @param y.str Character string. Name of variable in \code{data} to plot on 
#'   y-axis.
#' @param color.str Character string (optional). Name of variable in \code{data}
#'   to use as color aesthetic in plot.
#' @param show.plot Logical. Should this plot be printed? Default \code{FALSE}.
#' @param ... Other arguments to pass to [ggplot2::geom_line()].
#'
#' @return A ggplot line plot.
#' 
#' @examples 
#' df <- data.frame(time = 1:5, value = 5:9)
#' plotLine(df, x.str = "time", y.str = "value")
#' df2 <- data.frame(time = rep(1:5, 2), value = 1:10,
#'                   group = rep(letters[1:2], each = 5))
#' plotLine(df2, x.str = "time", y.str = "value", color.str = "group")
plotLine <- function(data, x.str, y.str, color.str = NULL, show.plot = F, ...) {
  if (is.null(x.str) | is.null(y.str)) {
    stop("Must specify x.str and y.str argument.")
  }
  
  aesthetics <- get_aesthetics(x.str = x.str, y.str = y.str, 
                               color.str = color.str, group.str = color.str)
  plt <- ggplot2::ggplot(data) +
    aesthetics + 
    ggplot2::geom_line(...) +
    ggplot2::labs(x = x.str, y = y.str, color = color.str) +
    prettyGGplotTheme()
  if (!is.null(color.str)) {
    if (is.character(data[[color.str]])) {
      data[[color.str]] <- as.factor(data[[color.str]])
    }
    plt <- plt + prettyGGplotColor(color = data[[color.str]])
  }
  if (show.plot) {
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
#' @param x.str Character string. Name of variable in \code{data} to plot on 
#'   x-axis.
#' @param y.str Character string. Name of variable in \code{data} to plot on 
#'   y-axis.
#' @param color.str Character string (optional). Name of variable in \code{data}
#'   to use as color aesthetic in plot.
#' @param show.plot Logical. Should this plot be printed? Default \code{FALSE}.
#' @param ... Other arguments to pass to [ggplot2::geom_point()].
#'
#' @return A ggplot scatter plot.
#' 
#' @examples 
#' plotScatter(iris, x.str = "Sepal.Width", y.str = "Sepal.Length")
#' plotScatter(iris, x.str = "Sepal.Width", y.str = "Sepal.Length",
#'             color.str = "Species")
#'             
plotScatter <- function(data, x.str, y.str, color.str = NULL,
                        show.plot = F, ...) {
  if (is.null(x.str) | is.null(y.str)) {
    stop("Must specify x.str and y.str argument.")
  }
  
  aesthetics <- get_aesthetics(x.str = x.str, y.str = y.str, 
                               color.str = color.str)
  plt <- ggplot2::ggplot(data) +
    aesthetics + 
    ggplot2::geom_point(...) +
    ggplot2::labs(x = x.str, y = y.str, color = color.str) +
    prettyGGplotTheme()
  if (!is.null(color.str)) {
    if (is.character(data[[color.str]])) {
      data[[color.str]] <- as.factor(data[[color.str]])
    }
    plt <- plt + prettyGGplotColor(color = data[[color.str]])
  }
  if (show.plot) {
    print(plt)
  }
  return(plt)
}

