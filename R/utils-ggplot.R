#' Customized pretty ggplot theme
#'
#' @param font Font family for ggplot text.
#' @param background_color Color for plot background.
#' @param strip_background_color Color for strip background (for
#'   \code{ggplot2::facet_grid()} and \code{ggplot2::facet_wrap()}).
#' @param grid_color Color of panel grid major axes or \code{NULL} if want no
#'   major grid lines.
#' @param axis_line_width Width of x and y axes lines.
#' @param show_ticks Logical; whether or not to show axes tick marks.
#' @param x_text_angle Logical; whether or not to angle x text at 45 degrees.
#' @param size_theme One of "small", "normal", "large", "xlarge"; default sizes
#'   for plot text and titles. If \code{NULL}, defaults to values specified by
#'   \code{axis_title_size}, \code{axis_text_size}, \code{legend_title_size},
#'   \code{legend_text_size}, \code{strip_text_size}, \code{title_size}.
#' @param axis_title_size Font size of axis title. Ignored if \code{size_theme}
#'   not \code{NULL}.
#' @param axis_text_size Font size of axis text. Ignored if \code{size_theme}
#'   not \code{NULL}.
#' @param legend_title_size Font size of legend title. Ignored if
#'   \code{size_theme} not \code{NULL}.
#' @param legend_text_size Font size of legend text/key. Ignored if
#'   \code{size_theme} not \code{NULL}.
#' @param strip_text_size Font size of strip text. Ignored if \code{size_theme}
#'   not \code{NULL}.
#' @param title_size Font size of plot title. Ignored if \code{size_theme}
#'   not \code{NULL}.
#' @param ... = other arguments to pass to \code{ggplot2::theme()}
#'
#' @return A \code{ggplot2::theme()} object.
#'
#' @examples
#' require(ggplot2)
#' ggplot(iris) +
#'   aes(x = Sepal.Length, y = Sepal.Width) +
#'   geom_point() +
#'   pretty_ggplot_theme()
#'
#' @export
pretty_ggplot_theme <- function(font = "Helvetica",
                                background_color = "grey98",
                                strip_background_color = "#2c3e50",
                                grid_color = "grey90",
                                axis_line_width = 1,
                                show_ticks = TRUE,
                                x_text_angle = FALSE,
                                size_theme = NULL,
                                axis_title_size = 10, axis_text_size = 7,
                                legend_title_size = 10, legend_text_size = 8,
                                strip_text_size = 9, title_size = 12,
                                ...) {
  if (!is.null(size_theme)) {
    size_theme <- match.arg(size_theme,
                            choices = c("small", "medium", "large",
                                        "xlarge", "xxlarge", "xxxlarge"))
    if (size_theme == "small") {
      axis_title_size <- 10
      axis_text_size <- 7
      legend_title_size <- 10
      legend_text_size <- 8
      strip_text_size <- 9
      title_size <- 12
    } else if (size_theme == "medium") {
      axis_title_size <- 14
      axis_text_size <- 10
      legend_title_size <- 14
      legend_text_size <- 10
      strip_text_size <- 12
      title_size <- 16
    } else if (stringr::str_detect(size_theme, "large")) {
      num_x <- stringr::str_count(size_theme, "x")
      axis_title_size <- 18 + num_x * 2
      axis_text_size <- 14 + num_x * 2
      legend_title_size <- 18 + num_x * 2
      legend_text_size <- 14 + num_x * 2
      strip_text_size <- 16 + num_x * 2
      title_size <- 20 + num_x * 2
    }
  }

  custom_theme <- ggplot2::theme(
    axis.title = ggplot2::element_text(family = font,
                                       size = axis_title_size,
                                       face = "bold"),
    axis.text = ggplot2::element_text(family = font, size = axis_text_size),
    axis.line = ggplot2::element_line(size = axis_line_width, color = "black"),
    axis.ticks = ggplot2::element_line(size = ifelse(show_ticks,
                                                     ggplot2::rel(1), 0),
                                       colour = "black"),
    axis.text.x = ggplot2::element_text(angle = ifelse(x_text_angle, 45, 0),
                                        hjust = ifelse(x_text_angle, 1, 0.5)),
    panel.grid.major = ggplot2::element_line(colour = grid_color,
                                             size = ggplot2::rel(0.5)),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = background_color),
    strip.background = ggplot2::element_rect(fill = strip_background_color,
                                             color = strip_background_color),
    strip.text = ggplot2::element_text(color = "white", face = "bold",
                                       size = strip_text_size),
    legend.key = ggplot2::element_rect(fill = "grey98"),
    legend.text = ggplot2::element_text(family = font, size = legend_text_size),
    legend.title = ggplot2::element_text(family = font, face = "bold",
                                         size = legend_title_size),
    plot.title = ggplot2::element_text(family = font, face = "bold",
                                       size = title_size),
    ...
  )

  return(custom_theme)
}

#' Customized pretty ggplot color and fill themes
#'
#' @param color Vector used for color aesthetic. Should match
#'   \code{ggplot2::aes(color = ...)} argument.
#' @param fill Vector used for fill aesthetic. Should match
#'   \code{ggplot2::aes(fill = ...)} argument.
#' @param viridis Logical. Whether or not to use \code{viridis} scheme if using
#'   discrete color scheme.
#' @param option Argument indicating \code{viridis} palette name.
#' @param drop Logical; whether or not to drop factors with no observations.
#' @param ... Other arguments to pass to [ggplot2::scale_color_manual()],
#'   [ggplot2::scale_fill_manual()], [viridis::scale_colour_viridis()], or
#'   [viridis::scale_fill_viridis()].
#'
#' @return A ggplot color or fill theme object.
#'
#' @examples
#' require(ggplot2)
#' ggplot(iris) +
#'   aes(x = Sepal.Length, y = Sepal.Width, color = Species) +
#'   geom_point() +
#'   pretty_ggplot_theme() +
#'   pretty_ggplot_color(color = iris$Species)
#' ggplot(iris) +
#'   aes(x = Sepal.Length, fill = Species) +
#'   geom_density() +
#'   pretty_ggplot_theme() +
#'   pretty_ggplot_fill(fill = iris$Species)
#'
#' @name pretty_ggplot_color_fill_themes
#' @rdname pretty_ggplot_color_fill_themes
#'
NULL

#' @rdname pretty_ggplot_color_fill_themes
#'
#' @export
pretty_ggplot_color <- function(color, viridis = F, option = "plasma", drop = T,
                                ...) {
  discrete <- is.factor(color)
  if (discrete) {
    if (nlevels(color) <= 8 & viridis == FALSE) {
      # color palette is a slight modification of
      # RColorBrewer::brewer.pal(n = 8, name = "Dark2")
      custom_palette <- c("#FF9300", "#1B9E77", "#7570B3", "#E7298A",
                          "#66A61E", "#E6AB02", "#A6761D", "#666666")
      custom_color <- ggplot2::scale_color_manual(values = custom_palette,
                                                  drop = drop, ...)
    } else {
      custom_color <- viridis::scale_colour_viridis(
        discrete = discrete, option = option,
        begin = 0, end = 0.95, drop = drop, ...
      )
    }
  } else {
    custom_color <- viridis::scale_colour_viridis(
      discrete = discrete, option = option,
      begin = 0, end = 0.95, ...
    )
  }
  return(custom_color)
}


#' @rdname pretty_ggplot_color_fill_themes
#'
#' @export
pretty_ggplot_fill <- function(fill, viridis = F, option = "plasma", drop = T,
                               ...) {
  discrete <- is.factor(fill)
  if (discrete) {
    if (nlevels(fill) <= 8 & viridis == FALSE) {
      # color palette is a slight modification of
      # RColorBrewer::brewer.pal(n = 8, name = "Dark2")
      custom_palette <- c("#FF9300", "#1B9E77", "#7570B3", "#E7298A",
                          "#66A61E", "#E6AB02", "#A6761D", "#666666")
      custom_fill <- ggplot2::scale_fill_manual(values = custom_palette,
                                                drop = drop, ...)
    } else {
      custom_fill <- viridis::scale_fill_viridis(
        discrete = discrete, option = option,
        begin = 0, end = 0.95, drop = drop, ...
      )
    }
  } else {
    custom_fill <- viridis::scale_fill_viridis(
      discrete = discrete, option = option,
      begin = 0, end = 0.95, ...
    )
  }
  return(custom_fill)
}

#' Blank axis themes for ggplot objects.
#'
#' @return A ggplot theme object.
#'
#' @examples
#' require(ggplot2)
#'
#' ## blank x-axis theme
#' ggplot(iris) +
#'   aes(x = Sepal.Length, fill = Species) +
#'   geom_density() +
#'   blank_x_theme()
#'
#' ## blank y-axis theme
#' ggplot(iris) +
#'   aes(x = Sepal.Length, fill = Species) +
#'   geom_density() +
#'   blank_y_theme()
#'
#' ## blank x- and y-axes theme
#' ggplot(iris) +
#'   aes(x = Sepal.Length, fill = Species) +
#'   geom_density() +
#'   blank_xy_theme()
#'
#' @name blank_ggplot_themes
#' @rdname blank_ggplot_themes
#'
NULL

#' @rdname blank_ggplot_themes
#'
#' @export
blank_x_theme <- function() {
  ggplot2::theme(axis.line.x = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank(),
                 panel.grid.major.x = ggplot2::element_blank())
}

#' @rdname blank_ggplot_themes
#'
#' @export
blank_y_theme <- function() {
  ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_blank())
}

#' @rdname blank_ggplot_themes
#'
#' @export
blank_xy_theme <- function() {
  ggplot2::theme(axis.line = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_blank())
}

#' Get list of aesthetics to add to ggplot.
#'
#' @description Helper function to ignore \code{NULL} inputs when adding
#'   aesthetics to a ggplot.
#'
#' @param x_str Character string specifying the name of the data column to plot
#'   on the x-axis.
#' @param y_str Character string specifying the name of the data column to plot
#'   on the y-axis.
#' @param color_str Character string specifying the name of the data column to
#'   use for the color aesthetic.
#' @param fill_str Character string specifying the name of the data column to
#'   use for the fill aesthetic.
#' @param group_str Character string specifying the name of the data column to
#'   use for the group aesthetic.
#' @param linetype_str Character string specifying the name of the data column
#'   to use for the linetype aesthetic.
#'
#' @return A [ggplot2::aes()] object.
#'
#' @export
get_aesthetics <- function(x_str = NULL, y_str = NULL,
                           color_str = NULL, fill_str = NULL,
                           group_str = NULL, linetype_str = NULL) {
  aes_list <- list()
  if (!is.null(x_str)) {
    aes_list$x <- substitute(.data[[x_str]], list(x_str = x_str))
  }
  if (!is.null(y_str)) {
    aes_list$y <- substitute(.data[[y_str]], list(y_str = y_str))
  }
  if (!is.null(color_str)) {
    aes_list$color <- substitute(.data[[color_str]],
                                 list(color_str = color_str))
  }
  if (!is.null(fill_str)) {
    aes_list$fill <- substitute(.data[[fill_str]], list(fill_str = fill_str))
  }
  if (!is.null(linetype_str)) {
    aes_list$linetype <- substitute(as.factor(.data[[linetype_str]]),
                                    list(linetype_str = linetype_str))
  }
  if (!is.null(group_str)) {
    aes_list$group <- substitute(.data[[group_str]],
                                 list(group_str = group_str))
  }
  return(do.call(ggplot2::aes, aes_list))
}

#' Add color to axis text labels in ggplot
#'
#' @description Helper function to add color to axis text labels in a ggplot
#'   object.
#'
#' @param plot A ggplot object
#' @param labels Axis text labels
#' @param label_colors Data vector to use for coloring axis text labels
#' @param axis One of "x" or "y" indicating which axis.
#'
#' @return A ggplot object with the axis text labels colored.
#'
#' @keywords internal
add_axis_text_colors <- function(plot, labels, label_colors = NULL,
                                 axis = c("x", "y")) {
  . <- NULL  # to fix no visible binding for global variable error
  lab_x <- NULL
  lab_y <- NULL
  lab_color <- NULL

  axis <- match.arg(axis)
  if (!is.null(label_colors)) {
    if (length(ggplot2::ggplot_build(plot)$layout$panel_params) > 1) {
      stop("Colors cannot be added to axis text labels when facets are used. ",
           sprintf("Set %s.label_colors = NULL.", axis))
    }
    if (identical(axis, "y")) {
      axis_labs <- lapply(rev(ggplot2::ggplot_build(plot)$layout$panel_params),
                          function(x) x$y$get_labels()) %>%
        purrr::reduce(c)  # y axis plot labels from bottom to top
    } else if (identical(axis, "x")) {
      axis_labs <- lapply(ggplot2::ggplot_build(plot)$layout$panel_params,
                          function(x) x$x$get_labels()) %>%
        purrr::reduce(c)  # x axis plot labels from left to right
    }
    labs_df <- data.frame(lab_x = 1, lab_y = 1,
                          lab_color = label_colors, lab = labels) %>%
      dplyr::left_join(x = data.frame(lab = axis_labs), y = ., by = "lab")
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(x = lab_x, y = lab_y, color = lab_color),
                          data = labs_df, size = -1)
    # get color and add color legend
    lab_colors <- get_custom_color_palette(labs_df$lab_color)
    if (is.factor(label_colors)) {
      plot <- plot +
        ggplot2::guides(
          color = ggplot2::guide_legend(override.aes = list(size = 3))
        ) +
        ggplot2::scale_color_manual(values = lab_colors)
    } else {
      plot <- plot + viridis::scale_colour_viridis(discrete = F)
    }
    if (identical(axis, "x")) {
      plot <- plot +
        ggplot2::theme(axis.text.x = ggplot2::element_text(color = lab_colors))
    } else if (identical(axis, "y")) {
      plot <- plot +
        ggplot2::theme(axis.text.y = ggplot2::element_text(color = lab_colors))
    }
  }
  return(plot)
}

#' Converts data vector to color vector
#'
#' @param color_labels Vector of labels to convert to colors.
#' @param return_palette Logical indicating whether or not to return the
#'   color palette used to convert data to colors.
#'
#' @returns If \code{return_palette = FALSE}, returns a vector of colors. If
#'   \code{return_palette = TRUE}, returns a list of two:
#'   \describe{
#'   \item{colors}{Vector of colors.}
#'   \item{palette}{Color palette used to convert data to colors.}
#'   }
#'
#' @keywords internal
get_custom_color_palette <- function(color_labels, return_palette = FALSE) {
  if (is.factor(color_labels)) {
    if (nlevels(color_labels) <= 8) {
      # color palette is a slight modification of
      # RColorBrewer::brewer.pal(n = 8, name = "Dark2")
      custom_colors <- c("#FF9300", "#1B9E77", "#7570B3", "#E7298A",
                         "#66A61E", "#E6AB02", "#A6761D", "#666666")
      colors_out <- custom_colors[color_labels]
    } else {
      custom_colors <- scales::col_factor(
        palette = "viridis", domain = levels(color_labels)
      )
      colors_out <- custom_colors(color_labels)
    }
  } else {
    custom_colors <- scales::col_numeric(
      palette = "viridis", domain = c(min(color_labels), max(color_labels))
    )
    colors_out <- custom_colors(color_labels)
  }
  if (return_palette) {
    return(list(colors = colors_out, palette = custom_colors))
  } else {
    return(colors_out)
  }
}
