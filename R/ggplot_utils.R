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
      custom_colors <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")
      custom_colors[2] <- custom_colors[1]
      custom_colors[1] <- "#FF9300"
      colors_out <- custom_colors[color_labels]
    } else {
      custom_colors <- leaflet::colorFactor(
        palette = "viridis", domain = levels(color_labels)
      )
      colors_out <- custom_colors(color_labels)
    }
  } else {
    custom_colors <- leaflet::colorNumeric(palette = "viridis",
                                           domain = c(min(color_labels),
                                                      max(color_labels)))
    colors_out <- custom_colors(color_labels)
  }
  if (return_palette) {
    return(list(colors = colors_out, palette = custom_colors))
  } else {
    return(colors_out)
  }
}
