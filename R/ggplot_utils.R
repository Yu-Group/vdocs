#' Get list of aesthetics to add to ggplot
#' 
#' @description Helper funciton to ignore NULL inputs when adding aesthetics to 
#'   a ggplot
#'   
#' @return Named list of non-NULL inputs.
get_aesthetics <- function(x.str = NULL, y.str = NULL,
                           color.str = NULL, fill.str = NULL,
                           group.str = NULL) {
  aes_list <- list()
  if (!is.null(x.str)) {
    aes_list$x <- substitute(.data[[x.str]], list(x.str = x.str))
  }
  if (!is.null(y.str)) {
    aes_list$y <- substitute(.data[[y.str]], list(y.str = y.str))
  }
  if (!is.null(color.str)) {
    aes_list$color <- substitute(.data[[color.str]], 
                                 list(color.str = color.str))
  }
  if (!is.null(fill.str)) {
    aes_list$fill <- substitute(.data[[fill.str]], list(fill.str = fill.str))
  }
  if (!is.null(group.str)) {
    aes_list$group <- substitute(.data[[group.str]],
                                 list(group.str = group.str))
  }
  return(do.call(ggplot2::aes, aes_list))
}

#' Add color to axis text labels in ggplot
#' 
#' @description Helper function to add color to axis text labels in a ggplot
#'   object.
#'
#' @param plt A ggplot object
#' @param labels Axis text labels
#' @param label.colors Data vector to use for coloring axis text labels
#' @param axis One of "x" or "y" indicating which axis.
#' 
#' @return A ggplot object with the axis text labels colored.
add_axis_text_colors <- function(plt, labels, label.colors = NULL, 
                                 axis = c("x", "y")) {
  axis <- match.arg(axis)
  if (!is.null(label.colors)) {
    if (length(ggplot2::ggplot_build(plt)$layout$panel_params) > 1) {
      stop("Colors cannot be added to axis text labels when facets are used. ",
           sprintf("Set %s.label.colors = NULL.", axis))
    }
    if (identical(axis, "y")) {
      axis_labs <- lapply(rev(ggplot2::ggplot_build(plt)$layout$panel_params),
                          function(x) x$y$get_labels()) %>%
        purrr::reduce(c)  # y axis plot labels from bottom to top
    } else if (identical(axis, "x")) {
      axis_labs <- lapply(ggplot2::ggplot_build(plt)$layout$panel_params,
                          function(x) x$x$get_labels()) %>%
        purrr::reduce(c)  # x axis plot labels from left to right
    }
    labs_df <- data.frame(lab_x = 1, lab_y = 1, 
                          lab_color = label.colors, lab = labels) %>%
      dplyr::left_join(x = data.frame(lab = axis_labs), y = ., by = "lab")
    plt <- plt +
      ggplot2::geom_point(ggplot2::aes(x = lab_x, y = lab_y, color = lab_color), 
                          data = labs_df, size = -1)
    # get color and add color legend
    lab_colors <- get_custom_color_palette(labs_df$lab_color)
    if (is.factor(label.colors)) {
      plt <- plt +
        ggplot2::guides(
          color = ggplot2::guide_legend(override.aes = list(size = 3))
        ) +
        ggplot2::scale_color_manual(values = lab_colors)
    } else {
      plt <- plt + ggplot2::scale_colour_viridis(discrete = F)
    }
    if (identical(axis, "x")) {
      plt <- plt + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(color = lab_colors))
    } else if (identical(axis, "y")) {
      plt <- plt + 
        ggplot2::theme(axis.text.y = ggplot2::element_text(color = lab_colors))
    }
  }
  return(plt)
}

#' Converts data vector to color vector
#' 
#' @returns If \code{return_palette = FALSE}, returns a vector of colors. If 
#'   \code{return_palette = TRUE}, returns a list of two:
#'   \describe{
#'   \item{colors}{Vector of colors.}
#'   \item{palette}{Color palette used to convert data to colors.}
#'   }
get_custom_color_palette <- function(color.labels, return_palette = FALSE) {
  if (is.factor(color.labels)) {
    if (nlevels(color.labels) <= 8) {
      custom_colors <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")
      custom_colors[2] <- custom_colors[1]
      custom_colors[1] <- "#FF9300"
      colors_out <- custom_colors[color.labels]
    } else {
      custom_colors <- leaflet::colorFactor(
        palette = "viridis", domain = levels(color.labels)
      )
      colors_out <- custom_colors(color.labels)
    }
  } else {
    custom_colors <- leaflet::colorNumeric(palette = "viridis", 
                                           domain = c(min(color.labels), 
                                                      max(color.labels)))
    colors_out <- custom_colors(color.labels)
  }
  if (return_palette) {
    return(list(colors = colors_out, palette = custom_colors))
  } else {
    return(colors_out)
  }
}
