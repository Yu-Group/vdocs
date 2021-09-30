plot_X_distribution <- function(X, type = c("density", "histogram", "boxplot"),
                                ...) {
  type <- match.arg(type)#, several.ok = TRUE)
  
  # make plot
  if (type == "histogram") {
    plt <- plotHistogram(data = X, position = "identity", ...) +
      ggplot2::labs(x = "X", y = "Frequency", title = "Overall X Distribution")
  }
  if (type == "density") {
    plt <- plotDensity(data = X, ...) +
      ggplot2::labs(x = "X", y = "Density", title = "Overall X Distribution")
  }
  if (type == "boxplot") {
    plt <- plotBoxplot(data = X, horizontal = T, ...) +
      ggplot2::labs(y = "X", y = "", title = "Overall X Distribution")
  }
  return(plt)
}

plot_y_distribution <- function(y, type = c("density", "histogram", "boxplot",
                                            "bar"),
                                ...) {
  type <- match.arg(type)
  y <- data.frame(y = y)
  # make plot
  if (type == "histogram") {
    plt <- plotHistogram(data = y, x.str = "y", position = "identity", ...) +
      ggplot2::labs(x = "y", y = "Frequency", title = "Overall y Distribution")
  } else if (type == "density") {
    plt <- plotDensity(data = y, x.str = "y", ...) +
      ggplot2::labs(x = "y", y = "Density", title = "Overall y Distribution")
  } else if (type == "boxplot") {
    plt <- plotBoxplot(data = y, x.str = "y", horizontal = T, ...) +
      ggplot2::labs(y = "y", title = "Overall y Distribution")
  } else if (type == "bar") {
    plt <- plotBarplot(data = y, x.str = "y", 
                       position = "fill", stat = "count", ...) +
      ggplot2::labs(x = "y", y = "Frequency", title = "Overall y Distribution")
  }
  return(plt)
}

#' Plot pretty pair plots using custom ggplot theme.
#' 
#' @description Wrapper around [GGally::ggpairs()] that plots pretty pair plots 
#'   using a custom ggplot theme.
#' 
#' @param data Data frame to use for plot.
#' @param columns Vector of column indicies or names to plot.
#' @param color (Optional) Data vector to use as colors for lower ggplot panels.
#' @param color.upper (Optional) Data vector to use as colors for upper ggplot 
#'   panels.
#' @param color.label Character string. Label for color legend title (used in
#'   lower ggplot panels).
#' @param color.upper.label Character string for color.upper legend title (used
#'   in upper ggplot panels).
#' @param manual.color Vector of colors to set manual color scheme corresponding 
#'   to color argument (i.e., the color scheme in the lower panels).
#' @param manual.color.upper Vector of colors to set manual color scheme 
#'   corresponding to color.upper argument (i.e., the color scheme in the upper 
#'   panels).
#' @param columnLabels Label names to be displayed on strips.
#' @param size Point size for [ggplot2::geom_point()].
#' @param alpha Alpha value for [ggplot2::geom_point()].
#' @param cor.text.size Size of correlation text.
#' @param show.upper Logical. Show we show subplots in upper panels?
#' @param subsample Proportion of points to subsample and plot.
#' @param title Character string. Title of plot.
#' @param drop Logical. Whether or not to drop factors with no observations.
#' @param theme_function function which adds theme() to ggpairs() object. If 
#'   \code{NULL}, add \code{prettyGGplotTheme()} to [GGally::ggpairs()] object.
#' @param show.plot Logical. Should this plot be printed? Default \code{FALSE}.
#' @param ... Other arguments to pass to \code{prettyGGplotTheme()} or 
#'   \code{theme_function()}
#'   
#' @return A [GGally::ggpairs] object.
#'
#' @examples
#' plotPairs(data = iris, columns = 1:ncol(iris), color = iris$Species)
#' 
plotPairs <- function(data, columns, color = NULL, color.upper = NULL, 
                      color.label = "", color.upper.label = "",
                      manual.color = NULL, manual.color.upper = NULL,
                      columnLabels = colnames(data[, columns]), title = "",
                      size = .5, alpha = .5, cor.text.size = 3.5, subsample = 1,
                      show.upper = T, drop = F, theme_function = NULL,
                      show.plot = F, ...) {
  
  # adding labels for colors
  plt_df <- as.data.frame(data)
  if (!is.null(color)) {
    plt_df <- plt_df %>%
      dplyr::mutate(color = color)
  }
  if (!is.null(color.upper)) {
    plt_df <- plt_df %>%
      dplyr::mutate(color.upper = color.upper)
  }
  
  # subsample points
  if (subsample != 1) {
    plt_df <- dplyr::slice_sample(plt_df, prop = subsample, replace = FALSE)
  }
  
  # check if show correlations in upper panel
  if (show.upper) {
    plot_cor <- GGally::wrap("cor", size = cor.text.size)
  } else {
    plot_cor <- "blank"
  }
  
  # get columns for plotting
  if (!is.numeric(columns)) {
    columns <- which(colnames(data) %in% columns)
  }
  
  # helper variables
  plot_points <- GGally::wrap("points", size = size, alpha = alpha)
  plot_density <- GGally::wrap("densityDiag", alpha = .5)
  
  if (is.null(color) & is.null(color.upper)) {  # no colors
    plt <- GGally::ggpairs(
      data = plt_df,
      columns = columns,
      diag = list(continuous = plot_density),
      lower = list(continuous = plot_points),
      upper = list(continuous = plot_cor),
      title = title,
      columnLabels = columnLabels
    )
    if (is.null(theme_function)) {
      plt <- plt + prettyGGplotTheme(...)
    } else {
      plt <- theme_function(plt, ...)
    }
    
  } else if (is.null(color.upper)) {  # one color
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
        ggplot2::labs(color = color.label)
      if (is.null(manual.color)) {
        legend_plt <- legend_plt + prettyGGplotColor(color = color, drop = drop)
      } else {
        legend_plt <- legend_plt +
          ggplot2::scale_color_manual(values = manual.color, drop = drop)
      }
      if (is.null(theme_function)) {
        legend_plt <- legend_plt + prettyGGplotTheme(...)
      } else {
        legend_plt <- theme_function(legend_plt, ...)
      }
      legend <- GGally::grab_legend(legend_plt)
    }
    
    if (is.factor(color)) {
      upper_ls <- list(continuous = plot_cor)
    } else {
      if (show.upper) {
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
      columnLabels = columnLabels
    ) 
    
    # change color palette for all panels
    for (i in 1:plt$nrow) {
      for (j in 1:plt$ncol) {
        if (is.null(manual.color)) {
          plt[i, j] <- plt[i, j] +
            prettyGGplotColor(color = color, drop = drop) +
            prettyGGplotFill(fill = color, drop = drop)
        } else {
          plt[i, j] <- plt[i, j] +
            ggplot2::scale_color_manual(values = manual.color, drop = drop) +
            ggplot2::scale_fill_manual(values = manual.color, drop = drop)
        }
      }
    }
    
    plt <- plt + ggplot2::labs(color = color.label, fill = color.label)
    if (is.null(theme_function)) {
      plt <- plt + prettyGGplotTheme(...)
    } else {
      plt <- theme_function(plt, ...)
    }
    
  } else {
    # make lower scatter plots and color by color for the ggpairs plots
    lowerContinuous <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      p <- ggplot2::ggplot(data) +
        ggplot2::aes_string(x = .data[[x_str]], y = .data[[y_str]],
                            color = color) +
        ggplot2::geom_point(size = size, alpha = alpha)
      return(p)
    }
    
    # make upper scatter plots and color by color.upper for the ggpairs plots
    upperContinuous <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      p <- ggplot2::ggplot(data) +
        ggplot2::aes(x = .data[[x_str]], y = .data[[y_str]], 
                     color = color.upper) +
        ggplot2::geom_point(size = size, alpha = alpha)
      return(p)
    }
    
    # make upper boxplots and color by color.upper for the ggpairs plots
    upperCombo <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      if (is.factor(data[[x_str]])) {
        p <- ggplot2::ggplot(data) +
          ggplot2::aes(x = .data[[x_str]], y = .data[[y_str]], 
                       fill = color.upper) +
          ggplot2::geom_boxplot()
      } else {
        p <- ggplot2::ggplot(data) +
          ggplot2::aes(x = .data[[y_str]], y = .data[[x_str]], 
                       fill = color.upper) +
          ggplot2::geom_boxplot() +
          ggplot2::coord_flip()
      }
      return(p)
    }
    
    # make upper bar plots and color by color.upper for the ggpairs plots
    upperDiscrete <- function(data, mapping, ...) {
      x_str <- as.character(mapping$x[2])
      y_str <- as.character(mapping$y[2])
      p <- ggplot2::ggplot(data) +
        ggplot2::aes(x = .data[[x_str]], fill = color.upper) +
        ggplot2::facet_grid(.data[[y]] ~ .) +
        ggplot2::geom_bar()
      return(p)
    }
    
    # number of color factors
    nfactors <- is.factor(color) + is.factor(color.upper)
    
    if (length(columns) == 1) {  # in this case, plot density
      if (nfactors == 0) {
        plt <- GGally::ggpairs(data = plt_df, columns = columns, 
                               title = title, legend = c(1, 1), 
                               columnLabels = columnLabels) + 
          ggplot2::labs(color = color.label, fill = color.label)
      } else {
        if (is.factor(color)) {
          plt_df$plt_color <- color
        } else {
          plt_df$plt_color <- color.upper
        }
        plt <- GGally::ggpairs(
          data = plt_df,
          columns = columns,
          mapping = ggplot2::aes(color = plt_color),
          diag = list(continuous = plot_density),
          title = title,
          legend = c(1, 1),
          columnLabels = columnLabels
        ) + ggplot2::labs(color = color.label, fill = color.label)
        if (is.null(manual.color)) {
          plt[1, 1] <- plt[1, 1] +
            prettyGGplotColor(color = plt_df$plt_color, drop = drop) +
            prettyGGplotFill(fill = plt_df$plt_color, drop = drop)
        } else {
          plt[1, 1] <- plt[1, 1] +
            ggplot2::scale_color_manual(values = manual.color, drop = drop) +
            ggplot2::scale_fill_manual(values = manual.color, drop = drop)
        }
      }
      
    } else {
      # grab color and color.upper legends
      legend_plt_df <- plt_df %>%
        dplyr::mutate(legend_x = data[, columns[1]],
                      legend_y = data[, columns[2]])
      legend_plt1 <- ggplot2::ggplot(legend_plt_df) +
        ggplot2::geom_point(ggplot2::aes(x = legend_x, y = legend_y,
                                         color = color)) +
        ggplot2::labs(color = color.label, fill = color.label) +
        ggplot2::theme(legend.position = "bottom")
      legend_plt2 <- ggplot2::ggplot(legend_plt_df) +
        ggplot2::geom_point(ggplot2::aes(x = legend_x, y = legend_y,
                                         color = color.upper)) +
        ggplot2::labs(color = color.upper.label, fill = color.upper.label) +
        ggplot2::theme(legend.position = "bottom")
      if (is.null(manual.color)) {
        legend_plt1 <- legend_plt1 +
          prettyGGplotColor(color = color, drop = drop)
      } else {
        legend_plt1 <- legend_plt1 +
          ggplot2::scale_color_manual(values = manual.color, drop = drop)
      }
      if (is.null(manual.color.upper)) {
        legend_plt2 <- legend_plt2 +
          prettyGGplotColor(color = color.upper, option = "D",
                            viridis = T, drop = drop)
      } else {
        legend_plt2 <- legend_plt2 +
          ggplot2::scale_color_manual(values = manual.color.upper, drop = drop)
      }
      if (is.null(theme_function)) {
        legend_plt1 <- legend_plt1 + prettyGGplotTheme(...)
        legend_plt2 <- legend_plt2 + prettyGGplotTheme(...)
      } else {
        legend_plt1 <- theme_function(legend_plt1, ...)
        legend_plt2 <- theme_function(legend_plt2, ...)
      }
      legend1 <- GGally::grab_legend(legend_plt1)
      legend2 <- GGally::grab_legend(legend_plt2)
      
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
          columnLabels = columnLabels
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
          columnLabels = columnLabels
        )
      } else {
        if (is.factor(color)) {
          plt_df$plt_color <- color
        } else {
          plt_df$plt_color <- color.upper
        }
        plt <- GGally::ggpairs(
          data = plt_df,
          columns = columns,
          mapping = ggplot2::aes(color = plt_color),
          diag = list(continuous = plot_density),
          lower = list(continuous = lowerContinuous),
          upper = list(continuous = upperContinuous),
          title = title,
          columnLabels = columnLabels
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
                if (is.null(manual.color)) {
                  plt[i, j] <- plt[i, j] +
                    prettyGGplotFill(fill = color, drop = drop)
                } else {
                  plt[i, j] <- plt[i, j] +
                    ggplot2::scale_fill_manual(values = manual.color,
                                               drop = drop)
                }
              } else {
                if (is.null(manual.color.upper)) {
                  plt[i, j] <- plt[i, j] +
                    prettyGGplotFill(fill = color.upper, option = "D", 
                                     viridis = T, drop = drop)
                } else {
                  plt[i, j] <- plt[i, j] +
                    ggplot2::scale_fill_manual(values = manual.color.upper, 
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
                if (is.null(manual.color)) {
                  plt[i, j] <- plt[i, j] +
                    prettyGGplotColor(color = color, drop = drop)
                } else {
                  plt[i, j] <- plt[i, j] +
                    ggplot2::scale_color_manual(values = manual.color,
                                                drop = drop)
                }
              } else {
                if (is.null(manual.color.upper)) {
                  plt[i, j] <- plt[i, j] +
                    prettyGGplotColor(color = color.upper, option = "D", 
                                      viridis = T, drop = drop)
                } else {
                  plt[i, j] <- plt[i, j] +
                    ggplot2::scale_color_manual(values = manual.color.upper,
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
          GGally::ggmatrix_gtable(plt + prettyGGplotTheme(...)),
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
        plt <- plt + prettyGGplotTheme(...)
      } else {
        plt <- theme_function(plt, ...)
      }
    }
  }
  
  if (show.plot) {
    print(plt)
  }
  return(plt)
}

#' Plot pretty PCA plots using custom ggplot theme.
#'
#' @description Run PCA on the given data matrix and generates PC plots for the
#'   specified principal components.
#'
#' @param X Data matrix or data.frame on which to perform PCA. Must specify 
#'   either \code{X} or \code{pca.out}.
#' @param pca.out Output of previous run of plotPCA() to avoid re-computing SVDs 
#'   (i.e., the PC loadings and scores) again. Must specify either \code{X} or
#'   \code{pca.out}. Ignored if \code{X} is provided.
#' @param npcs Number of top PCs to plot. Must specify either \code{npcs} or
#'   \code{pcs}.
#' @param pcs Vector of which PCs to show. Must specify either \code{npcs} or
#'   \code{pcs}. Ignored if \code{npcs} is provided.
#' @param color (Optional) Data vector to use as colors for lower ggplot panels.
#' @param color.upper (Optional) Data vector to use as colors for upper ggplot 
#'   panels.
#' @param color.label Character string. Label for color legend title (used in
#'   lower ggplot panels).
#' @param color.upper.label Character string for color.upper legend title (used
#'   in upper ggplot panels).
#' @param manual.color Vector of colors to set manual color scheme corresponding 
#'   to color argument (i.e., the color scheme in the lower panels).
#' @param manual.color.upper Vector of colors to set manual color scheme 
#'   corresponding to color.upper argument (i.e., the color scheme in the upper 
#'   panels).
#' @param size Point size for [ggplot2::geom_point()].
#' @param alpha Alpha value for [ggplot2::geom_point()].
#' @param subsample Proportion of points to subsample and plot.
#' @param show.var Logical. Whether or not to show the proportion of variance
#'   explained in axes labels.
#' @param center Logical. Whether or not to center data for PCA.
#' @param scale Logical. Whether or not to scale data for PCA.
#' @param title Character string. Title for ggplot.
#' @param show.plot Logical. Should this plot be printed? Default \code{FALSE}.
#' @param ... Other arguments to pass to \code{prettyGGplotTheme()}
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
#' out <- plotPCA(X = iris[, -5], npcs = 3, color = iris$Species)
#' out$plot
#' iris2 <- data.frame(iris, z = rep(letters[1:2], length.out = nrow(iris)))
#' out <- plotPCA(X = iris2[, -c(5, 6)], npcs = 3,
#'                color = iris2$Species, color.upper = iris2$z)
#' out$plot
#' 
plotPCA <- function(X, pca.out, npcs, pcs, 
                    color = NULL, color.upper = NULL, 
                    color.label = "", color.upper.label = "",
                    manual.color = NULL, manual.color.upper = NULL,
                    size = .5, alpha = 1, subsample = 1,
                    show.var = T, center = T, scale = F,
                    title = "", show.plot = F, ...) {
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
      u = pca.out$scores,
      v = pca.out$loadings,
      var_explained = pca.out$var.explained
    )
    d <- pca.out$d
  }
  
  # compute and show proportion of variance
  if (show.var) { 
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
  if (!is.null(color.upper)) {
    plt_df <- plt_df %>% dplyr::mutate(color.upper = color.upper)
  }
    
  # subsample points
  if (subsample != 1) {
    plt_df <- dplyr::slice_sample(plt_df, prop = subsample, replace = FALSE)
  }
  
  if (npcs == 2) {  # plot ggplot object instead of ggpairs
    if (!is.null(color)) {  # plot with color
      plt <- ggplot(plt_df) +
        aes(x = X1, y = X2, color = color) +
        geom_point(alpha = alpha, size = size) +
        labs(
          x = paste0("PC1", var_explained_str[1]),
          y = paste0("PC2", var_explained_str[2]),
          color = color.label, title = title
        ) +
        prettyGGplotTheme(...)
      if (is.null(manual.color)) {
        plt <- plt + prettyGGplotColor(color = plt_df$color)
      } else {
        plt <- plt + scale_color_manual(values = manual.color)
      }
    } else {  # plot without color
      plt <- ggplot(plt_df) +
        aes(x = X1, y = X2) +
        geom_point(alpha = alpha, size = size) +
        labs(
          x = paste0("PC1", var_explained_str[1]),
          y = paste0("PC2", var_explained_str[2]),
          title = title
        ) +
        prettyGGplotTheme(...)
    }
  } else {
    plt <- plotPairs(data = plt_df, columns = pcs, title = title, 
                     size = size, alpha = alpha, show.upper = F, drop = F, 
                     color = color, color.upper = color.upper, 
                     color.label = color.label, 
                     color.upper.label = color.upper.label,
                     manual.color = manual.color, 
                     manual.color.upper = manual.color.upper,
                     columnLabels = paste0("PC", pcs, var_explained_str[pcs]),
                     ...)
  }
  if (show.plot) {
    print(plt)
  }

  return(list(plot = plt, scores = X_svd$u, loadings = X_svd$v, d = d,
              var.explained = var_explained))
}

#' Plot pretty heatmaps using custom ggplot theme.
#' 
#' @description Generates nice heatmaps of a data matrix.
#' 
#' @param X Data matrix or data frame to use for heatmap.
#' @param y.labels y-axis labels in heatmap.
#' @param x.labels x-axis labels in heatmap.
#' @param y.labels.num Logical; whether or not y labels are numeric/continuous.
#' @param x.labels.num Logical; whether or not x labels are numeric/continuous.
#' @param y.label.colors (Optional) Data vector to use for coloring y-axis text.
#' @param x.label.colors (Optional) Data vector to use for coloring x-axis text.
#' @param y.groups Data vector of group ids to use for grouping rows in heatmap.
#' @param x.groups Data vector of group ids to use for grouping columns in 
#'   heatmap.
#' @param center Logical; whether or not to center columns of \code{X}.
#' @param scale Logical; whether or not to scale columns of \code{X}.
#' @param z.range Vector of length 2 with the min and max of the fill color 
#'   range for heatmap. Used to set bounds for fill legend.
#' @param text.size Numeric; size of text on heatmap. If \code{text.size} = 0 
#'   (default), no text is shown.
#' @param theme One of "default" or "blank", which specifies ggplot theme.
#' @param y.orient One of "identity" or "ordered". If "identity", plots heatmap
#'   of \code{X} as if it is an image. If "ordered", plots first row of \code{X}
#'   at the bottom of the heatmap and the last row of \code{X} on top.
#' @param size Size argument in [ggplot2::geom_tile()] to avoid white lines in
#'   continuous heatmap.
#' @param viridis Logical; whether or not to use viridis fill scheme.
#' @param option Argument indicating viridis palette name.
#' @param col_quantile Logical; whether or not to use quantiles to construct 
#'   fill color scale. Deafult is \code{FALSE}.
#' @param n_quantiles Number of quantiles for color scheme. Used only if
#'   \code{col_quantile = TRUE}.
#' @param manual.fill (Optional) Either "temperature" or a data vector of colors 
#'   for the fill color scale.
#' @param show.plot Logical. Should this plot be printed? Default \code{FALSE}.
#' @param ... Other arguments to pass to \code{prettyGGplotTheme()}.
#' 
#' @return A ggplot object.
#' 
#' @examples 
#' df <- as.data.frame(matrix(1:100, nrow = 10, byrow = 10)) 
#' plotHeatmap(df,  y.orient = "identity")
#' plotHeatmap(df,  y.orient = "ordered")
#' 
plotHeatmap <- function(X, y.labels = rownames(X), x.labels = colnames(X),
                        y.labels.num = FALSE, x.labels.num = FALSE,
                        y.label.colors = NULL, x.label.colors = NULL,
                        y.groups = NULL, x.groups = NULL,
                        center = FALSE, scale = FALSE, z.range = NULL,
                        text.size = 0, theme = "default", y.orient = "identity",
                        size = 0, viridis = T, option = "C", 
                        col_quantile = F, n_quantiles = 5,
                        manual.fill = NULL, show.plot = F, ...) {
  if (!all(sapply(X, is.numeric))) {
    stop("X must contain only numeric data. Please remove non-numeric columns.")
  }
  
  y.labels <- y.labels
  x.labels <- x.labels
  y.label.colors <- y.label.colors
  x.label.colors <- x.label.colors

  if (any(duplicated(y.labels)) | any(duplicated(x.labels))) {
    stop("x.labels and y.labels cannot contain duplicates.")
  }
  
  # center/scale X if specified
  if (center | scale) {
    X <- scale(X, center = center, scale = scale)
  }
  
  # get range
  if (is.null(z.range)) {
    z.min <- min(X, na.rm = T)
    z.max <- max(X, na.rm = T)
  } else {
    z.min <- z.range[1]
    z.max <- z.range[2]
  }
  
  # convert to long df to plot
  X_long <- as.data.frame(X) %>%
    setNames(x.labels)
  if (y.labels.num) {
    X_long <- X_long %>%
      dplyr::mutate(y = as.numeric(y.labels)) %>%
      tidyr::gather(key = "x", value = "fill", -y) 
  } else {
    X_long <- X_long %>%
      dplyr::mutate(y = forcats::fct_rev(forcats::fct_inorder(y.labels))) %>%
      tidyr::gather(key = "x", value = "fill", -y) 
  }
  if (x.labels.num) {
    X_long <- X_long %>% dplyr::mutate(x = as.numeric(x))
  } else {
    X_long <- X_long %>% dplyr::mutate(x = factor(x, levels = x.labels))
  }
  
  # add group labels to data frame
  if (!is.null(x.groups)) {
    X_long$x.group <- X_long %>%
      dplyr::left_join(y = data.frame(x = x.labels, x.group = x.groups), 
                       by = "x") %>%
      dplyr::pull(x.group)
  }
  if (!is.null(y.groups)) {
    X_long$y.group <- X_long %>%
      dplyr::left_join(y = data.frame(y = y.labels, y.group = y.groups),
                       by = "y") %>%
      dplyr::pull(y.group)
  }
  
  # base plot
  if (x.labels.num | y.labels.num) {
    plt <- ggplot2::ggplot(X_long) +
      ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = fill, color = fill), 
                         size = size) +
      ggplot2::guides(color = FALSE)
  } else {
    plt <- ggplot2::ggplot(X_long) +
      ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = fill))
  }
  
  # add group labels to plot
  if (!is.null(x.groups) & !is.null(y.groups)) {
    plt <- plt +
      ggplot2::facet_grid(y.group ~ x.group, space = "free", scales = "free")
  } else if (!is.null(x.groups)) {
    plt <- plt +
      ggplot2::facet_grid(~ x.group, space = "free", scales = "free")
  } else if (!is.null(y.groups)) {
    plt <- plt +
      ggplot2::facet_grid(y.group ~., space = "free", scales = "free")
  }
  
  # add text if specified
  if (text.size > 0) {
    plt <- plt +
      ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = fill), 
                         color = "black", size = text.size)
  }
  
  # add theme
  if (identical(theme, "default")) {
    plt <- plt + prettyGGplotTheme(...)
  } else if (identical(theme, "blank")) {
    plt <- plt + ggplot2::theme_void()
  } else {
    stop("Unknown theme argument.")
  }
  plt <- plt +
    ggplot2::theme(panel.spacing = grid::unit(0, "lines"),
                   panel.border = ggplot2::element_rect(color = "black", 
                                                        fill = NA, size = 1))
  
  # add color
  if (!col_quantile | is.factor(X_long$fill)) {
    if (is.null(manual.fill)) {
      plt <- plt +
        prettyGGplotFill(fill = fill, viridis = viridis, option = option) +
        prettyGGplotColor(color = fill, viridis = viridis, option = option)
    } else {
      if (manual.fill == "temperature") {
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
      } else if (manual.fill == "cor_temperature") {
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
          ggplot2::scale_fill_manual(values = manual.fill) +
          ggplot2::scale_color_manual(values = manual.fill)
      }
    }
  } else {
    if (!is.null(manual.fill)) {
      if (manual.fill == "temperature") {
        heat_pal <- c("blue", "white", "red")
      } else {
        heat_pal <- manual.fill
      }
    } else {
      heat_pal <- viridis::viridis(n = n_quantiles, option = option)
    }
    probs <- seq(0, 1, length = length(heat_pal))
    quantiles <- quantile(X_long$fill, probs, na.rm = T)
    heat_pal_values <- (quantiles - min(quantiles)) /
      (max(quantiles) - min(quantiles))
    plt <- plt + 
      ggplot2::scale_fill_gradientn(values = heat_pal_values, 
                                    colors  = heat_pal) +
      ggplot2::scale_color_gradientn(values = heat_pal_values, 
                                     colors = heat_pal)
  }
  
  # y axis orientation
  if (!x.labels.num) {
    plt <- plt + ggplot2::scale_x_discrete(expand = c(0, 0))
  } else {
    plt <- plt + ggplot2::scale_x_continuous(expand = c(0, 0))
  }
  if (!y.labels.num) {
    if (identical(y.orient, "identity")) {
      plt <- plt + ggplot2::scale_y_discrete(expand = c(0, 0))
    } else if (identical(y.orient, "ordered")) {
      plt <- plt + ggplot2::scale_y_discrete(expand = c(0, 0), limits = rev)
    }
  } else {
    plt <- plt + ggplot2::scale_y_continuous(expand = c(0, 0))
  }
  
  # add color to x and y axis text
  plt <- plt %>%
    add_axis_text_colors(labels = y.labels, label.colors = y.label.colors,
                         axis = "y") %>%
    add_axis_text_colors(labels = x.labels, label.colors = x.label.colors,
                         axis = "x")
  
  if (show.plot) {
    print(plt)
  }
  return(plt)
}

#' Plot pretty clustered heatmaps using custom ggplot theme.
#' 
#' @description Generates nice clustered heatmaps of a data matrix.
#
#' @param X Data matrix or data frame to use for heatmap.
#' @param y.labels y-axis labels in heatmap.
#' @param x.labels x-axis labels in heatmap.
#' @param y.labels.num Logical; whether or not y labels are numeric/continuous.
#' @param x.labels.num Logical; whether or not x labels are numeric/continuous.
#' @param y.label.colors (Optional) Data vector to use for coloring y-axis text.
#' @param x.label.colors (Optional) Data vector to use for coloring x-axis text.
#' @param y.groups Data vector of group ids to use for grouping rows in heatmap.
#' @param x.groups Data vector of group ids to use for grouping columns in 
#'   heatmap.
#' @param clust.x Logical; whether or not to cluster columns.
#' @param clust.y Logical; whether or not to cluster rows.
#' @param clust.x.wi.group Logical; whether or not cluster within x.groups.
#' @param clust.y.wi.group Logical; whether or not cluster within y.groups.
#' @param dist.metric.x Distance metric for clustering columns (see 
#'   [stats::dist()]).
#' @param dist.metric.y Distance metric for clustering rows (see 
#'   [stats::dist()]).
#' @param dist.mat.x Distance matrix for clustering columns (optional). Must 
#'   provide either \code{dist.metric} or \code{dist.mat} if \code{clust.x = T}.
#' @param dist.mat.x Distance matrix for clustering rows (optional). Must 
#'   provide either \code{dist.metric} or \code{dist.mat} if \code{clust.y = T}.
#' @param linkage.x Type of linkage for clustering columns (see 
#'   [stats::hclust()]).
#' @param linkage.y Type of linkage for clustering rows (see [stats::hclust()]).
#' @inheritParams plotHeatmap
#' 
#' @return A ggplot object.
#' 
#' @examples 
#' df <- as.data.frame(matrix(1:100, nrow = 10, byrow = 10))
#' plotHclustHeatmap(df)
#' 
plotHclustHeatmap <- function(X, y.labels = rownames(X), x.labels = colnames(X),
                              y.labels.num = FALSE, x.labels.num = FALSE,
                              y.label.colors = NULL, x.label.colors = NULL,
                              y.groups = NULL, x.groups = NULL,
                              clust.x = TRUE, clust.y = TRUE,
                              clust.x.wi.group = TRUE, clust.y.wi.group = TRUE,
                              dist.metric.x = "euclidean",
                              dist.metric.y = "euclidean",
                              dist.mat.x = NULL, dist.mat.y = NULL,
                              linkage.x = "ward.D", linkage.y = "ward.D",
                              center = FALSE, scale = FALSE, z.range = NULL,
                              text.size = 0, theme = "default", size = 0,
                              viridis = T, option = "C", 
                              col_quantile = F, n_quantiles = 5,
                              manual.fill = NULL, show.plot = F, ...) {
  
  if (!all(sapply(X, is.numeric))) {
    stop("X must contain only numeric data. Please remove non-numeric columns.")
  }
  if (any(is.na(X)) & (clust.x | clust.y)) {
    stop("NAs found in data. Please remove NAs.")
  }
  
  y.labels <- y.labels
  x.labels <- x.labels
  y.label.colors <- y.label.colors
  x.label.colors <- x.label.colors
  
  if (center | scale) {
    X <- scale(X, center = center, scale = scale)
  }
  
  # helper function to do the clustering and returns index order
  get_clust_order <- function(dist.mat, dist.metric, linkage, groups,
                              clust.wi.group, axis = c("x", "y")) {
    axis <- match.arg(axis)
    if (is.null(dist.mat)) {
      if (identical(axis, "x")) {
        Dmat <- dist(t(X), method = dist.metric)
      } else if (identical(axis, "y")) {
        Dmat <- dist(X, method = dist.metric)
      }
    } else {
      if (!("dist" %in% class(dist.mat))) {
        Dmat <- as.dist(dist.mat)
      } else {
        Dmat <- dist.mat
      }
    }
    if (is.null(groups)) {
      hclust_out <- hclust(Dmat, method = linkage)
      order_idx <- hclust_out$order
    } else if (!clust.wi.group) {
      hclust_out <- hclust(Dmat, method = linkage)
      order_idx <- hclust_out$order
    } else {
      order_idx <- c()
      for (group in unique(groups)) {
        group.idx <- groups == group
        if (sum(group.idx) > 1) {
          Dmat.sub <- as.dist(as.matrix(Dmat)[group.idx, group.idx])
          hclust_out <- hclust(Dmat.sub, method = linkage)
          col_idx <- data.frame(idx = 1:sum(group.idx), col = which(group.idx))
          order_idx <- c(order_idx, 
                         col_idx$col[match(hclust_out$order, col_idx$idx)])
        } else {
          order_idx <- c(order_idx, which(group.idx))
        }
      }
    }
    return(order_idx)
  }
  
  if (clust.x) {
    order_idx <- get_clust_order(dist.mat.x, dist.metric.x, linkage.x, 
                                 x.groups, clust.x.wi.group, axis = "x")
    X <- X[, order_idx]
    x.groups <- x.groups[order_idx]
    x.labels <- x.labels[order_idx]
    x.label.colors <- x.label.colors[order_idx]
  }
  if (clust.y) {
    order_idx <- get_clust_order(dist.mat.y, dist.metric.y, linkage.y, 
                                 y.groups, clust.y.wi.group, axis = "y")
    X <- X[order_idx, ]
    y.groups <- y.groups[order_idx]
    y.labels <- y.labels[order_idx]
    y.label.colors <- y.label.colors[order_idx]
  }
  
  plt <- plotHeatmap(X = X, y.labels = y.labels, x.labels = x.labels, 
                     y.labels.num = y.labels.num, x.labels.num = x.labels.num,
                     y.label.colors = y.label.colors, 
                     x.label.colors = x.label.colors,
                     y.groups = y.groups, x.groups = x.groups,
                     z.range = z.range, text.size = text.size, theme = theme, 
                     y.orient = "identity", size = size,
                     viridis = viridis, option = option, 
                     col_quantile = col_quantile, n_quantiles = n_quantiles, 
                     manual.fill = manual.fill, show.plot = show.plot, ...)
  return(plt)
}

#' Plot pretty correlation heatmaps using custom ggplot theme.
#' 
#' @description Generates nice correlation heatmaps of a data matrix.
#
#' @param X Data matrix or data frame to use for heatmap.
#' @param cor.type Type of correlation. Must be one of "pearson", "kendall", 
#'   or "spearman"
#' @param axis.labels Axis labels for correlation heatmap.
#' @param axis.label.colors (Optional) Data vector to use for coloring axis
#'   text labels.
#' @param clust Logical; whether or not to cluster columns and rows.
#' @param linkage Type of linkage for clustering columns (see 
#'   [stats::hclust()]).
#' @inheritParams 
#' 
#' @return A ggplot object.
#' 
#' @examples 
#' df <- as.data.frame(matrix(1:100, nrow = 10, byrow = 10))
#' plotCorHeatmap(df)
#' 
plotCorHeatmap <- function(X, cor.type = "pearson",
                           axis.labels = colnames(X), axis.label.colors = NULL,
                           clust = TRUE, linkage = "ward.D", z.range = c(-1, 1),
                           text.size = 0, theme = "default",
                           viridis = T, option = "C", 
                           col_quantile = F, n_quantiles = 5,
                           manual.fill = "cor_temperature", show.plot = F, 
                           ...) {
  
  if (!all(sapply(X, is.numeric))) {
    stop("X must contain only numeric data. Please remove non-numeric columns.")
  }
  
  axis.labels <- axis.labels
  axis.label.colors <- axis.label.colors

  cor_mat <- cor(X, method = cor.type, use = "pairwise.complete.obs")
  cor_dist <- as.dist(1 - abs(cor_mat))
  
  # cluster
  if (clust) {
    hclust_out <- hclust(cor_dist, method = linkage)
    cor_mat <- cor_mat[hclust_out$order, hclust_out$order]
    axis.labels <- axis.labels[hclust_out$order]
    axis.label.colors <- axis.label.colors[hclust_out$order]
  }
  
  cor_mat <- round(cor_mat, 2)
  
  plt <- plotHeatmap(X = cor_mat, 
                     y.labels = axis.labels, x.labels = axis.labels, 
                     y.label.colors = axis.label.colors, 
                     x.label.colors = axis.label.colors,
                     z.range = z.range, text.size = text.size, theme = theme, 
                     y.orient = "identity", viridis = viridis, option = option, 
                     col_quantile = col_quantile, n_quantiles = n_quantiles, 
                     manual.fill = manual.fill, show.plot = show.plot, ...)
  return(plt)
}

#' Plot pretty hierarchical clustering dendrograms.
#'
#' @description Run hierarchical clustering on the given data matrix and 
#'   plot the resulting dendrogram using a pretty theme. Also allows for 
#'   coloring of the leaf nodes.
#'
#' @param data Data matrix or data.frame on which to perform hierarchical
#'   clustering.
#' @param leaf.labels (Optional) Text labels for leaf nodes (e.g., 
#'   class/outcome labels).
#' @param leaf.colors (Optional) Data vector to use for coloring leaf nodes.
#' @param dist.metric Distance metric for clustering (see [stats::dist()]).
#' @param dist.mat (Optional) Distance matrix for clustering. Must provide 
#'   either \code{dist.metric} or \code{dist.mat}.
#' @param linkage Rype of linkage for hierarchical clustering (see 
#'   [stats::hclust()]).
#' @param text.size Numeric; size of text for leaf node labels. 
#' @param manual.color Vector of colors to set manual color scheme corresponding
#'   to the \code{leaf.labels}. This sets the color scheme of the leaf text 
#'   labels.
#' @param title Character string. Title of plot.
#' @param show.plot Logical. Should this plot be printed? Default \code{FALSE}.
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
#' out <- plotHclust(data = iris[, -5], leaf.labels = iris$Species,
#'                   leaf.colors = iris$Species)
#' out$plot
#' 
plotHclust <- function(data, 
                       leaf.labels = rownames(data), leaf.colors = NULL, 
                       dist.metric = "euclidean", dist.mat = NULL, 
                       linkage = "ward.D", text.size = 10, 
                       manual.color = NULL, title = NULL, show.plot = F) {

  data <- as.matrix(data)
  if (sum(is.na(data)) > 0) {
    stop("NAs found in data")
  }
  
  # distance matrix
  if (is.null(dist.mat)) {
    Dmat <- dist(data, method = dist.metric)
  } else {
    if (!("dist" %in% class(dist.mat))) {
      Dmat <- as.dist(dist.mat)
    } else {
      Dmat <- dist.mat
    }
  }
  
  # hierarchical clustering
  hclust_out <- hclust(Dmat, method = linkage)
  hclust_dend <- as.dendrogram(hclust_out)
  
  if (!is.null(leaf.colors)) {  # annotate tree leaves
    palette_out <- get_custom_color_palette(leaf.colors, TRUE)
    lab_colors <- palette_out$colors
    color_palette <- palette_out$palette
    lab_colors <- lab_colors[order.dendrogram(hclust_dend)]
    lab_df <- data.frame(x = 0, y = 0, color = leaf.colors)
  } else {
    lab_colors <- "black"
  }
  
  # get leaf labels
  if (is.null(leaf.labels)) {
    dendextend::labels(hclust_dend) <- rep(
      "------", length(dendextend::labels(hclust_dend))
    )
  } else {
    dendextend::labels(hclust_dend) <- leaf.labels[order.dendrogram(hclust_dend)]
  }
  
  if (is.null(title)) {
    title <- sprintf("Hierarchical Clustering: %s Linkage, %s Distance",
                     linkage, dist.metric)
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
                                                           size = text.size))
    )
  )
  
  # add legend
  if (!is.null(leaf.colors)) {  
    if (is.factor(leaf.colors)) {
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
        ggplot2::scale_colour_viridis(discrete = F)
    }
  }
  
  if (show.plot) {
    print(hclust_plt)
  }
  return(list(plot = hclust_plt, hclust = hclust_out, dend = hclust_dend))
}
