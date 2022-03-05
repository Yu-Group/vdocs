test_that("plot_data_split works properly", {
  data(iris)

  plt <- plot_data_split(train = iris, valid = iris, test = iris)
  vdiffr::expect_doppelganger("plot_data_split1", plt)
  plt <- plot_data_split(train = iris, valid = iris, test = iris,
                         by_feature = TRUE)
  vdiffr::expect_doppelganger("plot_data_split1", plt)

  plt <- plot_data_split(train = iris)
  vdiffr::expect_doppelganger("plot_data_split2", plt)

  plt <- plot_data_split(train = iris, valid = iris, test = iris,
                         by_feature = FALSE)
  vdiffr::expect_doppelganger("plot_data_split3", plt)

  plt <- plot_data_split(train = iris, valid = iris, test = iris,
                         plot_type = list(continuous = "histogram"))
  vdiffr::expect_doppelganger("plot_data_split4", plt)
  plt <- plot_data_split(train = iris, valid = iris, test = iris,
                         plot_type = list(continuous = "histogram"),
                         position = "dodge")
  vdiffr::expect_doppelganger("plot_data_split5", plt)

  expect_error(plot_data_split(train = iris, valid = iris, test = iris,
                               plot_type = "something"))
  expect_error(plot_data_split(train = iris, valid = iris, test = iris,
                               plot_type = list(continuous = "something")))
})

test_that("plot_data_distribution works properly", {
  data(iris)

  plt <- plot_data_distribution(data = iris)
  vdiffr::expect_doppelganger("plot_data_distribution1", plt)
  plt <- plot_data_distribution(data = iris, by_feature = TRUE)
  vdiffr::expect_doppelganger("plot_data_distribution1", plt)

  plt <- plot_data_distribution(data = iris, by_feature = FALSE)
  vdiffr::expect_doppelganger("plot_data_distribution2", plt)

  plt <- plot_data_distribution(data = iris,
                                plot_type = list(continuous = "histogram"))
  vdiffr::expect_doppelganger("plot_data_distribution3", plt)

  expect_error(plot_data_distribution(data = iris, plot_type = "something"))
  expect_error(
    plot_data_distribution(data = iris,
                           plot_type = list(continuous = "something"))
  )
})

test_that("plot_data_heatmap works properly", {
  data(iris)
  X <- iris %>% dplyr::select(-Species)
  y_factor <- iris$Species
  y_numeric <- 1:nrow(iris)

  plt <- plot_data_heatmap(X = X, y = y_factor)
  vdiffr::expect_doppelganger("plot_data_heatmap1", plt)
  plt <- plot_data_heatmap(X = X, y = y_numeric)
  vdiffr::expect_doppelganger("plot_data_heatmap2", plt)

  plt <- plot_data_heatmap(X = X, y = y_factor, clust_rows = FALSE)
  vdiffr::expect_doppelganger("plot_data_heatmap3", plt)
  plt <- plot_data_heatmap(X = X, y = y_factor,
                           clust_rows = FALSE, clust_cols = FALSE)
  vdiffr::expect_doppelganger("plot_data_heatmap4", plt)

  expect_error(plot_data_heatmap(X = iris, y = y_numeric))
  expect_error(plot_data_heatmap(X = X))
})

test_that("plot_pairs works properly", {
  data(iris)
  X <- iris %>%
    dplyr::mutate(Species2 = as.factor(rep(levels(Species),
                                           length.out = nrow(iris))))

  expect_error(plot_pairs(data = iris))
  expect_error(plot_pairs(data = iris, columns = 1:2,
                          color = as.character(iris$Species)),
               NA)

  plt <- plot_pairs(data = X, columns = 1:6)
  vdiffr::expect_doppelganger("plot_pairs1", plt)
  plt <- plot_pairs(data = X, columns = colnames(X))
  vdiffr::expect_doppelganger("plot_pairs1", plt)

  plt <- plot_pairs(data = X, columns = 1:6, color = X$Species)
  vdiffr::expect_doppelganger("plot_pairs2", plt)
  plt <- plot_pairs(data = X, columns = 1:6, color = X$Sepal.Length)
  vdiffr::expect_doppelganger("plot_pairs3", plt)

  plt <- plot_pairs(data = X, columns = 1:6,
                    color = X$Species, color_upper = X$Sepal.Length)
  vdiffr::expect_doppelganger("plot_pairs4", plt)
  plt <- plot_pairs(data = X, columns = 1:6,
                    color = X$Species, color_upper = X$Species2)
  vdiffr::expect_doppelganger("plot_pairs5", plt)
  plt <- plot_pairs(data = X, columns = 1:6,
                    color = X$Species, color_upper = X$Species)
  vdiffr::expect_doppelganger("plot_pairs2", plt)
})

test_that("plot_pca works properly", {
  data(iris)
  X <- iris %>% dplyr::select(-Species)

  expect_error(plot_pca(X = X))
  expect_error(plot_pca(X = iris, pcs = 1:3))

  # basic usage of plot_pca
  out <- plot_pca(X = X, npcs = 3, color = iris$Species)
  expect_equal(names(out),
               c("plot", "scores", "loadings", "d", "var_explained"))
  vdiffr::expect_doppelganger("plot_pca1", out$plot)

  # using cached version of plot_pca
  out2 <- plot_pca(pca_obj = out, pcs = 1:3, color = iris$Species)
  expect_equal(out[which(names(out) != "plot")],
               out2[which(names(out) != "plot")])
})

test_that("plot_hclust works properly", {
  data(iris)
  X <- iris %>% dplyr::select(-Species)

  out <- plot_hclust(data = X)
  expect_equal(names(out), c("plot", "hclust", "dend"))
  vdiffr::expect_doppelganger("plot_hclust1", out$plot)

  out <- plot_hclust(data = X, leaf_colors = iris$Species)
  vdiffr::expect_doppelganger("plot_hclust2", out$plot)
})
