test_that("plot_heatmap works properly", {
  data(iris)
  df <- as.data.frame(matrix(1:100, nrow = 10, byrow = 10))

  # check basic usage of plot_heatmap
  plt <- plot_heatmap(df)
  vdiffr::expect_doppelganger("plot_heatmap1", plt)
  plt <- plot_heatmap(df, y_orient = "ordered")
  vdiffr::expect_doppelganger("plot_heatmap2", plt)

  # check *_groups usage of plot_heatmap
  plt <- plot_heatmap(df, y_groups = rep(letters[1:2], length.out = nrow(df)))
  vdiffr::expect_doppelganger("plot_heatmap3", plt)
  plt <- plot_heatmap(df, y_groups = rep(letters[1:2], length.out = nrow(df)),
                      x_groups = rep(LETTERS[1:2], length.out = ncol(df)))
  vdiffr::expect_doppelganger("plot_heatmap4", plt)

  # check *text_num usage of plot_heatmap
  plt <- plot_heatmap(df, ytext_num = TRUE)
  vdiffr::expect_doppelganger("plot_heatmap5", plt)

  # check viridis_option usage of plot_heatmap
  plt <- plot_heatmap(df, viridis_option = "D")
  vdiffr::expect_doppelganger("plot_heatmap6", plt)

  # check renaming of axis text labels
  plt <- plot_heatmap(df,
                      ytext_labels = letters[1:nrow(df)],
                      xtext_labels = 1:ncol(df))
  vdiffr::expect_doppelganger("plot_heatmap7", plt)

  # check axis text coloring usage of plot_heatmap
  plt <- plot_heatmap(df,
                      ytext_colors = rep(letters[1:2], length.out = nrow(df)))
  vdiffr::expect_doppelganger("plot_heatmap8", plt)
  plt <- plot_heatmap(df,
                      xtext_colors = rep(LETTERS[1:2], length.out = nrow(df)))
  vdiffr::expect_doppelganger("plot_heatmap9", plt)
  # plt <- plot_heatmap(df,
  #                     ytext_colors = rep(letters[1:2], length.out = nrow(df)),
  #                     xtext_colors = rep(LETTERS[1:2], length.out = ncol(df)))
  # vdiffr::expect_doppelganger("plot_heatmap10", plt)
  expect_error(
    plot_heatmap(df,
                 ytext_colors = rep(letters[1:2], length.out = nrow(df)),
                 xtext_colors = rep(LETTERS[1:2], length.out = ncol(df)))
  )
  plt <- plot_heatmap(df, ytext_colors = 1:10)
  vdiffr::expect_doppelganger("plot_heatmap11", plt)
  # plt <- plot_heatmap(df, ytext_colors = 1:10, xtext_colors = 1:10)
  # vdiffr::expect_doppelganger("plot_heatmap12", plt)
  expect_error(
    plot_heatmap(df,
                 ytext_colors = 1:10,
                 xtext_colors = rep(LETTERS[1:2], length.out = ncol(df)))
  )
  expect_error(
    plot_heatmap(df,
                 y_groups = rep(letters[1:2], length.out = nrow(df)),
                 ytext_colors = rep(letters[1:2], length.out = nrow(df)))
  )
  expect_error(
    plot_heatmap(df,
                 y_groups = rep(letters[1:2], length.out = nrow(df)),
                 xtext_colors = rep(letters[1:2], length.out = ncol(df)))
  )
  expect_error(
    plot_heatmap(df,
                 x_groups = rep(letters[1:2], length.out = ncol(df)),
                 ytext_colors = rep(letters[1:2], length.out = nrow(df)))
  )
  expect_error(
    plot_heatmap(df,
                 x_groups = rep(letters[1:2], length.out = ncol(df)),
                 xtext_colors = rep(letters[1:2], length.out = ncol(df)))
  )

  plt <- plot_heatmap(iris %>% dplyr::select(-Species), y_groups = iris$Species)
  vdiffr::expect_doppelganger("plot_heatmap13", plt)
  plt <- plot_heatmap(iris %>% dplyr::select(-Species),
                      ytext_colors = iris$Species)
  vdiffr::expect_doppelganger("plot_heatmap14", plt)
  plt <- plot_heatmap(iris %>% dplyr::select(-Species),
                      y_orient = "ordered",
                      ytext_colors = iris$Species)
  vdiffr::expect_doppelganger("plot_heatmap15", plt)
})

test_that("plot_hclust_heatmap works properly", {
  data(iris)
  df <- as.data.frame(matrix(1:100, nrow = 10, byrow = 10))

  # check basic usage of plot_hclust_heatmap
  plt <- plot_hclust_heatmap(df)
  vdiffr::expect_doppelganger("plot_hclust_heatmap1", plt)
  plt <- plot_hclust_heatmap(df, clust_x = FALSE, clust_y = FALSE)
  vdiffr::expect_doppelganger("plot_heatmap1", plt)

  # check *_groups usage of plot_hclust_heatmap
  plt <- plot_hclust_heatmap(
    df,
    y_groups = rep(letters[1:2], length.out = nrow(df))
  )
  vdiffr::expect_doppelganger("plot_hclust_heatmap2", plt)
  plt <- plot_hclust_heatmap(
    df,
    x_groups = rep(LETTERS[1:2], length.out = ncol(df))
  )
  vdiffr::expect_doppelganger("plot_hclust_heatmap3", plt)
  plt <- plot_hclust_heatmap(
    df,
    y_groups = rep(letters[1:2], length.out = nrow(df)),
    x_groups = rep(LETTERS[1:2], length.out = ncol(df))
  )
  vdiffr::expect_doppelganger("plot_hclust_heatmap4", plt)

  # check axis text coloring usage of plot_hclust_heatmap
  plt <- plot_hclust_heatmap(
    df,
    ytext_colors = rep(letters[1:3], length.out = nrow(df))
  )
  vdiffr::expect_doppelganger("plot_hclust_heatmap5", plt)
  plt <- plot_hclust_heatmap(
    df,
    xtext_colors = rep(LETTERS[1:3], length.out = ncol(df))
  )
  vdiffr::expect_doppelganger("plot_hclust_heatmap6", plt)

  # check renaming of axis text labels
  plt <- plot_hclust_heatmap(df,
                             ytext_labels = letters[1:nrow(df)],
                             xtext_labels = 1:ncol(df))
  vdiffr::expect_doppelganger("plot_hclust_heatmap7", plt)

  # check on iris data set
  plt <- plot_hclust_heatmap(
    iris %>% dplyr::select(-Species),
    clust_x = FALSE, clust_y = TRUE,
    y_groups = iris$Species
  )
  vdiffr::expect_doppelganger("plot_hclust_heatmap8", plt)
  plt <- plot_hclust_heatmap(
    iris %>% dplyr::select(-Species),
    clust_x = FALSE, clust_y = TRUE,
    ytext_colors = iris$Species
  )
  vdiffr::expect_doppelganger("plot_hclust_heatmap9", plt)
})

test_that("plot_cor_heatmap works properly", {
  data(iris)
  X <- iris %>% dplyr::select(-Species)

  # check basic usage of plot_cor_heatmap
  plt <- plot_cor_heatmap(X)
  vdiffr::expect_doppelganger("plot_cor_heatmap1", plt)
  plt <- plot_cor_heatmap(X, text_size = 4)
  vdiffr::expect_doppelganger("plot_cor_heatmap2", plt)
  plt <- plot_cor_heatmap(X, cor_type = "spearman", text_size = 4, clust = F)
  vdiffr::expect_doppelganger("plot_cor_heatmap3", plt)

  # check renaming of axis text labels
  plt <- plot_cor_heatmap(X, xytext_labels = 1:4)
  vdiffr::expect_doppelganger("plot_cor_heatmap4", plt)

  # check axis text coloring usage of plot_cor_heatmap
  plt <- plot_cor_heatmap(
    X, xytext_colors = rep(letters[1:3], length.out = ncol(X))
  )
  vdiffr::expect_doppelganger("plot_cor_heatmap5", plt)
})

