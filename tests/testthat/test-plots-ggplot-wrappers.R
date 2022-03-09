test_that("plot_bar works properly", {
  df <- data.frame(x = rep(letters[1:3], 2),
                   y = rep(LETTERS[1:2], 3))
  df_identity <- data.frame(x = letters[1:3], y = c(4, 2, 1))

  # check basic usage of plot_bar
  plt <- plot_bar(data = df, x_str = "x")
  vdiffr::expect_doppelganger("plot_bar1", plt)
  plt <- plot_bar(df, x_str = "x", fill_str = "y")
  vdiffr::expect_doppelganger("plot_bar2", plt)

  # check stat = "identity" option
  plt <- plot_bar(data = df_identity, x_str = "x", y_str = "y",
                  stat = "identity")
  vdiffr::expect_doppelganger("plot_bar3", plt)
})

test_that("plot_boxplot works properly", {
  data(iris)

  # plot boxplot of all data in data frame
  plt <- plot_boxplot(as.data.frame(matrix(1:1000, nrow = 100)))
  vdiffr::expect_doppelganger("plot_boxplot1", plt)

  # plot boxplot of single column in data frame
  plt <- plot_boxplot(iris, y_str = "Sepal.Width")
  vdiffr::expect_doppelganger("plot_boxplot2", plt)
  plt <- plot_boxplot(iris, x_str = "Species", y_str = "Sepal.Width")
  vdiffr::expect_doppelganger("plot_boxplot3", plt)

  # check fill works properly for plot boxplot
  iris2 <- data.frame(iris,
                      z = as.factor(rep(letters[1:2],
                                    length.out = nrow(iris))))
  plt <- plot_boxplot(iris2, x_str = "Species", y_str = "Sepal.Width",
                      fill_str = "z")
  vdiffr::expect_doppelganger("plot_boxplot4", plt)
  plt <- plot_boxplot(iris2, y_str = "Sepal.Width", fill_str = "z")
  vdiffr::expect_doppelganger("plot_boxplot5", plt)
})

test_that("plot_density works properly", {
  data(iris)

  # plot distribution of all data in data frame
  plt <- plot_density(as.data.frame(matrix(1:1000, nrow = 100)))
  vdiffr::expect_doppelganger("plot_density1", plt)

  # plot distribution of a single column in data frame
  plt <- plot_density(iris, x_str = "Sepal.Width")
  vdiffr::expect_doppelganger("plot_density2", plt)
  plt <- plot_density(iris, x_str = "Sepal.Width", fill_str = "Species")
  vdiffr::expect_doppelganger("plot_density3", plt)
})

test_that("plot_histogram works properly", {
  data(iris)

  # plot distribution of all data in data frame
  plt <- plot_histogram(as.data.frame(matrix(1:1000, nrow = 100)))
  vdiffr::expect_doppelganger("plot_histogram1", plt)

  # plot distribution of a single column in data frame
  plt <- plot_histogram(iris, x_str = "Sepal.Width")
  vdiffr::expect_doppelganger("plot_histogram2", plt)
  plt <- plot_histogram(iris, x_str = "Sepal.Width", fill_str = "Species")
  vdiffr::expect_doppelganger("plot_histogram3", plt)
})

test_that("plot_line works properly", {
  df <- data.frame(time = 1:5, value = 5:9)

  expect_error(plot_line(df))

  # plot a single line
  plt <- plot_line(df, x_str = "time", y_str = "value")
  vdiffr::expect_doppelganger("plot_line1", plt)

  # plot multiple (grouped) lines
  df_grouped <- data.frame(time = rep(1:5, 2),
                           value = 1:10,
                           group = rep(letters[1:2], each = 5))
  plt <- plot_line(df_grouped, x_str = "time", y_str = "value",
                   color_str = "group")
  vdiffr::expect_doppelganger("plot_line2", plt)
})

test_that("plot_point works properly", {
  data(iris)

  expect_error(plot_point(data = iris))

  # basic usage of plot_point
  plt <- plot_point(iris, x_str = "Sepal.Width", y_str = "Sepal.Length")
  vdiffr::expect_doppelganger("plot_point1", plt)
  plt <- plot_point(iris, x_str = "Sepal.Width", y_str = "Sepal.Length",
                    color_str = "Species")
  vdiffr::expect_doppelganger("plot_point2", plt)
})
