test_that("split_data works properly", {
  data(iris)
  train_prop <- 0.6
  valid_prop <- 0.1
  test_prop <- 0.3

  # basic usage of split_data
  data_split <- split_data(X = iris %>% dplyr::select(-Species),
                           y = iris$Species,
                           train_prop = train_prop,
                           valid_prop = valid_prop,
                           test_prop = test_prop)
  expect_equal(names(data_split), c("X", "y"))
  expect_equal(sapply(data_split$X, nrow),
               c(train = train_prop, validate = valid_prop, test = test_prop) *
                 nrow(iris))
  expect_equal(sapply(data_split$y, length),
               c(train = train_prop, validate = valid_prop, test = test_prop) *
                 nrow(iris))

  # stratified: split iris data into training, validation, and test sets while
  # keeping `Species` distribution constant across partitions
  stratified_data_split <- split_data(X = iris %>% dplyr::select(-Species),
                                      y = iris$Species,
                                      stratified_by = iris$Species,
                                      train_prop = train_prop,
                                      valid_prop = valid_prop,
                                      test_prop = test_prop)
  expect_equal(names(stratified_data_split), c("X", "y"))
  expect_equal(sapply(stratified_data_split$X, nrow),
               c(train = train_prop, validate = valid_prop, test = test_prop) *
                 nrow(iris))
  expect_equal(sapply(stratified_data_split$y, length),
               c(train = train_prop, validate = valid_prop, test = test_prop) *
                 nrow(iris))
  for (y_class in unique(iris$Species)) {
    expect_equal(sapply(stratified_data_split$y, function(x) sum(x == y_class)),
                 c(train = train_prop, validate = valid_prop, test = test_prop) *
                   sum(iris$Species == y_class))
  }

  # test split_data when *_prop = 0
  train_prop <- 0.6
  valid_prop <- 0
  test_prop <- 0.4
  data_split <- split_data(X = iris %>% dplyr::select(-Species),
                           y = iris$Species,
                           train_prop = train_prop,
                           valid_prop = valid_prop,
                           test_prop = test_prop)
  expect_equal(names(data_split), c("X", "y"))
  expect_equal(sapply(data_split$X, nrow),
               c(train = train_prop, test = test_prop) * nrow(iris))
  expect_equal(sapply(data_split$y, length),
               c(train = train_prop, test = test_prop) * nrow(iris))
})

test_that("remove_cols family works properly", {
  data(iris)

  iris_extra <- iris %>%
    dplyr::mutate(na_col = NA,
                  na1_col = c(NA, rep(0, nrow(iris) - 1)),
                  dup_factor_col = Species,
                  dup_num_col = Sepal.Width,
                  na_dup_col = na_col,
                  const_col = 1)

  # basic usage of remove_na_cols
  out <- remove_na_cols(iris)
  expect_equal(out, iris)
  out <- remove_na_cols(as.matrix(iris))
  expect_equal(out, as.matrix(iris))
  out <- remove_na_cols(iris_extra)
  expect_equal(out,
               iris_extra %>% dplyr::select(-dplyr::starts_with("na")))
  out <- remove_na_cols(as.matrix(iris_extra))
  expect_equal(out,
               iris_extra %>%
                 dplyr::select(-dplyr::starts_with("na")) %>%
                 as.matrix())

  # basic usage of remove_duplicate_cols
  out <- remove_duplicate_cols(iris)
  expect_equal(out, iris)
  out <- remove_duplicate_cols(as.matrix(iris))
  expect_equal(out, as.matrix(iris))
  out <- remove_duplicate_cols(iris_extra)
  expect_equal(out,
               iris_extra %>% dplyr::select(-dplyr::contains("dup")))
  out <- remove_duplicate_cols(as.matrix(iris_extra))
  expect_equal(out,
               iris_extra %>%
                 dplyr::select(-dplyr::contains("dup")) %>%
                 as.matrix())

  # basic usage of remove_constant_cols
  out <- remove_constant_cols(iris)
  expect_equal(out, iris)
  out <- remove_constant_cols(as.matrix(iris))
  expect_equal(out, as.matrix(iris))
  out <- remove_constant_cols(iris_extra)
  expect_equal(out,
               iris_extra %>%
                 dplyr::select(-na_col, -na_dup_col, -const_col))
  out <- remove_constant_cols(as.matrix(iris_extra))
  expect_equal(out,
               iris_extra %>%
                 dplyr::select(-na_col, -na_dup_col, -const_col) %>%
                 as.matrix())

})

test_that("filter_cols family works properly", {
  X <- matrix(rnorm(5000), nrow = 100, ncol = 50)

  # basic usage of filter_cols_by_var
  out <- filter_cols_by_var(X)
  expect_equal(out, X)
  out <- filter_cols_by_var(X, min_var = 1)
  out2 <- filter_cols_by_var(X, min_var = 1, max_p = 50)
  expect_true(all(apply(out, 2, var) >= 1))
  expect_equal(out, out2)
  out <- filter_cols_by_var(X, max_p = 10)
  expect_equal(dim(out), c(100, 10))
  out <- filter_cols_by_var(X, min_var = 1, max_p = 10)
  expect_true(all(apply(out, 2, var) >= 1))
  expect_true(ncol(out) <= 10)
  expect_equal(nrow(out), 100)
})
