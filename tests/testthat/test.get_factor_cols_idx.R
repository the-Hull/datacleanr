test_that("Only factor columns are returned", {
  expect_equal(c(FALSE, FALSE, FALSE, FALSE, TRUE),
               get_factor_cols_idx(iris))
})
