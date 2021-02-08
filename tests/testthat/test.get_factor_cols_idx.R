test_that("Only factor columns are returned", {
  expect_equal(
    c(FALSE, FALSE, FALSE, FALSE, TRUE),
    get_factor_cols_idx(iris)
  )
})


test_that("Throws error with ncol(df)<1", {
  expect_error(get_factor_cols_idx(iris[, 0]),
    "Error: supply data set with multiple columns (data.frame, tbl, data.table)!",
    fixed = TRUE
  )
})
