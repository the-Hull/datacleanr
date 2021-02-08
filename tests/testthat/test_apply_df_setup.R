test_that("Grouped df is returned when Group supplied", {
  testdf <- apply_data_set_up(iris, "Species")
  expect_true(rlang::inherits_any(testdf, "grouped_df"))
})

test_that("df is returned as-is w/o Group", {
  testdf <- apply_data_set_up(iris, NULL)
  expect_true(!rlang::inherits_any(testdf, "grouped_df"))
})



test_that("setup fails if group isn't character", {
  expect_error(apply_data_set_up(iris, 5))
})
