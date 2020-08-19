test_that("table has 3 rows on grouped iris (species)", {


  rowcount_tbl <- nrow(make_group_table(dplyr::group_by(iris, Species)))

  rowcount_dplyr <- dplyr::n_groups(dplyr::group_by(iris, Species))

  expect_identical(rowcount_tbl, rowcount_dplyr)


})


