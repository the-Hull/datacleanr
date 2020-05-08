test_that("filter works with correct statement", {


    out <- checked_filter(iris, "Species == 'setosa'")

  expect_identical(out$filtered_df,
                   subset(iris, Species == 'setosa'))
})


test_that("filter identifies incorrect statement", {


    checks <- checked_filter(iris,
                   c("Species == 'setosa'", # should be true
                     "Species == 'wrong'", # should be true
                     "Spec == 'setosa'"))$succeeded


  expect_equivalent(checks,
                   c(TRUE, TRUE, FALSE))
})


