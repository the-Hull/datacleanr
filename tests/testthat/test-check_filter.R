test_that("filter works with correct statement", {


    out <- checked_filter(iris, "Species == 'setosa'", apply_grouped = FALSE)

  expect_identical(out$filtered_df,
                   subset(iris, Species == 'setosa'))
})


test_that("filter identifies incorrect statement", {


    checks <- checked_filter(iris,
                   c("Species == 'setosa'", # should be true
                     "Species == 'wrong'", # should be true
                     "Spec == 'setosa'"),
                   apply_grouped = c(FALSE,
                                     FALSE,
                                     FALSE))$succeeded


    expect_equivalent(checks,
                      c(TRUE, TRUE, FALSE))
})



test_that("grouped filtering gives same result as dplyr::filter on grouped df", {




  fdf <- checked_filter(dplyr::group_by(iris, Species),
                        "Sepal.Width > quantile(Sepal.Width, 0.2)",
                        apply_grouped = TRUE)$filtered_df

  dplyr_fdf <- dplyr::group_by(iris, Species) %>%
    dplyr::filter(Sepal.Width > quantile(Sepal.Width, 0.2))



  expect_equivalent(fdf,dplyr_fdf)
})


