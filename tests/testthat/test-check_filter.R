test_that("filter works with correct statement", {


    out <- checked_filter(iris, "Species == 'setosa'")

  expect_identical(out$filtered_df,
                   subset(iris, Species == 'setosa'))
})


test_that("filter identifies incorrect statement", {


    # out <- sapply(c("Species == 'setosa'", # should be true
    #                 "Species == 'wrong'", # should be true
    #                 "Spec == 'setosa'"),
    #               function(x){
    #                   try({checked_filter(iris,x)})
    #               },
    #               USE.NAMES = FALSE)
    #
    # checks <- !vapply(out, is.error, logical(1))
    #

    checks <- checked_filter(iris,
                   c("Species == 'setosa'", # should be true
                     "Species == 'wrong'", # should be true
                     "Spec == 'setosa'"))$succeeded


  expect_equivalent(checks,
                   c(TRUE, TRUE, FALSE))
})


