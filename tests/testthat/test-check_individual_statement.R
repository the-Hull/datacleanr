test_that("correct statement identified", {



  check <- check_individual_statement(iris, "Species == 'setosa'")




  expect_identical(check, TRUE)
})


test_that("filter identifies incorrect statement", {


  checks <- sapply( c("Species == 'setosa'", # should be true
                      "Species == 'wrong'", # should be true
                      "Spec == 'setosa'"),

                    function(x)
                      check_individual_statement(df = iris,
                                                 statement = x))


  expect_equivalent(checks,
                    c(TRUE, TRUE, FALSE))
})




