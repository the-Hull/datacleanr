test_that("Initial dcr checks recognize wrong class", {

  testdf <- iris
  class(testdf) <- "something"

  expect_error(dcr_checks(testdf),
               regexp = "Please provide a data.frame, tibble, or data.table.",
               fixed = TRUE)

})


test_that("Initial dcr checks recognize non-existing file", {

    expect_error(dcr_checks("Abc.Rds"),
                 regexp = "File does not exist.",
                 fixed = TRUE)

})

test_that("Initial dcr checks recognize wrong file ext", {

    expect_error(dcr_checks("Abc.Rdd"),
                 regexp = "Please provide a *.Rds file.",
                 fixed = TRUE)

})

test_that("Initial dcr checks recognizes wrong class in rds from filepath", {

    tmpfile <- tempfile(fileext = ".Rds")

    testdf <- iris
    class(testdf) <- "something"
    saveRDS(testdf, file = tmpfile)

    expect_error(dcr_checks(tmpfile),
                 regexp = "Please provide a data.frame, tibble, or data.table in your file path.",
                 fixed = TRUE)
    unlink(tmpfile)

})

test_that("Initial dcr checks works on iris from filepath", {

    tmpfile <- tempfile(fileext = ".Rds")
    saveRDS(iris, file = tmpfile)

    expect_identical(dcr_checks(tmpfile),
                     list(dataset = iris,
                          file_path = tmpfile))
    unlink(tmpfile)

})


test_that("Initial dcr checks works on iris", {

    expect_identical(dcr_checks(iris),
                     list(dataset = iris,
                          file_path = NULL))
})


# test_that("User warned about large data set", {
#
#
#   testdf <- data.frame(x = 1:15001)
#
#   expect_warning(dcr_checks(testdf),
#                  regexp = "Data set has over")
# })
#

