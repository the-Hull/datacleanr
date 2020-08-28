test_that("Initial dcr checks recognize wrong class", {


  testdf <- iris
  class(testdf) <- "something"

  expect_error(dcr_checks(testdf))

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

