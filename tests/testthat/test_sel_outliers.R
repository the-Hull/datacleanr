test_that("new points are added to selection", {

    test_old <- data.frame(keys = c(7,8,1,2,6),
                           selection_count = 1,
                           .annotation = "",
                           stringsAsFactors = FALSE)
    new <- data.frame(customdata = 9)

    expect_equal(NROW(handle_sel_outliers(test_old, new)), 6)


})


test_that("duplicates are removed from selection", {

    test_old <- data.frame(keys = c(7,8,1,2,6),
                           selection_count = 1,
                           .annotation = "",
                           stringsAsFactors = FALSE)

    new <- data.frame(customdata = c(7,8,1, 55))

    expect_equal(NROW(handle_sel_outliers(test_old, new)), 3)



})

test_that("empty df is returned for complete overlap", {

    test_old <- data.frame(keys = c(7,8,1,2,6),
                           selection_count = 1,
                           .annotation = "",
                           stringsAsFactors = FALSE)

    new <- data.frame(customdata = c(7,8,1,2,6))

    expect_equal(NROW(handle_sel_outliers(test_old, new)), 0)

})



test_that("selection count is reset to 1 if only single selection remains", {

    test_old <- data.frame(keys = c(7,8,1,2,6),
                           selection_count = 1,
                           .annotation = "",
                           stringsAsFactors = FALSE)

    new <- data.frame(customdata = c(7,8,1,2,6, 101))

    expect_equal(handle_sel_outliers(test_old, new)$selection_count, 1)


})
