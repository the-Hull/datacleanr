test_that("calculated limits are equal to manual approach (grouped iris)", {


    x <- "Sepal.Length"
    y <- "Sepal.Width"


    testdf <- dplyr::group_by(iris, Species) %>%
        dplyr::mutate(group_index = dplyr::cur_group_id()) %>%
        dplyr::filter(group_index %in% c(1,3))

    xrange <- range(testdf[ , x])
    yrange <- range(testdf[ , y])

    compare_list <- list(xlim = xrange,
                         ylim = yrange)



    lim_func <- calc_limits_per_groups(dframe = dplyr::group_by(iris, Species),
                                       group_index = c(1,3),
                                       xvar = x,
                                       yvar = y,
                                       scaling = 0)


    expect_identical(lim_func, compare_list)


})

test_that("calculated limits are equal to manual approach (ungrouped iris)", {


    x <- "Sepal.Length"
    y <- "Sepal.Width"


    testdf <- iris %>%
        dplyr::mutate(group_index = dplyr::cur_group_id())

    xrange <- range(testdf[ , x])
    yrange <- range(testdf[ , y])

    compare_list <- list(xlim = xrange,
                         ylim = yrange)



    lim_func <- calc_limits_per_groups(dframe = iris,
                                       group_index = c(1),
                                       xvar = x,
                                       yvar = y,
                                       scaling = 0)
    expect_identical(lim_func, compare_list)



})



