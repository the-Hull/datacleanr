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


test_that("calculated limits are equal to manual approach on time series df)", {


    tsdf <- data.frame(x = seq(as.POSIXct("2000-01-01", tz = "UTC"),
                       as.POSIXct("2000-02-01", tz = "UTC"),
                       length.out = 25),
               y = rnorm(25)) %>%
        dplyr::mutate(group_index = dplyr::cur_group_id())


    x <- "x"
    y <- "y"




    xrange <- range(tsdf[ , x])
    yrange <- range(tsdf[ , y])


    total_range <-  as.numeric(range(tsdf[ , x, drop = TRUE], na.rm = TRUE))
    # offset <- (diff(total_range)) * c(-1,1) + scaling
    offset <- (diff(total_range) * 0.01) * c(-1,1)


    lim <- range(tsdf[ , x, drop = TRUE], na.rm = TRUE) +
        offset

    compare_list <- list(xlim = lim,
                         ylim = yrange)






    lim_func <- calc_limits_per_groups(dframe = tsdf ,
                                       group_index = 1,
                                       xvar = x,
                                       yvar = y,
                                       scaling = 0)
    expect_identical(lim_func, compare_list)



})



test_that("calculated limits are equal to manual approach on time series df)", {


    tsdf <- data.frame(y = seq(as.POSIXct("2000-01-01", tz = "UTC"),
                               as.POSIXct("2000-02-01", tz = "UTC"),
                               length.out = 25),
                       x = rnorm(25)) %>%
        dplyr::mutate(group_index = dplyr::cur_group_id())


    x <- "x"
    y <- "y"




    xrange <- range(tsdf[ , x])
    yrange <- range(tsdf[ , y])


    total_range <-  as.numeric(range(tsdf[ , y, drop = TRUE], na.rm = TRUE))
    # offset <- (diff(total_range)) * c(-1,1) + scaling
    offset <- (diff(total_range) * 0.01) * c(-1,1)


    lim <- range(tsdf[ , y, drop = TRUE], na.rm = TRUE) +
        offset

    compare_list <- list(xlim = xrange,
                         ylim = lim)






    lim_func <- calc_limits_per_groups(dframe = tsdf ,
                                       group_index = 1,
                                       xvar = x,
                                       yvar = y,
                                       scaling = 0)
    expect_identical(lim_func, compare_list)



})
