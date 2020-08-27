test_that("filter works with correct statement", {


    out <- filter_scoped(iris, "Species == 'setosa'", scope_at =  NULL)

  expect_equivalent(out$filtered_df,
                   subset(iris, Species == 'setosa'))
})


test_that("filter identifies incorrect statement", {


    checks <- filter_scoped(iris,
                     "Spec == 'setosa'",
                     scope_at = NULL)$succeeded


  expect_equivalent(checks,
                   c(FALSE))
})



test_that("grouped filtering gives same result as dplyr::filter on grouped df", {




    fdf <- filter_scoped(dplyr::group_by(iris, Species),
                             "Sepal.Width > quantile(Sepal.Width, 0.2)",
                         scope_at = c(1,2,3))$filtered_df

    dplyr_fdf <- dplyr::group_by(iris, Species) %>%
        dplyr::filter(Sepal.Width > quantile(Sepal.Width, 0.2))



    expect_equivalent(fdf,dplyr_fdf)
})


test_that("scoped filtering gives same result as split-combine", {




  fdf <- filter_scoped(dplyr::group_by(iris, Species),
                       "Sepal.Width > quantile(Sepal.Width, 0.2)",
                       scope_at = c(2,3))$filtered_df

  dplyr_fdf <- dplyr::group_by(iris, Species) %>%
    split(.,
          dplyr::group_indices(.)) %>%
    purrr::map_at(.at = c(2,3),
          function(x){
    dplyr::filter(x, Sepal.Width > quantile(Sepal.Width, 0.2))
    }) %>%
    dplyr::bind_rows()



  expect_equivalent(fdf,dplyr_fdf)
})



test_that("recursive filter gives same result as manual, repeated filter (ungrouped)", {

  cdf <- dplyr::tibble(
    statement = c(
      "Sepal.Width > quantile(Sepal.Width, 0.1)",
      "Petal.Width > quantile(Petal.Width, 0.1)",
      "Petal.Length > quantile(Petal.Length, 0.1)"),
    scope_at = list(NULL, NULL, NULL))

    fdf <- filter_scoped_df(iris, condition_df = cdf)

    dplyr_fdf <- dplyr::filter(iris, Sepal.Width > quantile(Sepal.Width, 0.1))
    dplyr_fdf <- dplyr::filter(dplyr_fdf, Petal.Width > quantile(Petal.Width, 0.1))
    dplyr_fdf <- dplyr::filter(dplyr_fdf, Petal.Length > quantile(Petal.Length, 0.1))

    expect_equivalent(fdf,dplyr_fdf)



})


test_that("recursive filter gives same result as manual, repeated filter (grouped)", {

  cdf <- dplyr::tibble(
    statement = c(
      "Sepal.Width > quantile(Sepal.Width, 0.1)",
      "Petal.Width > quantile(Petal.Width, 0.1)",
      "Petal.Length > quantile(Petal.Length, 0.8)"),
    scope_at = list(NULL, NULL, c(1,2)))

  fdf <- filter_scoped_df(dplyr::group_by(iris, Species), condition_df = cdf)

  dplyr_fdf <- dplyr::filter(iris, Sepal.Width > quantile(Sepal.Width, 0.1))
  dplyr_fdf <- dplyr::filter(dplyr_fdf, Petal.Width > quantile(Petal.Width, 0.1))


  dplyr_fdf <- purrr::map_at(split_groups(dplyr::group_by(dplyr_fdf, Species)),
                             .at = c(1,2),
                             .f = ~dplyr::filter(.x, Petal.Length > quantile(Petal.Length, 0.8))) %>%
    dplyr::bind_rows()


  expect_equivalent(fdf,dplyr_fdf)



})


