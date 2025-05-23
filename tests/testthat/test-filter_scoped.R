test_that("filter works with correct statement", {
  out <- filter_scoped(iris, "Species == 'setosa'", scope_at = NULL)

  expect_equivalent(
    out$filtered_df,
    subset(iris, Species == "setosa")
  )
})



test_that("filter fails with incorrect statement", {


  # checks <- filter_scoped(iris,
  #                  "Spec == 'setosa'",
  #                  scope_at = NULL)$succeeded


  # expect_equivalent(checks,
  #                  c(FALSE))
  #
  expect_error(filter_scoped(iris,
    "Spec == 'setosa'",
    scope_at = NULL
  ))
})



test_that("grouped filtering gives same result as dplyr::filter on grouped df", {
  fdf <- filter_scoped(dplyr::group_by(iris, Species) %>%
    dplyr::mutate(.dcrindex = dplyr::cur_group_id()),
  "Sepal.Width > quantile(Sepal.Width, 0.2)",
  scope_at = c(1, 2, 3)
  )$filtered_df

  dplyr_fdf <- dplyr::group_by(iris, Species) %>%
    dplyr::filter(Sepal.Width > quantile(Sepal.Width, 0.2))



  expect_equivalent(fdf[, 1:5], dplyr_fdf)
})


test_that("scoped filtering gives same result as split-combine", {
  fdf <- filter_scoped(dplyr::group_by(iris, Species) %>%
    dplyr::mutate(.dcrindex = dplyr::cur_group_id()),
  "Sepal.Width > quantile(Sepal.Width, 0.2)",
  scope_at = c(2, 3)
  )$filtered_df

  dplyr_fdf <- dplyr::group_by(iris, Species) %>%
    dplyr::mutate(.dcrindex = dplyr::cur_group_id()) %>%
    split(.,
      f = .$.dcrindex
    ) %>%
    purrr::map_at(
      .at = c(2, 3),
      function(x) {
        dplyr::filter(x, Sepal.Width > quantile(Sepal.Width, 0.2))
      }
    ) %>%
    dplyr::bind_rows()



  expect_equivalent(fdf, dplyr_fdf)
})



test_that("recursive filter gives same result as manual, repeated filter (ungrouped)", {
  cdf <- dplyr::tibble(
    statement = c(
      "Sepal.Width > quantile(Sepal.Width, 0.1)",
      "Petal.Width > quantile(Petal.Width, 0.1)",
      "Petal.Length > quantile(Petal.Length, 0.1)"
    ),
    scope_at = list(NULL, NULL, NULL)
  )

  fdf <- filter_scoped_df(iris, condition_df = cdf)

  dplyr_fdf <- dplyr::filter(iris, Sepal.Width > quantile(Sepal.Width, 0.1))
  dplyr_fdf <- dplyr::filter(dplyr_fdf, Petal.Width > quantile(Petal.Width, 0.1))
  dplyr_fdf <- dplyr::filter(dplyr_fdf, Petal.Length > quantile(Petal.Length, 0.1))
  expect_equivalent(fdf, dplyr_fdf)
})


test_that("recursive filter gives same result as manual, repeated filter (grouped)", {
  cdf <- dplyr::tibble(
    statement = c(
      "Sepal.Width > quantile(Sepal.Width, 0.1)",
      "Petal.Width > quantile(Petal.Width, 0.1)",
      "Petal.Length > quantile(Petal.Length, 0.8)"
    ),
    scope_at = list(NULL, NULL, c(1, 2))
  )

  fdf <- filter_scoped_df(dplyr::group_by(iris, Species) %>%
    dplyr::mutate(.dcrindex = dplyr::cur_group_id()),
  condition_df = cdf
  )

  dplyr_fdf <- dplyr::filter(iris, Sepal.Width > quantile(Sepal.Width, 0.1))
  dplyr_fdf <- dplyr::filter(dplyr_fdf, Petal.Width > quantile(Petal.Width, 0.1))


  dplyr_fdf <- purrr::map_at(split_groups(dplyr::group_by(dplyr_fdf, Species) %>%
    dplyr::mutate(.dcrindex = dplyr::cur_group_id())),
  .at = c(1, 2),
  .f = ~ dplyr::filter(.x, Petal.Length > quantile(Petal.Length, 0.8))
  ) %>%
    dplyr::bind_rows()

  expect_equivalent(fdf, dplyr_fdf)
})




test_that("recursive filter gives same result as manual, repeated filter (grouped) w/O .dcrindex column", {
  cdf <- dplyr::tibble(
    statement = c(
      "Sepal.Width > quantile(Sepal.Width, 0.1)",
      "Petal.Width > quantile(Petal.Width, 0.1)",
      "Petal.Length > quantile(Petal.Length, 0.8)"
    ),
    scope_at = list(NULL, NULL, c(1, 2))
  )

  fdf <- filter_scoped_df(dplyr::group_by(iris, Species),
    condition_df = cdf
  )

  dplyr_fdf <- dplyr::filter(iris, Sepal.Width > quantile(Sepal.Width, 0.1))
  dplyr_fdf <- dplyr::filter(dplyr_fdf, Petal.Width > quantile(Petal.Width, 0.1))


  dplyr_fdf <- purrr::map_at(split_groups(dplyr::group_by(dplyr_fdf, Species)),
    .at = c(1, 2),
    .f = ~ dplyr::filter(.x, Petal.Length > quantile(Petal.Length, 0.8))
  ) %>%
    dplyr::bind_rows()

  expect_equivalent(fdf, dplyr_fdf)
})


test_that("non df/tibble/data.table throws error", {
  cdf <- dplyr::tibble(
    statement = c(
      "Sepal.Width > quantile(Sepal.Width, 0.1)",
      "Petal.Width > quantile(Petal.Width, 0.1)",
      "Petal.Length > quantile(Petal.Length, 0.8)"
    ),
    scope_at = list(NULL, NULL, c(1, 2))
  )

  expect_error(filter_scoped_df(
    dframe = character(1),
    condition_df = cdf
  ),
  regexp = "Please provide a data.frame or tibble as dframe"
  )
})



test_that("non tibble list-column for condition_df throws error", {
  cdf <- dplyr::tibble(
    statement = c(
      "Sepal.Width > quantile(Sepal.Width, 0.1)"
    ),
    scope_at = "1"
  )

  expect_error(filter_scoped_df(
    dframe = dplyr::group_by(iris, Species),
    condition_df = cdf
  ),
  regexp = "Please provide a list column or NULL in condition_df[ ,2]",
  fixed = TRUE
  )
})


test_that("non tibble  for condition_df throws error", {
  cdf <- data.frame(
    statement = c(
      "Sepal.Width > quantile(Sepal.Width, 0.1)"
    ),
    scope_at = "1"
  )

  expect_error(filter_scoped_df(
    dframe = dplyr::group_by(iris, Species),
    condition_df = cdf
  ),
  regexp = "Please provide a tibble as condition_df",
  fixed = TRUE
  )
})


test_that("zero-row tibble for condition_df returns dframe", {
  cdf <- dplyr::tibble(
    statement = c(
      "Sepal.Width > quantile(Sepal.Width, 0.1)"
    ),
    scope_at = list("1")
  )[0, ]

  expect_equal(
    filter_scoped_df(
      dframe = dplyr::group_by(iris, Species),
      condition_df = cdf
    ),
    dplyr::group_by(iris, Species)
  )
})
