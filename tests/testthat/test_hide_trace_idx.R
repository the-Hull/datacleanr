test_that("correct idx are returned for JS idx notation", {
  tmap <- as.matrix(cbind(c(1:5), c(0:4)))

  # assume df with 5 groups,

  #### NOTE: previous - seems that group idx changed to start from 1
  #### group 2 selected would imply removing index "1" (JS starts at 0)

  #### CURRENT:
  # group 2 selected would imply removing index "2"
  out <- hide_trace_idx(tmap, 5, 2)

  # expect_identical(out, c(0,2,3,4))
  expect_identical(out, as.integer(c(0, 2, 3, 4)))
})



test_that("NULL returned for no group selection", {
  tmap <- as.matrix(cbind(c(1:5), c(0:4)))

  # assume df with 5 groups,
  out <- hide_trace_idx(tmap, 5, NULL)


  expect_null(out)
})


test_that("NULL returned when  all groups selected", {
  tmap <- as.matrix(cbind(c(1:5), c(0:4)))

  # assume df with 5 groups,
  out <- hide_trace_idx(tmap, 5, 1:5)


  expect_null(out)
})
