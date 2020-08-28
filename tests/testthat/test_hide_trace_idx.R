test_that("correct idx are returned for JS idx notation", {


    # assume df with 5 groups,
    # group 2 selected would imply removing index "1" (JS starts at 0)
    out <- hide_trace_idx(5, 2)

    expect_identical(out, c(0,2,3,4))


})



test_that("NULL returned for no group selection", {


    # assume df with 5 groups,
    # group 2 selected would imply removing index "1" (JS starts at 0)
    out <- hide_trace_idx(5, NULL)


    expect_null(out)


})


test_that("NULL returned when  all groups selected", {


    # assume df with 5 groups,
    # group 2 selected would imply removing index "1" (JS starts at 0)
    out <- hide_trace_idx(5, 1:5)


    expect_null(out)


})


