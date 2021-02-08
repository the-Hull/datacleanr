test_that("Expect that first values from RColorbrewer palette are returned", {
  expect_equal(
    extend_palette(1),
    RColorBrewer::brewer.pal(3, "Dark2")[1]
  )
  expect_equal(
    extend_palette(2),
    RColorBrewer::brewer.pal(3, "Dark2")[1:2]
  )
  expect_equal(
    extend_palette(3),
    RColorBrewer::brewer.pal(3, "Dark2")
  )
})
