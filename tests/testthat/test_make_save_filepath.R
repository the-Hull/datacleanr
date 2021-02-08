test_that("Build correct path based on save dir, file name and suffix", {
  outpath <- make_save_filepath(
    save_dir = ".",
    input_filepath = "subdir/subsubdir/test.R",
    suffix = "outfile",
    "Rds"
  )

  expect_equivalent(outpath, "./test_outfile.Rds")
})
