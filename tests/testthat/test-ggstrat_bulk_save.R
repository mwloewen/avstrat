test_that("ggstrat_bulk_save creates plot files in a tempdir", {
  # create a temporary directory that will be cleaned up automatically
  tmp <- withr::local_tempdir()

  # run the bulk save with a simple plotting function
  ggstrat_bulk_save(
    example_data_strat,
    plotfunction = ggstrat,   # your real plotting function
    outdir = tmp,
    ask = FALSE               # skip interactive prompt
  )

  # check that at least one file was created
  files <- list.files(tmp, pattern = "\\.png$", full.names = TRUE)
  expect_true(length(files) > 0)

  # optional: check that the files are non-empty
  expect_true(all(file.info(files)$size > 0))
})
