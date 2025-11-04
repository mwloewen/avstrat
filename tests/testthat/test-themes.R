test_that("theme_avstrat returns a theme object", {
  th <- theme_avstrat()
  expect_s3_class(th, "theme")
})

test_that("theme_avstrat can be added to a ggstrat", {
  p <- ggstrat(df = example_data_strat, section_name = "Fail1") +
    theme_avstrat()
  expect_s3_class(p, "ggplot")
})
