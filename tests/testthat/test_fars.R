setwd(system.file("extdata", package = "courserafars"))

test_that("yearly data is summarized", {
  df <- fars_summarize_years(2013:2015)
  expect_type(df, "list")
  expect_equal(df$`2014`[11], 2714)
})

test_that("summarization of year without data fails", {
  expect_warning(
    fars_read_years(c(2021)),
    "invalid year: 2021"
  )
})
