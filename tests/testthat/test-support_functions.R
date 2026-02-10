test_that("clean_file looks for spreadsheets ", {
  file <- file.path("dir","testing.pdf")
  expect_error(clean_file(file))
})
