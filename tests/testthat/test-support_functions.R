test_that("clean_file finds missing columns", {
  file <- system.file("extdata", "sample_swan.csv", package = "sfsScorer")
  df <- rio::import(file) |>
    select(-swan1)
  expect_error(clean_file(df, test = 'swan'))
})


test_that("clean_file finds impossible values", {
  file <- system.file("extdata", "sample_swan.csv", package = "sfsScorer")
  df <- rio::import(file) |>
    mutate(swan1 = 10)
  expect_error(clean_file(df, test = 'swan'))
})

test_that("mkvars works properly", {
  vars <- mkvars(1, 18, root = 'swan')
  expect_equal(vars, paste0('swan',seq(1, 18, by = 1)))
})

test_that("mkpro works properly", {
  df <- data.frame(swan1 = c(1, 2, 0, NA),
                   swan2 = c(-2, NA, 1, NA),
                   swan3 = c(1, 3, -1, 2),
                   swan4 = c(1, 2, 0, 1))
  check <- data.frame(mkpro(maxmiss = 1, dat = df, a = 1, b = 4))
  expect <- data.frame(swan_tot = c(1, 7, 0, NA),
                       swan_miss = c(0, 1, 0, 2),
                       swan_pro = c(1, 9 + 1/3, 0, NA))
  expect_equal(check, expect)
})
