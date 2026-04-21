test_that("clean_file finds missing columns", {
  file <- system.file("extdata", "sample_swan.csv", package = "sfsScorer")
  df <- rio::import(file) |>
    select(-swan1)
  expect_error(clean_file(df, test = 'swan'))
})

#### Check File - ignore_check = F ####
test_that("clean_file finds impossible values", {
  file <- system.file("extdata", "sample_swan.csv", package = "sfsScorer")
  df <- rio::import(file) |>
    mutate(swan1 = 10)
  expect_error(clean_file(df, test = 'swan', ignore_check = F))
})

test_that("clean_file fixes p_respondent", {
  file <- system.file("extdata", "sample_swan.csv", package = "sfsScorer")
  df <- rio::import(file) |>
    dplyr::mutate(p_respondent = 5)
  expect_error(clean_file(df, test = 'swan', ignore_check = F))
})

test_that("clean_file fixes age class", {
  file <- system.file("extdata", "sample_swan.csv", package = "sfsScorer")
  df <- rio::import(file) |>
    mutate(row = row_number()) |>
    mutate(age = as.character(age)) |>
    dplyr::mutate(age = case_when(row == 2 ~ 'row',
                                  T ~ age)) |>
    select(-row)
  expect_error(clean_file(df, test = 'swan', ignore_check = F))
})

test_that("clean_file fixes age above 19 or below 5", {
  file <- system.file("extdata", "sample_swan.csv", package = "sfsScorer")
  df <- rio::import(file) |>
    mutate(row = row_number()) |>
    dplyr::mutate(age = case_when(row == 2 ~ 20,
                                  row == 4 ~ 2,
                                  T ~ age)) |>
    select(-row)
  expect_error(clean_file(df, test = 'swan', ignore_check = F))
})

test_that("clean_file checks gender", {
  file <- system.file("extdata", "sample_swan.csv", package = "sfsScorer")
  df <- rio::import(file) |>
    mutate(gender = NA)
  expect_warning(clean_file(df, test = 'swan', required_test_cols = paste0('swan',seq(1,18))))
})

#### Check File - ignore_check = T ####
test_that("clean_file finds impossible values ignore check", {
  file <- system.file("extdata", "sample_swan.csv", package = "sfsScorer")
  df <- rio::import(file) |>
    mutate(swan1 = 10)
  expect <- df |>
    mutate(swan1 = case_when(swan1 == 10 ~ NA,
                             T ~ swan1))
  expect_equal(clean_file(df, test = 'swan', ignore_check = T, required_test_cols = paste0('swan',seq(1,18))), expect)
})

test_that("clean_file fixes p_respondent ignore check", {
  file <- system.file("extdata", "sample_swan.csv", package = "sfsScorer")
  df <- rio::import(file) |>
    dplyr::mutate(p_respondent = 5)
  expect <- df |>
    mutate(p_respondent = case_when(p_respondent %in% c(1,0) ~ p_respondent,
                             T ~ NA))
  expect_equal(clean_file(df, test = 'swan', ignore_check = T, required_test_cols = paste0('swan',seq(1,18))), expect)
})

test_that("clean_file fixes age class ignore check", {
  file <- system.file("extdata", "sample_swan.csv", package = "sfsScorer")
  df <- rio::import(file) |>
    mutate(row = row_number()) |>
    mutate(age = as.character(age)) |>
    dplyr::mutate(age = case_when(row == 2 ~ 'row',
                                  T ~ age)) |>
    select(-row)
  expect <- df |>
    mutate(age = case_when(age == 'row' ~ NA,
                           T ~ age)) |>
    mutate(age = as.numeric(age))
  expect_equal(clean_file(df, test = 'swan', ignore_check = T, required_test_cols = paste0('swan',seq(1,18))), expect)
})

test_that("clean_file deals with invalid ages", {
  file <- system.file("extdata", "sample_swan.csv", package = "sfsScorer")
  df <- rio::import(file) |>
    mutate(row = row_number()) |>
    dplyr::mutate(age = case_when(row == 2 ~ 21,
                                  row == 3 ~ 2,
                                  T ~ age)) |>
    select(-row)
  expect <- df |>
    mutate(age = case_when(age >= 19 | age < 5 ~ NA,
                           T ~ age)) |>
    mutate(age = as.numeric(age))
  expect_equal(clean_file(df, test = 'swan', ignore_check = T, required_test_cols = paste0('swan',seq(1,18))), expect)
})

#### Make Variables ####

test_that("mkpro works properly", {
  df <- data.frame(swan1 = c(1, 2, 0, NA),
                   swan2 = c(-2, NA, 1, NA),
                   swan3 = c(1, 3, -1, 2),
                   swan4 = c(1, 2, 0, 1))
  check <- data.frame(mkpro(maxmiss = 1, dat = df, required_test_cols = paste0('swan',seq(1,4))
                            ))
  expect <- data.frame(swan_tot = c(1, 7, 0, NA),
                       swan_miss = c(0, 1, 0, 2),
                       swan_pro = c(1, 9 + 1/3, 0, NA))
  expect_equal(check, expect)
})
