test_that("TOCS summary function works", {
  sample_data <- data.frame(p_respondent = c(0),
                        age = c(18.4),
                        gender = c(2),
                        tocs1 = c(2),
                        tocs2 = c(2),
                        tocs3 = c(3),
                        tocs4 = c(1),
                        tocs5 = c(3),
                        tocs6 = c(1),
                        tocs7 = c(3),
                        tocs8 = c(0),
                        tocs9 = c(-2),
                        tocs10 = c(0),
                        tocs11 = c(1),
                        tocs12 = c(2),
                        tocs13 = c(0),
                        tocs14 = c(0),
                        tocs15 = c(0),
                        tocs16 = c(0),
                        tocs17 = c(-2),
                        tocs18 = c(1),
                        tocs19 = c(1),
                        tocs20 = c(-2),
                        tocs21 = c(-3),
                        tocs22 = c(-3),
                        tocs23 = c(2),
                        tocs24 = c(0))

  expect_equal(build_summary_tocs(sample_data, max_missing = 0), sample_data |> bind_cols(age18 = c(18),
                                                                                       female = c(1),
                                                                                       youth = c(1),
                                                                                       tocs_tot = c(10),
                                                                                       tocs_miss = c(0),
                                                                                       tocs_pro = c(10)))
})

test_that("TOCS model function works", {
  sample_data <- data.frame(p_respondent = c(0),
                            age = c(18.4),
                            gender = c(2),
                            tocs1 = c(2),
                            tocs2 = c(2),
                            tocs3 = c(3),
                            tocs4 = c(1),
                            tocs5 = c(3),
                            tocs6 = c(1),
                            tocs7 = c(3),
                            tocs8 = c(0),
                            tocs9 = c(-2),
                            tocs10 = c(0),
                            tocs11 = c(1),
                            tocs12 = c(2),
                            tocs13 = c(0),
                            tocs14 = c(0),
                            tocs15 = c(0),
                            tocs16 = c(0),
                            tocs17 = c(-2),
                            tocs18 = c(1),
                            tocs19 = c(1),
                            tocs20 = c(-2),
                            tocs21 = c(-3),
                            tocs22 = c(-3),
                            tocs23 = c(2),
                            tocs24 = c(0)) |>
    build_summary_tocs(max_missing = 0)

  expect_equal(run_model_tocs(sample_data), sample_data |> bind_cols(tocs_gender_tscores = c(50.67452),
                                                                     tocs_tscores = c(52.10126)))
})

test_that("score_tocs2 works", {

  required_test_cols <-  paste0('tocs',seq(1,24,1))
  required_dem_cols <- c('age','gender','p_respondent')
  required_cols <- c(required_dem_cols, required_test_cols)

  df <- validate_data |>
    select(all_of(required_cols)) |>
    score_tocs2() |>
    select(contains('_pro'), contains('_tot'), contains('_miss'), contains('tscore')) |>
    mutate(across(everything(),
                  ~round(.x, digits = 5)))

  compare <- validate_data |>
    rename(tocs_gender_tscores = tocs_gender_study_tscores,
           tocs_tscores = tocs_study_tscores) |>
    select(colnames(df)) |>
    mutate(across(everything(),
                  ~round(.x, digits = 5)))

  expect_equal(df, compare)
})

