test_that("score_swan works", {


  required_test_cols <- c('swan1','swan2','swan3','swan4','swan5','swan6','swan7','swan8','swan9',
                          'swan10','swan11','swan12','swan13','swan14','swan15','swan16','swan17','swan18')
  required_dem_cols <- c('age','gender','p_respondent')
  required_cols <- c(required_dem_cols, required_test_cols)

  df <- validate_data |>
    select(all_of(required_cols)) |>
    mutate(across(all_of(required_test_cols),
                  ~.x*-1)) |>
    score_swan() |>
    select(-contains('_reversed')) |>
    select(contains('_pro'), contains('_tot'), contains('_miss'), contains('tscore'))

  compare <- validate_data |>
    rename(swan_tot_gender_tscores = swan_gender_study_tscores,
           swan_tot_tscores = swan_study_tscores,
           swan_hi_gender_tscores = swan_hi_gender_study_tscores,
           swan_ia_gender_tscores = swan_ia_gender_study_tscores,
           swan_hi_tscores = swan_hi_study_tscores,
           swan_ia_tscores = swan_ia_study_tscores) |>
    select(colnames(df))

  expect_equal(df, compare)
})
