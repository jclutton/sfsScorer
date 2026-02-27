# Score a TOCS-2 Questionnaire

The Toronto Obsessive-Compulsive Scale 2 (TOCS-2) is a validated
instrument for measuring Obsessive-Compulsive (OCD) traits. This article
shows you how to score your TOCS-2 tests.

## Quick Start

The code below shows how to score a TOCS-2 questionnaire using the
[`score_tocs2()`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/score_tocs2.md)
function.

``` r
library(sfsScorer)
#> sfsScorer has been loaded

#' Here's how we expect the data to be formatted
#' age (5-18)
#' gender (1 = boy, 2 = girl)
#' p_respondent (1 = parent / caregiver responded, 0 = child / youth respondent)
#' All tocs questions 1-24 as tocs1...tocs24
head(random_data, 1)
#>   age gender p_respondent tocs1 tocs2 tocs3 tocs4 tocs5 tocs6 tocs7 tocs8 tocs9
#> 1  14      2            1    -1     2    -2     0     3    -1     0     1     2
#>   tocs10 tocs11 tocs12 tocs13 tocs14 tocs15 tocs16 tocs17 tocs18 tocs19 tocs20
#> 1      2      0     -2      2     -1     -1      2      0     -2     -2      2
#>   tocs21 tocs22 tocs23 tocs24
#> 1      1     -2      3     -3

# Score from a data frame
scores <- score_tocs2(df = random_data)
#> ✔ The model scored 5 observations.

# OR
# Score from a a csv or xlsx file
tocs_csv <- system.file("extdata", "sample_tocs.csv", package = "sfsScorer")
scores_csv <- score_tocs2(file = tocs_csv)
#> ✔ The model scored 5 observations.

# Data are validated by default
 df_mod <- random_data |> 
   dplyr::mutate(tocs1 = 6)
 try(scores_csv <- score_tocs2(df = df_mod))
#> There are 5 impossible values in the file.
#> The only valid values are -3, -2, -1, 0, 1, 2, 3, and NA.
#> 
#> Error in clean_file(df, test = "tocs", ignore_check = ignore_check) : 
#>   Please correct or remove these rows - "Row 1: tocs1 - 6", "Row 2: tocs1
#> - 6", "Row 3: tocs1 - 6", "Row 4: tocs1 - 6", and "Row 5: tocs1 - 6"
 
 # To ignore validation and introduce NAs use `ignore_check = TRUE`. 
 # This can impact scoring
 scores_csv <- score_tocs2(df = df_mod, ignore_check = TRUE)
#> ! 5 impossible values were changed to NA. This could impact scores. 
#> The only valid values are -3, -2, -1, 0, 1, 2, 3, and NA. To correct, review the following rows before running - "Row 1: tocs1 - 6", "Row 2: tocs1 - 6", "Row 3: tocs1 - 6", "Row 4: tocs1 - 6", and "Row 5: tocs1 - 6"
#> ✔ The model scored 0 observations.
#> ! 5 observations were not scored due to excessive missingness. 0 questions are allowed to be missing.
 
 # Allow more missingness
 # This will use prorated scores
  scores_csv <- score_tocs2(df = df_mod, ignore_check = TRUE, max_missing = 1)
#> ! 5 impossible values were changed to NA. This could impact scores. 
#> The only valid values are -3, -2, -1, 0, 1, 2, 3, and NA. To correct, review the following rows before running - "Row 1: tocs1 - 6", "Row 2: tocs1 - 6", "Row 3: tocs1 - 6", "Row 4: tocs1 - 6", and "Row 5: tocs1 - 6"
#> ✔ The model scored 5 observations.
```

## Notes Before Starting

- T-scores will be generated based on gendered and non-gendered norms.
  Please feel free to include children who are trans or non-binary in
  your dataset and leave the codes for their gender as appropriate for
  the individuals in your study. Non-gendered t-scores will be generated
  for all individuals. Gendered t-scores will only be generated when
  gender is coded as 1 or 2. It is recommended to use the non-gendered
  t-scores for trans or non-binary individuals.

## Instructions

### Formatting Your Data

Our first step is to prepare your raw TOCS-2 data.

1.  Prepare a spreadsheet, preferably a .csv file, with a row for each
    of the tests you’d like to score.

2.  Be sure the following columns are present in the spreadsheet and
    rename the columns to match the reference guide below. All of the
    following columns are necessary for the model. If age or
    p_respondent are missing from a row, the model will not return a
    t-score for that row.

    [TABLE]

### Generate Scores

Use the code below to generate your t-scores. First, you will be
prompted to select your file. Second, we will check that your data are
formatted properly. Third, t-scores will be generated.

If you receive an error, please correct the issue in your file, save
your file, then run the
[`score_tocs2()`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/score_tocs2.md)
function again.

``` r
library(sfsScorer)

# The score_tocs2 function checks that your data are formatted correctly and generates the t-scores
tocs <- score_tocs2()
```

### Additional options

You have the option to…

``` r
# Score from a data.frame in case the data do not exist in a csv file
tocs <- score_tocs2(df = tocs)

# Specify the input file
tocs <- score_tocs2(file = here("test_scores.csv"))

# Export results to an output folder
tocs <- score_tocs2(output_folder = file.path("C:","Users",..."yourpath"))

# Change the number of allowed missingness. 
# Default missinness is 0 questions. You can use the max_missing variable to allow more missingness
# Doing so will use a prorated score to generate a t-score. This can produce outliers and issues
tocs <- score_tocs2(file = here("test_scores.csv"), max_missing = 2)
```

## Understanding the Output

### Summary Values

| Column    | Description                                                               |
|-----------|---------------------------------------------------------------------------|
| tocs_tot  | A summed score of the answered questions                                  |
| tocs_miss | A count of missing values across                                          |
| tocs_pro  | A prorated score by dividing swan_tot by the number of answered questions |

### T-scores

| Column              | Description                                                                                  |
|---------------------|----------------------------------------------------------------------------------------------|
| tocs_gender_tscores | A t-score normed to the Spit for Science sample that adjusts for age, respondent, and gender |
| tocs_tscores        | A t-score normed to the Spit for Science that adjusts for age and respondent                 |
