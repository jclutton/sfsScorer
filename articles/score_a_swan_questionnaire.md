# Score a SWAN Questionnaire

The Strengths and Weaknesses of ADHD Symptoms and Normal Behavior Rating
Scale (SWAN) is a validated instrument for measuring
attention-deficit/hyperactivity disorder (ADHD) traits ([Burton et al.,
2018](https://doi.org/10.1101/248484)). The sfsScorer package provides
an easy way to automatically score your SWAN tests.

## Quick Start

The code below shows how to score a SWAN questionnaire using the
[`score_swan()`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/score_swan.md)
function.

``` r
library(sfsScorer)
#> sfsScorer has been loaded

#' Here's how we expect the data to be formatted
#' age (5-18)
#' gender (1 = boy, 2 = girl)
#' p_respondent (1 = parent / caregiver responded, 0 = child / youth respondent)
#' All swan questions 1-18 as swan1...swan18
head(random_data, 1)
#>   age gender p_respondent swan1 swan2 swan3 swan4 swan5 swan6 swan7 swan8 swan9
#> 1  13      1            1    -1     2    -2     0     3    -1     0     1     2
#>   swan10 swan11 swan12 swan13 swan14 swan15 swan16 swan17 swan18
#> 1      2      0     -2      2     -1     -1      2      0     -2

# Score from a data frame
scores <- score_swan(df = random_data)
#> ✔ The model scored 5 observations.

# OR
# Score from a a csv or xlsx file
csv <- system.file("extdata", "sample_swan.csv", package = "sfsScorer")
scores_csv <- score_swan(file = csv)
#> ✔ The model scored 5 observations.

# Data are validated by default
 df_mod <- random_data |> 
   dplyr::mutate(swan1 = 6)
 try(scores_csv <- score_swan(df = df_mod))
#> There are 5 impossible values in the file.
#> The only valid values are -3, -2, -1, 0, 1, 2, 3, and NA.
#> 
#> Error in clean_file(df, test = "swan", ignore_check = ignore_check) : 
#>   Please correct or remove these rows - "Row 1: swan1 - 6", "Row 2: swan1
#> - 6", "Row 3: swan1 - 6", "Row 4: swan1 - 6", and "Row 5: swan1 - 6"
 
 # To ignore validation and introduce NAs use `ignore_check = TRUE`
 scores_csv <- score_swan(df = df_mod, ignore_check = TRUE)
#> ! 5 impossible values were changed to NA. This could impact scores. 
#> The only valid values are -3, -2, -1, 0, 1, 2, 3, and NA. To correct, review the following rows before running - "Row 1: swan1 - 6", "Row 2: swan1 - 6", "Row 3: swan1 - 6", "Row 4: swan1 - 6", and "Row 5: swan1 - 6"
#> ✔ The model scored 5 observations.
```

## Notes about the data

- The
  [`score_swan()`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/score_swan.md)
  function will ask you to upload a spreadsheet with your SWAN data. If
  you have any additional columns beyond the necessary columns described
  below, i.e. an identifier, those columns will remain untouched in the
  output. For example, if your input file has two additional columns, an
  identifier and parent’s education, those columns will pass through to
  the output file.

- Scores are automatically reversed before calculating the t-score so
  that a higher score is associated with higher ADHD trait.

- The test is split into two subdomains. Questions 1-9 measure
  inattentiveness. Questions 10-18 measure hyperactivity and
  impulsivity. If more than one question is missing from a subdomain the
  test will not be scored.

- T-scores will be generated based on gendered and non-gendered norms.
  Please feel free to include children who are trans or non-binary in
  your dataset and leave the codes for their gender as appropriate for
  the individuals in your study. Non-gendered t-scores will be generated
  for all individuals. To generate gendered t-scores for binary gendered
  participants be sure to code gender as 1 = boy and 2 = girl. Any
  gender not coded as 1 or 2 will not receive a gendered t-score. Any
  gender not coded as 1 or 2 will not receive a gendered t-score.

## Instructions

### Formatting Your Data

Our first step is to prepare your raw SWAN data.

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
formatted properly. Third, t-scores for the full test as well as the two
subdomains (inattentive and hyperactive) will be generated.

If you receive an error, please correct the issue in your file, save
your file, then run the
[`score_swan()`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/score_swan.md)
function again.

``` r
library(sfsScorer)

# The score_swan checks that your data are formatted correctly and generates the t-scores
swan_tscores <- score_swan()
```

### Additional options

``` r
# Score from a data.frame in case the data do not exist in a csv file, i.e. a REDCap API
swan_tscores <- score_tocs2(df = sample_data)

# Example of how to specify the input file
swan_tscores <- score_swan(file = here("test_scores.csv"))

# Example of how to specify an output folder
swan_tscores <- score_swan(output_folder = file.path("C:","Users",..."yourpath"))

# Data are automatically validated. 
# To ignore validation and introduce NA, ignore_check = TRUE
swan_tscores <- score_swan(df = sample_data, ignore_check = TRUE)
```

## Understanding the Output

### Reversed SWAN Scores

- Columns `swan1` to `swan18` are reverse-scored and returned as
  `swan1_reversed` to `swan18` reversed respectively.

### Summary Values

| Column       | Description                                                                                                |
|--------------|------------------------------------------------------------------------------------------------------------|
| swan_tot     | A summed score of the answered questions across the entire test                                            |
| swan_miss    | A count of missing values across the entire test                                                           |
| swan_pro     | A prorated score by dividing swan_tot by the number of answered questions across the entire test           |
| swan_ia_tot  | A summed score of the answered questions across the inattentive subdomain                                  |
| swan_ia_miss | A count of missing values across the inattentive subdomain                                                 |
| swan_ia_pro  | A prorated score by dividing swan_tot by the number of answered questions across the inattentive subdomain |
| swan_hi_tot  | A summed score of the answered questions across the hyperactive subdomain                                  |
| swan_hi_miss | A count of missing values across the hyperactive subdomain                                                 |
| swan_hi_pro  | A prorated score by dividing swan_tot by the number of answered questions across the hyperactive subdomain |

### T-scores for generic model

| Column                  | Description                                                                                           |
|-------------------------|-------------------------------------------------------------------------------------------------------|
| swan_tot_gender_tscores | A t-score across the entire SWAN test that adjusts for age, respondent, and gender                    |
| swan_tot_tscores        | A t-score across the entire SWAN test that adjusts for age and respondent                             |
| swan_ia_gender_tscores  | A t-score of the inattentive subdomain (questions 1-9) that adjusts for age, respondent, and gender   |
| swan_ia_tscores         | A t-score of the inattentive subdomain (questions 1-9) that adjusts for age and respondent            |
| swan_hi_gender_tscores  | A t-score of the hyperactive subdomain (questions 10-18) that adjusts for age, respondent, and gender |
| swan_hi_tscores         | A t-score of the hyperactive subdomain (questions 10-18) that adjusts for age and respondent          |
