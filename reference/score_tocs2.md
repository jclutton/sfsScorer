# Run analysis on TOCS raw values to return t-scores

score_tocs2() returns gendered and non-gendered t-scores for the
[Toronto Obsessive-Compulsive Scale (TOCS)
assessment](https://pubmed.ncbi.nlm.nih.gov/27015722/)

**\[experimental\]**

## Usage

``` r
score_tocs2(
  df = NULL,
  file = FALSE,
  output_folder = NULL,
  age_var = "age",
  gender_var = "gender",
  respondent_var = "p_respondent",
  tocs2_vars = paste0("tocs", seq(1, 24)),
  max_missing = 0,
  ignore_check = FALSE
)
```

## Arguments

- df:

  If you already have the TOCS-2 data in your R environment, pass the
  dataframe to this parameter

- file:

  If you prefer scoring a spreadsheet...

  1.  Change to `TRUE` to pop-up a finder to allow you select a file.
      Alternatively, leave df and file empty to pop-up a finder.

  2.  Or specify a pathway

- output_folder:

  Optional, output file pathway. Defauts to `NULL`. Specify a pathway to
  output a csv file.

- age_var:

  Name of the age variable in your data

- gender_var:

  Name of the gender variable in your data

- respondent_var:

  Name of the respondent variable in your data

- tocs2_vars:

  Column names of the 24 TOCS questions

- max_missing:

  By default, 0 items are allowed to be missing on the TOCS. Any
  questionnaire with 1 or more missing, will not be scored. If you'd
  like to adjust this number, change the max_missing value. This will
  use a prorated score to generate t-scores. Please be aware that
  missingness can induce issues when analyzing.

- ignore_check:

  Data are validated to look for missing or improperly formatted values
  before scoring. Errors are thrown when data aren't valid; however,
  this can cause issues in real data sets where data vary for good
  reasons. To skip the validation process, set ignore_check to `TRUE`.
  NAs will be returned where data are invalid

## Value

A dataframe where...  

- T-scores and total scores columns are added

- SWAN questions are reversed scored so that higher numbers match
  increased symptoms.

- Otherwise all columns are not modified

## Examples

``` r
# Read in the file of scores
# This is an example file
csv <- system.file("extdata", "sample_tocs.csv", package = "sfsScorer")

# Score via the file parameter
scores_csv <- score_tocs2(file = csv)
#> ✔ The model scored 5 observations.

# Score via the df paramter
df <- rio::import(csv)
scores_csv <- score_tocs2(df = df)
#> ✔ The model scored 5 observations.

# The data are automatically validated.
# To ignore the validation errors and introduce `NA`, set `ignore_check = TRUE`
df_mod <- df |>
  dplyr::mutate(p_respondent = 2)
scores_csv <- score_tocs2(df = df_mod, ignore_check = TRUE)
#> ! 5 non-valid p_respondent values were changed to NA. This could impact scores. 
#> The only valid p_respondent  value are 1 and 0. To correct, review the following rows before re-running - 1, 2, 3, 4, and 5
#> ✔ The model scored 0 observations.
```
