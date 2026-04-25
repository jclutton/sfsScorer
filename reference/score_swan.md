# Run analysis on SWAN raw values to return t-scores

score_swan() returns gendered and non-gendered t-scores for the
Strengths and Weaknesses of ADHD Symptoms and Normal Behavior Rating
Scale (SWAN) assessment

**\[experimental\]**

## Usage

``` r
score_swan(
  df = NULL,
  file = FALSE,
  age_var = "age",
  gender_var = "gender",
  respondent_var = "p_respondent",
  swan_vars = paste0("swan", seq(1, 18)),
  reverse_scored = NULL,
  ignore_check = FALSE,
  output_folder = NULL
)
```

## Arguments

- df:

  Dataframe with SWAN data

- file:

  If you prefer scoring a spreadsheet...

  1.  Change to `TRUE` to pop-up a finder to allow you select a file.
      Alternatively, leave df and file empty to pop-up a finder.

  2.  Or specify a pathway

- age_var:

  Name of the age variable in your data

- gender_var:

  Name of the gender variable in your data

- respondent_var:

  Name of the respondent variable in your data

- swan_vars:

  Column names of the 18 SWAN questions

- reverse_scored:

  Different versions of the SWAN exist. To properly score values, we
  need SWAN questions 1 to 18 to be reverse scored where **Far Below**
  is equivalent to **3** so that high numbers pair with high symptoms.

  - `TRUE` - Far Below is equivalent to 3

  - `FALSE` - Far Below is equivalent to -3

  - `NULL` - An interactive workflow will ask you questions about your
    data

- ignore_check:

  Data are validated to look for missing or improperly formatted values
  before scoring. Errors are thrown when data aren't valid; however,
  this can cause issues in real data sets where data vary for good
  reasons. To skip the validation process, set ignore_check to `TRUE`.
  NAs will be returned where data are invalid

- output_folder:

  Optional, output file pathway. Defauts to `NULL`. Specify a pathway to
  output a csv file.

## Value

A dataframe where...  

- T-scores and total scores columns are added

- SWAN questions are reversed scored so that higher numbers match
  increased symptoms.

- Otherwise all columns are not modified

## Examples

``` r
#' # Read in the file of scores
csv <- system.file("extdata", "sample_swan.csv", package = "sfsScorer")

# Score via the file parameter
scores_csv <- score_swan(file = csv, reverse_scored = FALSE)
#> ✔ The model scored 5 observations.

# Already working with a dataframe? Score via the df paramter
df <- rio::import(csv)
# Name your variables
scores_csv <- score_swan(df = df, age_var = 'age',
gender_var = 'gender', respondent_var = 'p_respondent', reverse_scored = FALSE)
#> ✔ The model scored 5 observations.

# Data will be validated
df_mod <- df |>
  dplyr::mutate(swan1 = 6)

try(scores_csv <- score_swan(df = df_mod, reverse_scored = FALSE))
#> There are 5 impossible values in the file.
#> The only valid values are -3, -2, -1, 0, 1, 2, 3, and NA.
#> 
#> Error in clean_file(df, test = "swan", ignore_check = ignore_check, age_var = age_var,  : 
#>   Please correct or remove these rows - "Row 1: swan1 - 6", "Row 2: swan1
#> - 6", "Row 3: swan1 - 6", "Row 4: swan1 - 6", and "Row 5: swan1 - 6"

# To ignore the validation errors and introduce `NA`, set `ignore_check = TRUE`
scores_csv <- score_swan(df = df_mod, ignore_check = TRUE, reverse_scored = FALSE)
#> ! 5 impossible values were changed to NA. This could impact scores. 
#> The only valid values are -3, -2, -1, 0, 1, 2, 3, and NA. To correct, review the following rows before running - "Row 1: swan1 - 6", "Row 2: swan1 - 6", "Row 3: swan1 - 6", "Row 4: swan1 - 6", and "Row 5: swan1 - 6"
#> ✔ The model scored 5 observations.

```
