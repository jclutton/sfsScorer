# Run analysis on SWAN raw values to return t-scores

score_swan() returns gendered and non-gendered t-scores for the
Strengths and Weaknesses of ADHD Symptoms and Normal Behavior Rating
Scale (SWAN) assessment

**\[experimental\]**

## Usage

``` r
score_swan(df = NULL, file = FALSE, output_folder = NULL, ignore_check = FALSE)
```

## Arguments

- df:

  If you already have the SWAN data in your R environment, pass the
  dataframe to this parameter

- file:

  If you prefer scoring a spreadsheet...

  1.  Change to `TRUE` to pop-up a finder to allow you select a file.
      Alternatively, leave df and file empty to pop-up a finder.

  2.  Or specify a pathway

- output_folder:

  Optional, output file pathway. Defauts to `NULL`. Specify a pathway to
  output a csv file.

- ignore_check:

  Data are validated to look for missing or improperly formatted values
  before scoring. Errors are thrown when data aren't valid; however,
  this can cause issues in real data sets where data vary for good
  reasons. To skip the validation process, set ignore_check to `TRUE`.
  NAs will be returned where data are invalid

## Value

table with t-scores attached to raw swan values

## Examples

``` r
# Read in the file of scores
# This is an example file
csv <- system.file("extdata", "sample_swan.csv", package = "sfsScorer")

# Score via the file parameter
scores_csv <- score_swan(file = csv)
#> ✔ The model scored 5 observations.

# Score via the df paramter
df <- rio::import(csv)
scores_csv <- score_swan(df = df)
#> ✔ The model scored 5 observations.

# Data will be validated
df_mod <- df |>
  dplyr::mutate(swan1 = 6)
try(scores_csv <- score_swan(df = df_mod))
#> There are 5 impossible values in the file.
#> The only valid values are -3, -2, -1, 0, 1, 2, 3, and NA.
#> 
#> Error in clean_file(df, test = "swan", ignore_check = ignore_check) : 
#>   Please correct or remove these rows - "Row 1: swan1 - 6", "Row 2: swan1
#> - 6", "Row 3: swan1 - 6", "Row 4: swan1 - 6", and "Row 5: swan1 - 6"

# To ignore the validation errors and introduce `NA`, set `ignore_check = TRUE`
scores_csv <- score_swan(df = df_mod, ignore_check = TRUE)
#> ! 5 impossible values were changed to NA. This could impact scores. 
#> The only valid values are -3, -2, -1, 0, 1, 2, 3, and NA. To correct, review the following rows before running - "Row 1: swan1 - 6", "Row 2: swan1 - 6", "Row 3: swan1 - 6", "Row 4: swan1 - 6", and "Row 5: swan1 - 6"
#> ✔ The model scored 5 observations.

```
