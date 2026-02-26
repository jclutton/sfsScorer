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
csv <- system.file("extdata", "sample_swan.csv", package = "sfsScorer")

# Score via the file parameter
scores_csv <- score_swan(file = csv)
#> ✔ The model scored 5 observations.
#> # A tibble: 4 × 6
#> # Groups:   gender, youth [4]
#>   gender youth p_respondent     n  mean     sd
#>    <int> <dbl>        <int> <int> <dbl>  <dbl>
#> 1      1     1            1     1  50.8 NA    
#> 2      2     0            1     2  55.5  0.844
#> 3      2     1            0     1  58.3 NA    
#> 4      5     0            1     1 NaN   NA    

# Score via the df paramter
df <- rio::import(csv)
scores_csv <- score_swan(df = df)
#> ✔ The model scored 5 observations.
#> # A tibble: 4 × 6
#> # Groups:   gender, youth [4]
#>   gender youth p_respondent     n  mean     sd
#>    <int> <dbl>        <int> <int> <dbl>  <dbl>
#> 1      1     1            1     1  50.8 NA    
#> 2      2     0            1     2  55.5  0.844
#> 3      2     1            0     1  58.3 NA    
#> 4      5     0            1     1 NaN   NA    

# The data are automatically validated.
# To ignore the validation errors and introduce `NA`, set `ignore_check = FALSE`
scores_csv <- score_swan(df = df, ignore_check = FALSE)
#> ✔ The model scored 5 observations.
#> # A tibble: 4 × 6
#> # Groups:   gender, youth [4]
#>   gender youth p_respondent     n  mean     sd
#>    <int> <dbl>        <int> <int> <dbl>  <dbl>
#> 1      1     1            1     1  50.8 NA    
#> 2      2     0            1     2  55.5  0.844
#> 3      2     1            0     1  58.3 NA    
#> 4      5     0            1     1 NaN   NA    

```
