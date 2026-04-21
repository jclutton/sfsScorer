# Make Prorated Scores

maxmiss: minimum number of missing values that sets total and prorated
total to missing

## Usage

``` r
mkpro(maxmiss = NA, dat = NA, required_test_cols = NULL, newroot = "swan")
```

## Arguments

- maxmiss:

  maximum number of missing values before can be considered invalid

- dat:

  should be a data.frame from
  [`clean_file()`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/clean_file.md)

- required_test_cols:

  An array of variable names that accounts for all of the questions in
  the questionnaire

- newroot:

  a new name if root names need to be changed

## Value

A data frame ready for use or an error

## Development

2026-04-21: Changed Annie's original code to match the changes to the
rest of the code. Allows for dynamic variable naming now.

## Author

Annie
