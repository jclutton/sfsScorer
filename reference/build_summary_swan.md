# Build Totals and Prorated Totals for Full Test and Subdomains

Use the dataframe from
[`clean_file()`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/clean_file.md)
and the
[`mkpro()`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/mkpro.md)
function to reverse scores, then calculate totals, missingness, and
pro-rated totals for the total test and subdomains

## Usage

``` r
build_summary_swan(df = NULL)
```

## Arguments

- df:

  should be a data.frame from
  [`clean_file()`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/clean_file.md)

## Value

A data frame with all of the totals columns
