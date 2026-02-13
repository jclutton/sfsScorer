# Build Totals and Prorated Totals for Full Test and Subdomains

Use the dataframe from
[`clean_file()`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/clean_file.md)
and the
[`mkpro()`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/mkpro.md)
function to reverse scores, then calculate totals, missingness, and
pro-rated totals for the total test and subdomains

## Usage

``` r
build_summary_tocs(df = NULL, max_missing = NULL)
```

## Arguments

- df:

  should be a data.frame from
  [`clean_file()`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/clean_file.md)

- max_missing:

  max_missing is passed from the
  [`get_tocs_tscores()`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/get_tocs_tscores.md)
  function. By default, the tocs allows 0 missing items.

## Value

A data frame with all of the totals columns
