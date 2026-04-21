# Build Totals and Prorated Totals for Full Test and Subdomains

Use the dataframe from
[`clean_file()`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/clean_file.md)
and the
[`mkpro()`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/mkpro.md)
function to reverse scores, then calculate totals, missingness, and
pro-rated totals for the total test and subdomains

## Usage

``` r
build_summary_tocs(
  df = NULL,
  max_missing = NULL,
  age_var = "age",
  gender_var = "gender",
  respondent_var = "p_respondent",
  required_test_cols = NULL
)
```

## Arguments

- df:

  should be a data.frame from
  [`clean_file()`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/clean_file.md)

- max_missing:

  max_missing is passed from the
  [`score_tocs2()`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/score_tocs2.md)
  function. By default, the tocs allows 0 missing items.

- age_var:

  Name of the age variable in your data

- gender_var:

  Name of the gender variable in your data

- respondent_var:

  Name of the respondent variable in your data

- required_test_cols:

  An array of the names of the questionnaires questions, i.e. swan1,
  swan2, etc

## Value

A data frame with all of the totals columns
