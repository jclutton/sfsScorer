# Build Totals and Prorated Totals for Full Test and Subdomains

Use the dataframe from
[`clean_file`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/clean_file.md)
and the
[`mkpro`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/mkpro.md)
function to reverse scores, then calculate totals, missingness, and
pro-rated totals for the total test and subdomains

## Usage

``` r
build_summary_swan(
  df = NULL,
  age_var = "age",
  gender_var = "gender",
  respondent_var = "p_respondent",
  required_test_cols = NULL,
  reverse_scored = NULL
)
```

## Arguments

- df:

  should be a data.frame from
  [`clean_file`](https://Schachar-Crosbie-Lab.github.io/sfsScorer/reference/clean_file.md)

- age_var:

  Name of the age variable in your data

- gender_var:

  Name of the gender variable in your data

- respondent_var:

  Name of the respondent variable in your data

- required_test_cols:

  An array of the names of the questionnaires questions, i.e. swan1,
  swan2, etc

- reverse_scored:

  Different versions of the SWAN exist. To properly score values, we
  need SWAN questions 1 to 18 to be reverse scored where **Far Below**
  is equivalent to **3** so that high numbers pair with high symptoms.

  - `TRUE` - Far Below is equivalent to 3

  - `FALSE` - Far Below is equivalent to -3

  - `NULL` - An interactive workflow will ask you questions about your
    data

## Value

A data frame with all of the totals columns
