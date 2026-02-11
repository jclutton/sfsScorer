# An old version of the model, initially written in Annie's language

This is the generic model used to create t-scores. It adjusts for
gender, age, respondent, and time. It is best used in all cases unless a
team is trying to look at values over time Use the dataframe from
[`build_summary_swan()`](https://jclutton.github.io/sfsScorer/reference/build_summary_swan.md)
to produce t-scores

## Usage

``` r
run_model_old_swan(df = NULL)
```

## Arguments

- df:

  should be a data.frame from
  [`build_summary_swan()`](https://jclutton.github.io/sfsScorer/reference/build_summary_swan.md)

## Value

A data frame with t-scores
