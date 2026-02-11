# Runs the model that adjusts for time and create t-scores

This is the generic model used to create t-scores. It adjusts for
gender, age, respondent, and time. It is best used in all cases unless a
team is trying to look at values over time Use the dataframe from
`build_summary()` to produce t-scores

## Usage

``` r
run_model_swan(df = NULL)
```

## Arguments

- df:

  should be a data.frame from `build_summary()`

## Value

A data frame with t-scores
