# Make Prorated Scores

a: first item number in questionnaire (usually "1")

## Usage

``` r
mkpro(
  maxmiss = NA,
  dat = NA,
  a = NULL,
  b = NULL,
  root = "swan",
  newroot = "swan"
)
```

## Arguments

- maxmiss:

  maximum number of missing values before can be considered invalid

- dat:

  should be a data.frame from
  [`clean_file()`](https://jclutton.github.io/sfsScorer/reference/clean_file.md)

- a:

  First question of subset

- b:

  Last question of subset

- root:

  Root name of

- newroot:

  a new name if root names need to be changed

## Value

A data frame ready for use or an error

## Author

Annie
