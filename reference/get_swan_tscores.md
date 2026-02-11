# Run analysis on SWAN raw values to return t-scores

get_swan_tscores() returns gendered and non-gendered t-scores for the
Strengths and Weaknesses of ADHD Symptoms and Normal Behavior Rating
Scale (SWAN) assessment

## Usage

``` r
get_swan_tscores(file = NULL, output_folder = here::here())
```

## Arguments

- file:

  Pathway to formatted raw SWAN scores. If left blank file finder will
  pop up to allow you to select the file.

- output_folder:

  Output file pathway

  1.  Leave blank - This will output a csv file with the t-scores to
      your working directory

  2.  Specify a pathway - This will output a csv file to the specified
      pathway

  3.  Set to `NULL` - This will not output a csv file

## Value

table with t-scores attached to raw swan values
