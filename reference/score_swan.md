# Run analysis on SWAN raw values to return t-scores

score_swan() returns gendered and non-gendered t-scores for the
Strengths and Weaknesses of ADHD Symptoms and Normal Behavior Rating
Scale (SWAN) assessment

**\[experimental\]**

## Usage

``` r
score_swan(df = NULL, file = FALSE, output_folder = NULL)
```

## Arguments

- df:

  If you already have the SWAN data in your R environment, pass the
  dataframe to this parameter

- file:

  If you prefer scoring a spreadsheet...

  1.  TRUE - This will pop-up a finder to allow you select a file

  2.  Specify a pathway

- output_folder:

  Output file pathway

  1.  Leave blank - This will output a csv file with the t-scores to
      your working directory

  2.  Specify a pathway - This will output a csv file to the specified
      pathway

  3.  Set to `NULL` - This will not output a csv file

## Value

table with t-scores attached to raw swan values
