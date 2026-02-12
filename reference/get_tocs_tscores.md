# Run analysis on TOCS raw values to return t-scores

get_tocs_tscores() returns gendered and non-gendered t-scores for the
[Toronto Obsessive-Compulsive Scale (TOCS)
assessment](https://pubmed.ncbi.nlm.nih.gov/27015722/)

**\[experimental\]**

## Usage

``` r
get_tocs_tscores(file = NULL, output_folder = here::here(), max_missing = 0)
```

## Arguments

- file:

  Pathway to formatted raw scores. If left blank file finder will pop up
  to allow you to select the file.

- output_folder:

  Output file pathway

  1.  Leave blank - This will output a csv file with the t-scores to
      your working directory

  2.  Specify a pathway - This will output a csv file to the specified
      pathway

  3.  Set to `NULL` - This will not output a csv file

- max_missing:

  By default, 0 items are allowed to be missing on the TOCS. Any
  questionnaire with 1 or more missing, will not be scored. If you'd
  like to adjust this number, change the max_missing value. This will
  use a prorated score to generate t-scores. Please be aware that
  missingness can induce issues when analyzing.

## Value

table with t-scores attached to raw swan values
