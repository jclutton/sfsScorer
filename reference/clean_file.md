# Clean File

This function runs checks to be sure that the file and non-test specific
data are formatted correctly

## Usage

``` r
clean_file(
  df = NULL,
  test = NULL,
  ignore_check = NULL,
  age_var = "age",
  gender_var = "gender",
  respondent_var = "p_respondent",
  required_test_cols = NULL
)
```

## Arguments

- df:

  The df function allows you to point to a dataframe as opposed to a
  file

- test:

  Which questionnaire are we running

- ignore_check:

  Data are validated to look for missing or improperly formatted values
  before scoring. Errors are thrown when data aren't valid; however,
  this can cause issues in real data sets where data vary for good
  reasons. To skip the validation process, set ignore_check to TRUE. NAs
  will be returned where data are invalid

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

A clean data frame ready for t-scores
