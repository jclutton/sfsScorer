#     sfsScorer: Scoring tools for commonly used child and youth psychiatric questionnaires.
#     Copyright (C) @ 2026 SickKids
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU Affero General Public License as
#     published by the Free Software Foundation, either version 3 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU Affero General Public License for more details.
#
#     You should have received a copy of the GNU Affero General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.

#### Load language ####
#### Declares language upload loading the package.
.onAttach <- function(libname, pkgname) {
  #packageStartupMessage("sfsScorer Copyright (C) 2026 SickKids\nThis program comes with ABSOLUTELY NO WARRANTY. ")

  welcome <- function() {
    cli::cli_inform(cli::cli({
      cli::cli_par()
      cli::cli_text("{.pkg sfsScorer} Copyright (C) 2026 Jonathan Clutton")
      cli::cli_text("This program comes with ABSOLUTELY NO WARRANTY")
      cli::cli_text("This is free software, and you are welcome to redistribute it under certain conditions")
      cli::cli_text("See conditions: {.url https://jclutton.github.io/sfsScorer/LICENSE.html}")
      cli::cli_par()
      }),
    class = "packageStartupMessage")
  }

  welcome()

}

#### Global Variables ####
globalVariables(c())


#### clean_file function ####
#' @name clean_file
#'
#' @title Clean File
#'
#' @description This function runs checks to be sure that the file and non-test specific data are formatted correctly
#'
#' @param df The df function allows you to point to a dataframe as opposed to a file
#' @param test Which questionnaire are we running
#' @param ignore_check Data are validated to look for missing or improperly formatted values before scoring. Errors are thrown when data aren't valid; however, this can cause issues
#' in real data sets where data vary for good reasons. To skip the validation process, set ignore_check to TRUE. NAs will be returned where data are invalid
#' @param required_test_cols An array of the names of the questionnaires questions, i.e. swan1, swan2, etc
#' @inheritParams score_swan
#'
#' @importFrom rio import
#' @importFrom rio get_ext
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter
#' @importFrom dplyr anti_join
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr rename
#' @importFrom rlang .data
#' @import cli
#'
#'
#' @returns A clean data frame ready for t-scores
#'
#'
#'
clean_file <- function(df = NULL, test = NULL, ignore_check = NULL,
                       age_var = 'age', gender_var = 'gender', respondent_var = 'p_respondent', required_test_cols = NULL) {

  # Create temporary df and add row number for alerts
  df_temp <- df |>
    mutate(row = row_number())

  # Check for Required Questions
  if(test == 'swan'){

    if(length(required_test_cols) != 18){

      cli::cli_abort(paste('Please check that all of the SWAN questions are in your dataframe. There should be 18 total questions, usually numbered swan1, swan2, etc.'))
    }

    # swan only allows values -3 to 3
    value_range <- c(seq(-3, 3, 1), NA)
  } else if(test == 'tocs'){

    if(length(required_test_cols) != 24){

      cli::cli_abort(paste('Please check that all of the TOCS-2 questions are in your dataframe. There should be 24 total questions, usually numbered tocs1, tocs2, etc.'))
    }

    value_range <- c(seq(-3, 3, 1), NA)
  }

  required_dem_cols <- c(age_var, gender_var, respondent_var)

  if(!all(c(required_test_cols,required_dem_cols) %in% colnames(df))){

    missing_cols <- c(required_test_cols,required_dem_cols)[which(!c(required_test_cols,required_dem_cols) %in% colnames(df))]

      cli::cli_abort(paste('Please check the column names in your file. The file appears to be missing the following required columns...\n
               {.val {missing_cols}}'))
  }

  ##### Check for impossible values ###
  df_long <- df_temp |>
    tidyr::pivot_longer(cols = dplyr::all_of(required_test_cols),
                        names_to = 'name',
                        values_to = 'value') |>
    dplyr::filter(!.data$value %in% value_range)

  if(nrow(df_long) > 0){

    values <- df_long |>
      select(c('row', 'name', 'value')) |>
      mutate(text = paste0("Row ",.data$row,": ", .data$name," - ", .data$value))

    if(!ignore_check){

      cli::cli_par()
      cli::cli_text("There {?is/are} {nrow(df_long)} impossible value{?s} in the file.")
      cli::cli_text("The only valid values are {.val {value_range}}.")
      cli::cli_end()
      cli::cli_abort("Please correct or remove these rows - {.val {values$text}}")

    } else if(ignore_check){

      df <- df |>
        mutate(across(all_of(required_test_cols),
                      ~case_when(.x %in% value_range ~ .x,
                                 T ~ NA)))
      cli::cli_alert_warning(paste("{nrow(df_long)} impossible values were changed to {.emph NA}. This could impact scores. \n",
                                   "The only valid values are {.val {value_range}}. To correct, review the following rows before running - {.val {values$text}}"))
    }

  }

  # P_respondent
  if(any(!(df[[respondent_var]] %in% c(0,1)))) {

    vals <- c(1,0)

    rows <- df_temp |>
      filter(! {{ respondent_var }} %in% c(0,1))

    if(!ignore_check){

      cli::cli_par()
      cli::cli_text("There is/are {sum(!df[[respondent_var]] %in% c(0,1))} non-valid value(s) in the {.field {respondent_var}} column.")
      cli::cli_text("The only valid values are {.val {vals}}.")
      cli::cli_end()
      cli::cli_abort("Please correct or remove these rows - {.val {rows$row}}")

    } else if(ignore_check){

      df <- df |>
        mutate(across(all_of(c('p_respondent')),
                      ~case_when(.x %in% vals ~ .x,
                                 T ~ NA)))
      cli::cli_alert_warning(paste("{sum(!df[[respondent_var]] %in% c(0,1))} non-valid {.field {respondent_var}} values were changed to {.emph NA}. This could impact scores. \n",
                                   "The only valid {.field {respondent_var}}  value{?s} are {.val {vals}}. To correct, review the following rows before re-running - {.val {rows$row}}"))
    }

  }

  # Check age class
  if(!class(df[[age_var]]) %in% c('numeric','integer')){

    suppressWarnings(rows <- df_temp |>
      mutate(parse_age = as.numeric({{age_var}})) |>
      filter(is.na(.data$parse_age)))

    if(!ignore_check){

      cli::cli_par()
      cli::cli_text("There is/are {sum(is.na(rows$parse_age))} non-numeric value(s) in the {.field {age_var}} column.")
      cli::cli_text("{.field {age_var}} must be a number between {.val {c(5,19)}}.")
      cli::cli_end()
      cli::cli_abort("Please correct or remove these rows - {.val {rows$row}}")

    } else if(ignore_check){

      suppressWarnings(df <- df |>
        mutate(age = as.numeric(.data$age)))

      cli::cli_alert_warning(paste("{sum(is.na(rows$parse_age))} non-valid {.field {age_var}} value(s) was/were changed to {.emph NA}. This could impact scores. \n",
                                   "{.field {age_var}} must be a number between {.val {c(5,19)}}. To correct, review the following rows before re-running - {.val {rows$row}}"))
    }


    }

  # Check that Age is formatted correctly
  if(any(df[[age_var]] >= 19, na.rm = T) | any(df[[age_var]]< 5, na.rm = T)){

    rows <- df_temp |>
      filter({{age_var}} >= 19 | {{age_var}} < 5)

    if(!ignore_check){

      cli::cli_par()
      cli::cli_text("There {?is/are} {nrow(rows)} non-valuid values in the {.field age} column.")
      cli::cli_text("{.field age} must be a number between {.val {c(5,19)}}.")
      cli::cli_end()
      cli::cli_abort("Please correct or remove these rows - {.val {rows$row}}")

    } else if(ignore_check){

      df <- df |>
        mutate(across(all_of(age_var),
                      ~case_when(.x >= 19 | .x < 5 ~ NA,
                                 T ~ age)))

      cli::cli_alert_warning(paste("{nrow(rows)} non-valid {.field {age_var}} value(s) were changed to {.emph NA}. This could impact scores. \n",
                                   "{.field {age_var}} must be a number between {.val {c(5,19)}}. To correct, review the following rows before re-running - {.val {rows$row}}"))
    }

  }

  # Check gender
  if(!any(as.character(unique(df[[gender_var]])) %in% c('1','2'))){
    warning(paste("The package wasn't able to find any gender coded as 1 or 2, and therefore won't generate any gender-based t-scores. If you'd like to generate gender-based t-scores, gender should be coded as... \n",
                  "1 = Boy \n",
                  "2 = Girl \n"))
  }


  return(df = df)
}



#' @name mkpro
#'
#' @title Make Prorated Scores
#'
#' @author Annie
#'
#' @description maxmiss: minimum number of missing values that sets total and prorated total to missing
#          WARNING: default does not set any totals or pro-rated totals to missing to leave this up to the individual analyst
# dat: name of data frame with items - default is S2quest
# newroot: root for new variable names if different (used for subdomain totals) - default if newroot is not specified in call to function, newroot = root
#'
#' @section Development:
#' 2026-04-21: Changed Annie's original code to match the changes to the rest of the code. Allows for dynamic variable naming now.
#'
#'
#' @param dat should be a data.frame from [clean_file()]
#' @param maxmiss maximum number of missing values before can be considered invalid
#' @param newroot a new name if root names need to be changed
#' @param required_test_cols An array of variable names that accounts for all of the questions in the questionnaire
#'
#' @returns A data frame ready for use or an error
#'
mkpro <- function(maxmiss = NA, dat = NA, required_test_cols = NULL, newroot = 'swan' ) {


  cnams <- required_test_cols
  n <- length(cnams)

  tot <- apply(dat[, cnams], 1 , sum, na.rm = T)
  miss <- apply(dat[, cnams], 1 , function(x)
    sum(is.na(x)))
  pro <- tot / (n - miss) * n

  if (is.na(maxmiss))
    maxmiss <- n

  pro <- ifelse(miss > maxmiss, NA, pro)
  tot <- ifelse(miss > maxmiss, NA, tot)

  allvars <- cbind(tot, miss, pro)
  colnames(allvars) <-
    c(stringr::str_c(newroot, "_tot"),
      stringr::str_c(newroot, "_miss"),
      stringr::str_c(newroot, "_pro"))

  return(allvars)
}

