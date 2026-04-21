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


#' @name score_swan
#'
#' @title Run analysis on SWAN raw values to return t-scores
#'
#' @description score_swan() returns gendered and non-gendered t-scores for the Strengths and Weaknesses of ADHD Symptoms and Normal Behavior Rating Scale (SWAN) assessment
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param df Dataframe with SWAN data
#' @param age_var Name of the age variable in your data
#' @param gender_var Name of the gender variable in your data
#' @param respondent_var Name of the respondent variable in your data
#' @param swan_vars Column names of the 18 SWAN questions
#' @param file If you prefer scoring a spreadsheet...
#' \enumerate{
#'  \item Change to `TRUE` to pop-up a finder to allow you select a file. Alternatively, leave df and file empty to pop-up a finder.
#'  \item Or specify a pathway
#'  }
#' @param output_folder Optional, output file pathway. Defauts to `NULL`. Specify a pathway to output a csv file.
#' @param ignore_check Data are validated to look for missing or improperly formatted values before scoring. Errors are thrown when data aren't valid; however, this can cause issues
#' in real data sets where data vary for good reasons. To skip the validation process, set ignore_check to `TRUE`. NAs will be returned where data are invalid
#' @param reverse_scored
#' Different versions of the SWAN exist. To properly score values, we need SWAN questions 1 to 18 to be reverse scored where \strong{Far Below} is equivalent to \strong{3} so that high numbers pair with high symptoms.
#'\itemize{
#'  \item `TRUE` - Far Below is equivalent to 3
#'  \item `FALSE` - Far Below is equivalent to -3
#'  \item `NULL` - An interactive workflow will ask you questions about your data
#'  }
#'
#'
#' @importFrom rio export
#' @importFrom lubridate now
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom stringr str_replace_all
#' @importFrom stats sd
#' @importFrom here here
#' @importFrom rlang .data
#' @importFrom utils menu
#' @import cli
#'
#' @returns
#' A dataframe where... \cr
#' \itemize{
#'   \item T-scores and total scores columns are added
#'   \item SWAN questions are reversed scored so that higher numbers match increased symptoms.
#'   \item Otherwise all columns are not modified
#' }
#'
#'
#' @examples
#' # Read in the file of scores
#' csv <- system.file("extdata", "sample_swan.csv", package = "sfsScorer")
#'
#' # Score via the file parameter
#' scores_csv <- score_swan(file = csv, reverse_scored = FALSE)
#'
#' # Already working with a dataframe? Score via the df paramter
#' df <- rio::import(csv)
#' # Name your variables
#' scores_csv <- score_swan(df = df, age_var = 'age',
#' gender_var = 'gender', respondent_var = 'p_respondent', reverse_scored = FALSE)
#'
#' # Data will be validated
#' df_mod <- df |>
#'   dplyr::mutate(swan1 = 6)
#'
#' try(scores_csv <- score_swan(df = df_mod, reverse_scored = FALSE))
#'
#' # To ignore the validation errors and introduce `NA`, set `ignore_check = TRUE`
#' scores_csv <- score_swan(df = df_mod, ignore_check = TRUE, reverse_scored = FALSE)
#'
#'
#' @export
#'
#'

score_swan <- function(df = NULL, file = FALSE,
                       age_var = 'age', gender_var = 'gender', respondent_var = 'p_respondent', swan_vars = paste0('swan',seq(1,18)),
                       reverse_scored = NULL, ignore_check = FALSE, output_folder = NULL) {

  if(is.null(df) | is.character(df) | is.logical(df)){

    # Import df
    if(is.character(file)){

      #Check to make sure the filetype is correct
      if(!rio::get_ext(file) %in% c('csv','xlsx','xls')){
        stop(paste0(basename(file),'s filetype is not usable. It must be a .csv, .xlsx, or .xls filetype. Please correct the filetype before continuing'))
      }

      df <- rio::import(file)
    } else if(file == TRUE | is.logical(file)){
      cli::cli_alert_info('No file pathway was found. Please use the finder to select the file you would like to score.')
      file <- file.choose()
      df <- rio::import(file)
    }
  }

  # Run QC checks on data
  check <- clean_file(df, test = 'swan', ignore_check = ignore_check,
                      age_var = age_var, gender_var = gender_var, respondent_var = respondent_var, required_test_cols = swan_vars)

  # Figure out if questions need to be reverse scored
  if(is.null(reverse_scored)){

    cli::cli_h2("Reverse Scoring")
    cli::cli_text("We need information on how your SWAN questionnaire was designed to properly score it.")
    cli::cli_par()
    #cli::cli_text("Think about how the questions {.val #1} to {.val #18} were scored in your questionnaire.")
    cli::cli_text("Is the answer choice {.field Far Below} equivalent to {.field -3} in your raw data?")
    cli::cli_end()
    reversed1 <- menu(choices = c("Yes","No"))

    if(reversed1 == 1){
      cli::cli_text("Great! So to confirm {.field Far Above} was equivalent to {.field 3}, right?\f")
    } else if(reversed1 == 2){
      cli::cli_text("Okay. So if I understand correctly, {.field Far Below} is equivalent to {.field 3} in your raw data?\f")
    }
    cli::cli_alert_info("Want to skip this step? Use the {.envvar reverse_scored} parameter next time you run the {.fn score_swan} function.")
    reversed2 <- utils::menu(choices = c("Yes","No"))

    if(reversed1 == 1 & reversed2 == 1){
      reverse_scored <- F
      cli::cli_alert_info("{.field {swan_vars[1]}} to {.field {swan_vars[18]}} have been reversed, i.e. multiplied by -1, so that higher numbers match increased symptoms.")
    } else if (reversed1 == 2 & reversed2 == 1){
      reverse_scored <- T
    } else if (reversed2 !=1){
      cli::cli_abort("You selected an option that isn't possible. Please re-run {.fn score_swan}")
    }
  }

  # Summarize Scores
  summary <- build_summary_swan(check,
                                age_var = age_var, gender_var = gender_var, respondent_var = respondent_var, required_test_cols = swan_vars,
                                reverse_scored = reverse_scored)

  # Run the model
  score <- run_model_swan(summary)

  # Print a summary in the console
  cli::cli_alert_success(paste0("The model scored ",sum(!is.na(score$swan_tot_tscores))," observations."))
  if(sum(score$swan_ia_miss > 1 | score$swan_hi_miss > 1) > 0){
    cli::cli_alert_warning(paste0(sum(score$swan_ia_miss > 1 | score$swan_hi_miss > 1)," observations were not scored due to excessive missingness. ",
                                  "Only one question can be missing per subdomain."))
  }

  # Hiding summary for now
  # print(
  #   score |>
  #     dplyr::group_by(.data$gender, .data$youth, .data$p_respondent) |>
  #     dplyr::summarise(n = dplyr::n(),
  #                      mean = mean(.data$swan_tot_gender_tscores, na.rm = T),
  #                      sd = stats::sd(.data$swan_tot_gender_tscores, na.rm = T))
  # )

  score <- score |>
    dplyr::select(-c('age18','youth','female'))

  # Save file if specified
  if(!is.null(output_folder)){

    rio::export(score,
                file.path(output_folder,paste0('swan_scored_',format(lubridate::now(), format='%Y-%m-%d %H-%M-%S'),'.csv')))

    message(paste("A spreadsheet of your scored SWAN tests has been saved to",output_folder))

  }

  return(score = score)

}

#' @name build_summary_swan
#'
#' @title Build Totals and Prorated Totals for Full Test and Subdomains
#'
#' @description Use the dataframe from \code{\link{clean_file}} and the \code{\link{mkpro}} function to reverse scores, then
#' calculate totals, missingness, and pro-rated totals for the total test and subdomains
#'
#' @import dplyr
#'
#' @importFrom rlang .data
#'
#' @param df should be a data.frame from \code{\link{clean_file}}
#' @inheritParams score_swan
#' @inheritParams clean_file
#'
#' @returns A data frame with all of the totals columns
#'
build_summary_swan <- function(df = NULL,
                               age_var = 'age', gender_var = 'gender', respondent_var = 'p_respondent', required_test_cols = NULL,
                               reverse_scored = NULL) {

  ia_subdomain <- required_test_cols[1:9]
  hi_subdomain <- required_test_cols[10:18]

  df_tot <- df |>
    dplyr::mutate(age18 = dplyr::case_when(.data[[age_var]] < 18 ~ .data[[age_var]],
                                           .data[[age_var]] >= 18 ~ 18,
                                           T ~ .data[[age_var]])) |>
    # Use same codings as Annie's script
    dplyr::mutate(female = dplyr::case_when(as.character(.data[[gender_var]]) == "1" ~ 0,
                                            as.character(.data[[gender_var]]) == "2" ~ 1,
                                            T ~ NA)) |>
    dplyr::mutate(youth = dplyr::case_when(.data[[age_var]] < 12 ~ 0,
                                           .data[[age_var]] >= 12 ~ 1,
                                           T ~ NA))

  if(reverse_scored == F){
    df_tot <- df_tot |>
      # Reverse scores
      dplyr::mutate(dplyr::across(dplyr::all_of(c(ia_subdomain, hi_subdomain)),
                                  ~-1*.x))
  }


  #Inattentive
  df_tot <- cbind(df_tot, mkpro(dat = df_tot, required_test_cols = ia_subdomain, newroot = 'swan_ia', maxmiss = 1))

  #Hyperactive
  df_tot <- cbind(df_tot, mkpro(dat = df_tot, required_test_cols = hi_subdomain, newroot = 'swan_hi', maxmiss = 1))

  #Whole test scores
  df_tot <- cbind(df_tot, mkpro(dat = df_tot, required_test_cols = c(ia_subdomain, hi_subdomain))) |>
    # If a subdomain is missing more than one, mark as NA
    dplyr::mutate(dplyr::across(c('swan_tot','swan_pro'),
                                ~ dplyr::case_when(.data$swan_ia_miss > 1 | .data$swan_hi_miss > 1 ~ NA,
                                                   T ~ .)))




  return(df_tot = df_tot)

}

#' @name run_model_swan
#'
#' @title Runs the model that adjusts for time and create t-scores
#'
#' @description This is the generic model used to create t-scores. It adjusts for gender, age, respondent, and time.
#' It is best used in all cases unless a team is trying to look at values over time
#' Use the dataframe from [build_summary_swan()] to produce t-scores
#'
#' @param df should be a data.frame from [build_summary_swan()]
#'
#' @returns A data frame with t-scores
#'
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr contains
#' @importFrom rlang .data
#'
run_model_swan <- function(df = NULL) {


  #### Produce t-scores with gender
  df_mod <- df |>
    dplyr::mutate(swan_gender_pred = -4.0630359 - 0.3384133  * .data$age18 + 1.7004264 * .data$female + 1.5455007 *
                    .data$p_respondent - 8.3141252 * .data$female * .data$p_respondent) |>
    dplyr::mutate(swan_gender_low = as.numeric((.data$swan_pro - .data$swan_gender_pred) < 0)) |>
    dplyr::mutate(swan_gender_sd_pred = sqrt(325.95663 -
                                               7.12465 * .data$age18 +
                                               13.4144 * .data$female -
                                               229.07860 * .data$p_respondent +
                                               106.69317 * .data$swan_gender_low  +
                                               29.39191 * .data$age18 * .data$p_respondent -
                                               44.74060 * .data$female * .data$p_respondent)) |>
    dplyr::mutate(res_adj = dplyr::case_when(.data$female == 0 & .data$youth == 0  ~ -0.06113885,
                                             .data$ female == 0 & .data$youth == 1  ~ -0.06736433,
                                             .data$female == 1  & .data$youth == 0 ~ -0.07421668,
                                             .data$ female ==1  & .data$youth ==1 ~ -0.06374547)) |>
    dplyr::mutate(sd_adj = dplyr::case_when(.data$female == 0 & .data$youth == 0  ~ 0.9986281,
                                            .data$female == 0 & .data$youth == 1  ~ 0.9879085,
                                            .data$female == 1  & .data$youth == 0 ~ 0.9976924,
                                            .data$female ==1  & .data$youth ==1 ~ 0.9972819)) |>
    dplyr::mutate(swan_tot_gender_tscores = dplyr::case_when(.data$age18 < 12 & .data$p_respondent == 0 ~ NA,
                                                             T ~ (((.data$swan_pro - .data$swan_gender_pred) / .data$swan_gender_sd_pred) + .data$res_adj) / (.data$sd_adj) * 10 + 50))

  #### Full Test Across Gender
  df_mod <- df_mod |>
    dplyr::mutate(swan_pred = -3.3512518 - 0.3206639 * .data$age18 - 2.5708190 * .data$p_respondent) |>
    dplyr::mutate(swan_low = as.numeric((.data$swan_pro - .data$swan_pred) < 0)) |>
    dplyr::mutate(swan_sd_pred = sqrt(314.405841 -
                                        4.962281 * .data$age18 -
                                        252.382387 * .data$p_respondent +
                                        156.755038 * .data$swan_low +
                                        30.436026 * .data$age18 * .data$p_respondent -
                                        5.112260 * .data$age18 * .data$swan_low)) |>
    dplyr::mutate(res_adj = dplyr::case_when(.data$youth == 0  ~ -0.06810488,
                                             .data$youth == 1  ~ -0.05023800)) |>
    dplyr::mutate(sd_adj = dplyr::case_when(.data$youth == 0  ~ 0.9993120,
                                            .data$youth == 1  ~ 0.9889011)) |>
    dplyr::mutate(swan_tot_tscores = dplyr::case_when(.data$age18 < 12 & .data$p_respondent == 0 ~ NA,
                                                      T ~ (((.data$swan_pro - .data$swan_pred) / .data$swan_sd_pred) + .data$res_adj) / (.data$sd_adj) * 10 + 50))

  #### Inattentive Models with
  df_mod <- df_mod |>
    dplyr::mutate(swan_ia_gender_pred = -5.8020600 + 0.1024968 * .data$age18 +
                    3.4245032 * .data$female + 1.9036940 * .data$p_respondent -
                    0.1688897 * .data$age18 * .data$female - 4.8939896 * .data$female * .data$p_respondent) |>
    dplyr::mutate(swan_ia_gender_low = as.numeric((.data$swan_ia_pro - .data$swan_ia_gender_pred) < 0)) |>
    dplyr::mutate(swan_ia_gender_sd_pred = sqrt(48.549114  +
                                                  1.769995 * .data$age18 +
                                                  3.912498 * .data$female -
                                                  38.303116 *  .data$p_respondent +
                                                  45.919611 * .data$swan_ia_gender_low +
                                                  6.907424 *  .data$age18 * .data$p_respondent -
                                                  2.897395 * .data$age18 * .data$swan_ia_gender_low -
                                                  12.385480 * .data$female * .data$p_respondent))|>
    dplyr::mutate(res_adj = dplyr::case_when(.data$female == 0 & .data$youth == 0  ~ -0.047669334,
                                             .data$female == 0 & .data$youth == 1  ~ -0.001994747,
                                             .data$female == 1  & .data$youth == 0 ~ -0.052532166,
                                             .data$female ==1  & .data$youth ==1 ~ -0.011352696)) |>
    dplyr::mutate(sd_adj = dplyr::case_when(.data$female == 0 & .data$youth == 0  ~ 0.9983462,
                                            .data$female == 0 & .data$youth == 1  ~ 0.9892343,
                                            .data$female == 1 & .data$youth == 0 ~ 1.0007317,
                                            .data$female ==1  & .data$youth ==1 ~ 0.9974842)) |>
    dplyr::mutate(swan_ia_gender_tscores = dplyr::case_when(.data$age18 < 12 & .data$p_respondent == 0 ~ NA,
                                                            T ~ (((.data$swan_ia_pro - .data$swan_ia_gender_pred) / .data$swan_ia_gender_sd_pred) + .data$res_adj) / (.data$sd_adj) * 10 + 50))




  #### Inattentive across gender
  df_mod <- df_mod |>
    dplyr::mutate(swan_ia_pred = -3.7744681 - 0.6639846 * .data$p_respondent) |>
    dplyr::mutate(swan_ia_low = as.numeric((.data$swan_ia_pro - .data$swan_ia_pred) < 0)) |>
    dplyr::mutate(swan_ia_sd_pred = sqrt(43.782402 +
                                           2.444014 * .data$age18 -
                                           43.067165 * .data$p_respondent +
                                           57.574635 * .data$swan_ia_low +
                                           7.026785 * .data$age18 * .data$p_respondent -
                                           4.044906 * .data$age18 * .data$swan_ia_low)) |>
    dplyr::mutate(res_adj = dplyr::case_when(.data$youth == 0  ~ -0.050939400,
                                             .data$youth == 1  ~ -0.001064299)) |>
    dplyr::mutate(sd_adj = dplyr::case_when(.data$youth == 0  ~ 0.9993775,
                                            .data$youth == 1  ~ 0.9932011)) |>
    dplyr::mutate(swan_ia_tscores = dplyr::case_when(.data$age18 < 12 & .data$p_respondent == 0 ~ NA,
                                                     T ~ (((.data$swan_ia_pro - .data$swan_ia_pred) / .data$swan_ia_sd_pred) + .data$res_adj) / (.data$sd_adj) * 10 + 50))


  #### Hyperactive with gender
  df_mod <- df_mod |>
    dplyr::mutate(swan_hi_gender_pred = -4.17675209 - 0.04167303 * .data$age18 + 0.70229528 * .data$female + 5.13833773 *
                    .data$p_respondent - 0.35179042 * .data$age18 * .data$p_respondent - 4.35765313 * .data$female * .data$p_respondent) |>
    dplyr::mutate(swan_hi_gender_low = as.numeric((.data$swan_hi_pro - .data$swan_hi_gender_pred) < 0)) |>
    dplyr::mutate(swan_hi_gender_sd_pred = sqrt(97.2118786  -
                                                  1.6070110 * .data$age18 -
                                                  17.5869608 * .data$female -
                                                  62.4036863 * .data$p_respondent +
                                                  7.5639752 * .data$swan_hi_gender_low  +
                                                  0.5922562 * .data$age18 * .data$female +
                                                  6.4226127 * .data$age18 * .data$p_respondent +
                                                  1.3285283 * .data$age18 * .data$swan_hi_gender_low +
                                                  4.5001409  * .data$female * .data$p_respondent +
                                                  78.7787513 * .data$female * .data$swan_hi_gender_low +
                                                  19.6268534 * .data$p_respondent * .data$swan_hi_gender_low -
                                                  3.7360877 * .data$age18 * .data$female *  .data$swan_hi_gender_low -
                                                  49.7954285 * .data$female * .data$p_respondent * .data$swan_hi_gender_low))|>
    dplyr::mutate(res_adj = dplyr::case_when(.data$female == 0 & .data$youth == 0  ~ -0.07854678,
                                             .data$female == 0 & .data$youth == 1  ~ -0.07142296,
                                             .data$female == 1  & .data$youth == 0 ~ -0.08761418,
                                             .data$female ==1  & .data$youth ==1 ~ -0.09748210)) |>
    dplyr::mutate(sd_adj = dplyr::case_when(.data$female == 0 & .data$youth == 0  ~ 1.0002075,
                                            .data$female == 0 & .data$youth == 1  ~ 0.9772225,
                                            .data$female == 1  & .data$youth == 0 ~ 0.9956266,
                                            .data$female ==1  & .data$youth ==1 ~ 0.9976900)) |>
    dplyr::mutate(swan_hi_gender_tscores = dplyr::case_when(.data$age18 < 12 & .data$p_respondent == 0 ~ NA,
                                                            T ~ (((.data$swan_hi_pro - .data$swan_hi_gender_pred) / .data$swan_hi_gender_sd_pred) + .data$res_adj) / (.data$sd_adj) * 10 + 50))


  #### Hyperactive across Gender
  df_mod <- df_mod |>
    dplyr::mutate(swan_hi_pred = -3.85277159 -
                    0.03568567 * .data$age18 + 2.93347689 * .data$p_respondent -
                    0.34794969 *.data$ age18 *
                    .data$p_respondent) |>
    dplyr::mutate(swan_hi_low = as.numeric((.data$swan_hi_pro - .data$swan_hi_pred) < 0)) |>
    dplyr::mutate(swan_hi_sd_pred = sqrt(95.490458 -
                                           1.760477 * .data$age18 -
                                           63.468222 * .data$p_respondent +
                                           38.526578 * .data$swan_hi_low +
                                           6.763566 * .data$age18 * .data$p_respondent)) |>
    dplyr::mutate(res_adj = dplyr::case_when(.data$youth == 0  ~ -0.08224601,
                                             .data$youth == 1  ~ -0.08580553)) |>
    dplyr::mutate(sd_adj = dplyr::case_when(.data$youth == 0  ~ 0.9980346,
                                            .data$youth == 1  ~ 0.9868408)) |>
    dplyr::mutate(swan_hi_tscores = dplyr::case_when(.data$age18 < 12 & .data$p_respondent == 0 ~ NA,
                                                     T ~ (((.data$swan_hi_pro - .data$swan_hi_pred) / .data$swan_hi_sd_pred) + .data$res_adj) / (.data$sd_adj) * 10 + 50))

  #### Remove extra columns ####
  df_final <- df_mod |>
    dplyr::select(-dplyr::contains("pred"), -dplyr::contains("low"), -dplyr::contains("adj"))


  return(df = df_final)
}
