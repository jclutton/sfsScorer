#     Copyright (C) @ 2025 SickKids
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
#' @param df If you already have the SWAN data in your R environment, pass the dataframe to this parameter
#' @param file If you prefer scoring a spreadsheet...
#' \enumerate{
#'  \item Change to `TRUE` to pop-up a finder to allow you select a file. Alternatively, leave df and file empty to pop-up a finder.
#'  \item Or specify a pathway
#'  }
#' @param output_folder Optional, output file pathway. Defauts to `NULL`. Specify a pathway to output a csv file.
#' @param ignore_check Data are validated to look for missing or improperly formatted values before scoring. Errors are thrown when data aren't valid; however, this can cause issues
#' in real data sets where data vary for good reasons. To skip the validation process, set ignore_check to `TRUE`. NAs will be returned where data are invalid
#'
#' @importFrom rio export
#' @importFrom lubridate now
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom stringr str_replace_all
#' @importFrom stats sd
#' @importFrom here here
#' @importFrom rlang .data
#' @importFrom cli cli_alert_success
#'
#' @returns table with t-scores attached to raw swan values
#'
#' @examples
#' # Read in the file of scores
#' # This is an example file
#' csv <- system.file("extdata", "sample_swan.csv", package = "sfsScorer")
#'
#' # Score via the file parameter
#' scores_csv <- score_swan(file = csv)
#'
#' # Score via the df paramter
#' df <- rio::import(csv)
#' scores_csv <- score_swan(df = df)
#'
#' # The data are automatically validated.
#' # To ignore the validation errors and introduce `NA`, set `ignore_check = TRUE`
#' df_mod <- df |>
#'   dplyr::mutate(swan1 = 6)
#' scores_csv <- score_swan(df = df_mod, ignore_check = TRUE)
#'
#'
#' @export
#'
#'

score_swan <- function(df = NULL, file = FALSE, output_folder = NULL, ignore_check = FALSE) {

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
  check <- clean_file(df, test = 'swan', ignore_check = ignore_check)

  # Summarize Scores
  summary <- build_summary_swan(check)

  # Run the model
  score <- run_model_swan(summary)

  # Print a summary in the console
  cli::cli_alert_success(paste0("The model scored ",sum(!is.na(score$swan_tot_tscores))," observations."))
  if(sum(score$swan_ia_miss > 1 | score$swan_hi_miss > 1) > 0){
    cli::cli_alert_warning(paste0(sum(score$swan_ia_miss > 1 | score$swan_hi_miss > 1)," observations were not scored due to excessive missingness. ",
                                  "Only one question can be missing per subdomain."))
  }

  print(
    score |>
      dplyr::group_by(.data$gender, .data$youth, .data$p_respondent) |>
      dplyr::summarise(n = dplyr::n(),
                       mean = mean(.data$swan_tot_gender_tscores, na.rm = T),
                       sd = stats::sd(.data$swan_tot_gender_tscores, na.rm = T))
  )

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
#' @description Use the dataframe from [clean_file()] and the [mkpro()] function to reverse scores, then
#' calculate totals, missingness, and pro-rated totals for the total test and subdomains
#'
#' @import dplyr
#'
#' @importFrom rlang .data
#'
#' @param df should be a data.frame from [clean_file()]
#'
#' @returns A data frame with all of the totals columns
#'
build_summary_swan <- function(df = NULL) {

  ia_subdomain <- mkvars(1,9, 'swan')
  hi_subdomain <- mkvars(10, 18, 'swan')

  df_tot <- df |>
    dplyr::mutate(age18 = dplyr::case_when(.data$age < 18 ~ age,
                                           .data$age >= 18 ~ 18,
                                           T ~ .data$age)) |>
    # Use same codings as Annie's script
    dplyr::mutate(female = dplyr::case_when(as.character(.data$gender) == "1" ~ 0,
                                            as.character(.data$gender) == "2" ~ 1,
                                            T ~ NA)) |>
    dplyr::mutate(youth = dplyr::case_when(.data$age < 12 ~ 0,
                                           .data$age >= 12 ~ 1,
                                           T ~ NA)) |>

    # Reverse scores
    dplyr::mutate(dplyr::across(dplyr::all_of(c(ia_subdomain, hi_subdomain)),
                                ~-1*.x))

  #Inattentive
  df_tot <- cbind(df_tot, mkpro(dat = df_tot, a = 1, b = 9, newroot = 'swan_ia', maxmiss = 1))

  #Hyperactive
  df_tot <- cbind(df_tot, mkpro(dat = df_tot, a = 10, b = 18, newroot = 'swan_hi', maxmiss = 1))

  #Whole test scores
  df_tot <- cbind(df_tot, mkpro(dat = df_tot, a = 1, b = 18)) |>
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

  ia_subdomain <- mkvars(1,9, 'swan')
  hi_subdomain <- mkvars(10, 18, 'swan')

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
    dplyr::select(-dplyr::contains("pred"), -dplyr::contains("low"), -dplyr::contains("adj")) |>
    # Reverse scores back to initial input
    dplyr::mutate(dplyr::across(dplyr::all_of(c(ia_subdomain, hi_subdomain)),
                                ~-1*.x)) |>
    # Print columns with reversed scores
    dplyr::mutate(dplyr::across(dplyr::all_of(c(ia_subdomain, hi_subdomain)),
                                ~-1*.x,
                                .names = "{col}_reversed"))



  return(df = df_final)
}
