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


#' @name score_tocs2
#'
#' @title Run analysis on TOCS raw values to return t-scores
#'
#' @description score_tocs2() returns gendered and non-gendered t-scores for the [Toronto Obsessive-Compulsive Scale (TOCS) assessment](https://pubmed.ncbi.nlm.nih.gov/27015722/)
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param df If you already have the TOCS-2 data in your R environment, pass the dataframe to this parameter
#' @param file If you prefer scoring a spreadsheet...
#' \enumerate{
#'  \item Change to `TRUE` to pop-up a finder to allow you select a file. Alternatively, leave df and file empty to pop-up a finder.
#'  \item Or specify a pathway
#'  }
#' @param max_missing By default, 0 items are allowed to be missing on the TOCS. Any questionnaire with 1 or more missing, will not be scored. If you'd like to adjust this number, change the max_missing value.
#'  This will use a prorated score to generate t-scores. Please be aware that missingness can induce issues when analyzing.
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
#' @importFrom cli cli_alert_success
#'
#' @returns table with t-scores attached to raw swan values
#'
#' @examples
#' # Read in the file of scores
#' # This is an example file
#' csv <- system.file("extdata", "sample_tocs.csv", package = "sfsScorer")
#'
#' # Score via the file parameter
#' scores_csv <- score_tocs2(file = csv)
#'
#' # Score via the df paramter
#' df <- rio::import(csv)
#' scores_csv <- score_tocs2(df = df)
#'
#' # The data are automatically validated.
#' # To ignore the validation errors and introduce `NA`, set `ignore_check = TRUE`
#' df_mod <- df |>
#'   dplyr::mutate(p_respondent = 2)
#' scores_csv <- score_tocs2(df = df_mod, ignore_check = TRUE)
#'
#' @export
#'
#'

score_tocs2 <- function(df = NULL, file = FALSE, output_folder = NULL,
                             max_missing = 0, ignore_check = FALSE) {

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
  check <- clean_file(df, test = 'tocs', ignore_check = ignore_check)

  # Summarize Scores
  summary <- build_summary_tocs(check, max_missing = max_missing)

  # Run the model
  score <- run_model_tocs(summary)

  # Print a summary in the console
  cli::cli_alert_success(paste0("The model scored ",sum(!is.na(score$tocs_tscores))," observations."))
  if(sum(score$tocs_miss > 0)){
    cli::cli_alert_warning(paste0(sum(score$tocs_miss > 0)," observations were not scored due to excessive missingness. ",
                                ifelse(max_missing == 0, '0 questions are allowed to be missing.',
                                       paste0('The allowed missingness was changed to ',max_missing,'. We recommend not allowing any missingess'))))
  }

  print(
    score |>
      dplyr::group_by(.data$gender, .data$youth, .data$p_respondent) |>
      dplyr::summarise(n = dplyr::n(),
                       mean = mean(.data$tocs_gender_tscores, na.rm = T),
                       sd = stats::sd(.data$tocs_gender_tscores, na.rm = T))
  )

  score <- score |>
    dplyr::select(-c('age18','youth','female'))

  # Save file if specified
  if(!is.null(output_folder)){

    rio::export(score,
                file.path(output_folder,paste0('tocs_scored_',format(lubridate::now(), format='%Y-%m-%d %H-%M-%S'),'.csv')))

    cli::cli_alert_success(paste("A spreadsheet of your scored SWAN tests has been saved to {.file",output_folder,"}"))

  }

  return(score = score)


}


#' @name build_summary_tocs
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
#' @param max_missing max_missing is passed from the [score_tocs2()] function. By default, the tocs allows 0 missing items.
#'
#' @returns A data frame with all of the totals columns
#'
build_summary_tocs <- function(df = NULL, max_missing = NULL) {


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
                                           T ~ NA))

  #Whole test scores
  df_tot <- cbind(df_tot, mkpro(dat = df_tot, a = 1, b = 24, maxmiss = max_missing, root = 'tocs', newroot = 'tocs'))



  return(df_tot = df_tot)

}

#' @name run_model_tocs
#'
#' @title Runs the model that adjusts for time and create t-scores
#'
#' @description This is the generic model used to create t-scores. It adjusts for gender, age, respondent, and time.
#' It is best used in all cases unless a team is trying to look at values over time
#' Use the dataframe from [build_summary_tocs()] to produce t-scores
#'
#' @param df should be a data.frame from [build_summary_tocs()]
#'
#' @returns A data frame with t-scores
#'
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr contains
#' @importFrom rlang .data
#'
run_model_tocs <- function(df = NULL) {


  #### Produce t-scores with gender
  df_mod <- df |>
    dplyr::mutate(tocs_gender_pred = -18.4550303 + 0.8571027 *
                    .data$age18 + 3.0678664 *
                    .data$female + 2.0479330 *
                    .data$p_respondent +  0.3759008 *
                    .data$age18 *
                    .data$female - 0.6196103 *
                    .data$age18 *
                    .data$p_respondent - 5.6892318 *
                    .data$female *
                    .data$p_respondent) |>
    dplyr::mutate(tocs_gender_low = as.numeric((.data$tocs_pro - .data$tocs_gender_pred) < 0)) |>
    dplyr::mutate(tocs_gender_sd_pred = sqrt(290.769936 +  6.297301 *
                                               .data$age18 + 317.752045 *
                                               .data$female + 42.555670 *
                                               .data$p_respondent + 340.503643 *
                                               .data$tocs_gender_low - 23.323885 *
                                               .data$age18 *
                                               .data$female - 8.317498 *
                                               .data$age18 *
                                               .data$p_respondent - 12.091091 *
                                               .data$age18 *
                                               .data$tocs_gender_low - 420.979110 *
                                               .data$female *
                                               .data$p_respondent + 479.98711 *
                                               .data$p_respondent *
                                               .data$tocs_gender_low + 32.203872 *
                                               .data$age18 *
                                               .data$female *
                                               .data$p_respondent)) |>
    dplyr::mutate(res_adj = dplyr::case_when(.data$female == 0 & .data$youth == 0  ~ -0.2412086,
                                             .data$ female == 0 & .data$youth == 1  ~ -0.1459772,
                                             .data$female == 1  & .data$youth == 0 ~ -0.2556146,
                                             .data$ female ==1  & .data$youth ==1 ~ -0.1158219)) |>
    dplyr::mutate(sd_adj = dplyr::case_when(.data$female == 0 & .data$youth == 0  ~ 0.9495748,
                                            .data$female == 0 & .data$youth == 1  ~ 0.9702869,
                                            .data$female == 1  & .data$youth == 0 ~ 0.9461211,
                                            .data$female ==1  & .data$youth ==1 ~ 1.0067446)) |>
    dplyr::mutate(tocs_gender_tscores = dplyr::case_when(.data$age18 < 12 & .data$p_respondent == 0 ~ NA,
                                                             T ~ round((((.data$tocs_pro - .data$tocs_gender_pred) / .data$tocs_gender_sd_pred) +.data$ res_adj) / (.data$sd_adj) * 10 + 50, digits = 5)))

  #### Full Test Across Gender
  df_mod <- df_mod |>
    dplyr::mutate(tocs_pred = -20.7163807 + 1.3408502 *
                    .data$age18 + 3.0621229 *
                    .data$p_respondent - 0.9237466 *
                    .data$age18 *
                    .data$p_respondent) |>
    dplyr::mutate(tocs_low = as.numeric((.data$tocs_pro - .data$tocs_pred) < 0)) |>
    dplyr::mutate(tocs_sd_pred = sqrt(358.225292 + 1.349141 *
                                        .data$age18 - 66.973190 *
                                        .data$p_respondent + 341.077714 *
                                        .data$tocs_low - 11.102991 *
                                        .data$age18 *
                                        .data$tocs_low + 372.550807 *
                                        .data$p_respondent *
                                        .data$tocs_low
    )) |>
    dplyr::mutate(res_adj = dplyr::case_when(.data$youth == 0  ~ -0.2326041,
                                             .data$youth == 1  ~ -0.1265345)) |>
    dplyr::mutate(sd_adj = dplyr::case_when(.data$youth == 0  ~ 0.9701794,
                                            .data$youth == 1  ~ 0.9991996)) |>
    dplyr::mutate(tocs_tscores = dplyr::case_when(.data$age18 < 12 & .data$p_respondent == 0 ~ NA,
                                                      T ~ round((((.data$tocs_pro - .data$tocs_pred) / .data$tocs_sd_pred) + .data$res_adj) / (.data$sd_adj) * 10 + 50, digits = 5)))


  #### Remove extra columns ####
  df_final <- df_mod |>
    dplyr::select(-dplyr::contains("pred"), -dplyr::contains("low"), -dplyr::contains("adj"))



  return(df = df_final)
}
