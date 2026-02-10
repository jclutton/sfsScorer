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


#' @name get_swan_tscores
#'
#' @title Run analysis on SWAN raw values to return t-scores
#'
#' @description get_swan_tscores() returns gendered and non-gendered t-scores for the Strengths and Weaknesses of ADHD Symptoms and Normal Behavior Rating Scale (SWAN) assessment
#'
#' @param file Pathway to formatted raw SWAN scores. If left blank file finder will pop up to allow you to select the file.
#' @param output_folder Output file pathway
#'  \enumerate{
#'  \item Leave blank - This will output a csv file with the t-scores to your working directory
#'  \item Specify a pathway - This will output a csv file to the specified pathway
#'  \item Set to `NULL` - This will not output a csv file
#'  }
#'
#' @importFrom rio export
#' @importFrom lubridate now
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom stringr str_replace_all
#' @importFrom stats sd
#' @importFrom here here
#'
#' @returns table with t-scores attached to raw swan values
#'
#' @export
#'
#'

get_swan_tscores <- function(file = NULL, output_folder = here::here()) {

  if(is.null(file)){
    file <- file.choose()
  }

  # Run QC checks on data
  check <- clean_file(file)

  # Summarize Scores
  summary <- build_summary(check)

  # Run the model
  score <- run_model(summary)

  # Print a summary in the console
  message(paste0("The model scored ",sum(!is.na(score$swan_tot_gender_tscores))," observations. \n \n",
                 sum(score$swan_ia_miss > 1 | score$swan_hi_miss > 1)," observations were not scored due to excessive missingness. ",
                 "Only one question can be missing per subdomain."))
  print(
    score |>
      dplyr::group_by(gender, youth, p_respondent) |>
      dplyr::summarise(n = dplyr::n(),
                       mean = mean(swan_tot_gender_tscores, na.rm = T),
                       sd = sd(swan_tot_gender_tscores, na.rm = T))
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
