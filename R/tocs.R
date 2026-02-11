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


#' @name get_tocs_tscores
#'
#' @title Run analysis on TOCS raw values to return t-scores
#'
#' @description get_tocs_tscores() returns gendered and non-gendered t-scores for the [Toronto Obsessive-Compulsive Scale (TOCS) assessment](https://pubmed.ncbi.nlm.nih.gov/27015722/)
#'
#' @param file Pathway to formatted raw scores. If left blank file finder will pop up to allow you to select the file.
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

get_tocs_tscores <- function(file = NULL, output_folder = here::here()) {

  if(is.null(file)){
    file <- file.choose()
  }

  # Run QC checks on data
  check <- clean_file(file)


}
