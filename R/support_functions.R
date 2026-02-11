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

#### Load language ####
#### Declares language upload loading the package.
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("sfsScorer has been loaded")
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
#' @param file_path Should be a path on your computer to the SWAN scores
#' @param test Which questionnaire are we running
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
#'
#' @returns A clean data frame ready for t-scores
#'
#'
clean_file <- function(file_path = NULL, test = NULL) {

  #Check to make sure the filetype is correct
  if(!rio::get_ext(file_path) %in% c('csv','xlsx','xls')){
    stop(paste0(basename(file_path),'s filetype is not usable. It must be a .csv, .xlsx, or .xls filetype. Please correct the filetype before continuing'))
  }

  # Import df
  df <- rio::import(file_path)

  # Check for Required Questions
  if(test == 'swan'){

    required_test_cols <- c('swan1','swan2','swan3','swan4','swan5','swan6','swan7','swan8','swan9',
                            'swan10','swan11','swan12','swan13','swan14','swan15','swan16','swan17','swan18')
    # swan only allows values -3 to 3
    value_range <- seq(-3, 3, 1)
  } else if(test == 'tocs'){
    required_test_cols <- paste0('tocs',seq(1,24,1))

    value_range <- seq(-3, 3, 1)
  }

  required_dem_cols <- c('age','gender','p_respondent')

  if(!all(c(required_test_cols,required_dem_cols) %in% colnames(df))){

    missing_cols <- c(required_test_cols,required_dem_cols)[which(!c(required_test_cols,required_dem_cols) %in% colnames(df))]

    stop(paste('Please check the column names in your file. The file appears to be missing the following required columns...\n',
               paste(missing_cols, collapse = ", ")))
  }

  ##### Check for impossible values ###
  df_long <- df |>
    tidyr::pivot_longer(cols = dplyr::all_of(required_test_cols)) |>
    dplyr::filter(!.data$value %in% value_range)

  if(nrow(df_long) > 0){
    stop(paste("There appear to be",nrow(df_long),"values above 3 or below -3 in the file. These are not possible in the SWAN test.",
               "Please correct or remove the rows from the filing before running the get_swan_tscores() function again."))
  }

  # P_respondent
  if(any(!df$p_respondent %in% c(0,1))) {

    stop(paste(sum(!df$p_respondent %in% c(0,1)),"of your records do not have a 1 or 0 for p_respondent.",
               "Please check that every row has a 0 or 1 for p_respondent where 1 = Parent Respondent and 0 = Child / Youth Self-Respondent.",
               "Once all rows have been corrected try running the get_swan_tscores() function again. p_respondent is reqired to generate a t-score."))
  }

  # Check
  if(!class(df$age) %in% c('numeric','integer')){
    stop(paste("It appears as though your age variable is not formatted as a number. Please remove any non-numeric characters from your age column.",
               "Once all rows have been corrected try running the get_swan_tscores() function again. age is reqired to generate a t-score."))
  }

  # Check that Age is formatted correctly
  if(any(df$age >= 19)){
    stop(paste("Some of your records have an age of 19 or above. T-scores are applicable only for individuals aged 5-18.",
               "Please check that ages are correct and remove any records 19 or above before running the get_swan_tscores() function again."))
  }

  if(any(df$age < 5)){
    stop(paste("Some of your records have an age below 5. T-scores are applicable only for individuals aged 5-18.",
               "Please check that ages are correct and remove any records below 5 before running the get_swan_tscores() function again."))
  }

  # Check gender
  if(!any(as.character(unique(df$gender)) %in% c('1','2'))){
    warning(paste("The package wasn't able to find any gender coded as 1 or 2, and therefore won't generate any gender-based t-scores. If you'd like to generate gender-based t-scores, gender should be coded as... \n",
                  "1 = Boy \n",
                  "2 = Girl \n"))
  }


  return(df = df)
}




#' @name mkvars
#'
#' @title Make Variables - Subset SWAN to subdomains.
#'
#' @author Annie
#'
#' @description Pass the root of the test with the question numbers to subset the SWAN. 1-9 = Inattentive. 10-18 = Hyperactive.
#' Function to list all questionnaire items (and not have to type them out) - used throughout
#' a:  first item number (usually "1", but when referencing subdomains, or for SWAN ODD, first item may be something other than 1
#' b:  last item number
#' root:  part of the item name that doesn't change (eg:  for swan1 to swan18, the root is "swan" )
#'
#' @importFrom stringr str_c
#'
#' @param a First question of subset
#' @param b Last question of subset
#' @param root Root name of
#'
#' @returns A data frame ready for use or an error
mkvars <- function(a = NULL, b = NULL, root = 'swan') {
  cnams <- NULL
  for (i in a:b) {
    cnams[i - a + 1] <- stringr::str_c(root, i)
  }
  return(cnams)
}

#' @name mkpro
#'
#' @title Make Prorated Scores
#'
#' @author Annie
#'
#' @description
#' a: first item number in questionnaire (usually "1")
# b: last item number in questionnaire
# root: non numeric part of the item name (see calls to the function below)
# maxmiss: minimum number of missing values that sets total and prorated total to missing
#          WARNING: default does not set any totals or pro-rated totals to missing to leave this up to the individual analyst
# dat: name of data frame with items - default is S2quest
# newroot: root for new variable names if different (used for subdomain totals) - default if newroot is not specified in call to function, newroot = root
#'
#' @param dat should be a data.frame from [clean_file()]
#' @param maxmiss maximum number of missing values before can be considered invalid
#' @inheritParams mkvars
#' @param newroot a new name if root names need to be changed
#'
#' @returns A data frame ready for use or an error
#'
mkpro <- function(maxmiss = NA, dat = NA, a = NULL, b = NULL, root = 'swan', newroot = 'swan' ) {


  cnams <- mkvars(a, b, root)
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

