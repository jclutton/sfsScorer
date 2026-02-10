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
globalVariables(c("value",'swan_tot_gender_tscores','sd',
                  'swan1','swan2','swan3','swan4','swan5','swan6','swan7','swan8','swan9',
                  'swan10','swan11','swan12','swan13','swan14','swan15','swan16','swan17','swan18',
                  'age','gender','p_respondent',
                  'age18','female','youth','p_respondent',
                  'type',
                  "swan_gender_low", "swan_gender_pred", "swan_hi_gender_low",
                  "swan_hi_gender_pred", "swan_hi_low", "swan_hi_pred", "swan_hi_pro",
                  "swan_ia_gender_low", "swan_ia_gender_pred", "swan_ia_low", "swan_ia_pred",
                  "swan_ia_pro", "swan_low", "swan_pred", "swan_pro"))


#### clean_file function ####
#' @name clean_file
#'
#' @title Clean File
#'
#' @description This function checks that the uploaded SWAN score file does not have any issues with the SWAN
#' values or the gender and age coding
#'
#' @param file_path Should be a path on your computer to the SWAN scores
#'
#' @importFrom rio import
#' @importFrom rio get_ext
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter
#' @importFrom dplyr anti_join
#' @importFrom dpyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr rename
#'
#' @returns A clean data frame ready for t-scores
#'
#'
clean_file <- function(file_path = NULL) {

  #Check to make sure the filetype is correct
  if(!rio::get_ext(file_path) %in% c('csv','xlsx','xls')){
    stop(paste0(basename(file_path),'s filetype is not usable. It must be a .csv, .xlsx, or .xls filetype. Please correct the filetype before continuing'))
  }

  # Import SWAN Scores
  df <- rio::import(file_path)

  # Check for Required Questions
  required_test_cols <- c('swan1','swan2','swan3','swan4','swan5','swan6','swan7','swan8','swan9',
                          'swan10','swan11','swan12','swan13','swan14','swan15','swan16','swan17','swan18')

  required_dem_cols <- c('age','gender','p_respondent')

  if(!all(c(required_test_cols,required_dem_cols) %in% colnames(df))){

    missing_cols <- c(required_test_cols,required_dem_cols)[which(!c(required_test_cols,required_dem_cols) %in% colnames(df))]

    stop(paste('Please check the column names in your file. The file appears to be missing the following required columns...\n',
               paste(missing_cols, collapse = ", ")))
  }

  ##### Check for impossible values ###
  df_long <- df |>
    tidyr::pivot_longer(cols = dplyr::all_of(required_test_cols)) |>
    dplyr::filter(!value %in% c(-3,-2,-1,0,1,2,3))

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

#' @name build_summary
#'
#' @title Build Totals and Prorated Totals for Full Test and Subdomains
#'
#' @description Use the dataframe from [clean_file()] and the [mkpro()] function to reverse scores, then
#' calculate totals, missingness, and pro-rated totals for the total test and subdomains
#'
#' @import dplyr
#'
#' @param df should be a data.frame from [clean_file()]
#'
#' @returns A data frame with all of the totals columns
#'
build_summary <- function(df = NULL) {

  ia_subdomain <- mkvars(1,9, 'swan')
  hi_subdomain <- mkvars(10, 18, 'swan')

  df_tot <- df |>
    dplyr::mutate(age18 = dplyr::case_when(age < 18 ~ age,
                                           age >= 18 ~ 18,
                                           T ~ age)) |>
    # Use same codings as Annie's script
    dplyr::mutate(female = dplyr::case_when(as.character(gender) == "1" ~ 0,
                                            as.character(gender) == "2" ~ 1,
                                            T ~ NA)) |>
    dplyr::mutate(youth = dplyr::case_when(age < 12 ~ 0,
                                           age >= 12 ~ 1,
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
                                ~ dplyr::case_when(swan_ia_miss > 1 | swan_hi_miss > 1 ~ NA,
                                                   T ~ .)))




  return(df_tot = df_tot)

}

#' @name run_model
#'
#' @title Runs the model that adjusts for time and create t-scores
#'
#' @description This is the generic model used to create t-scores. It adjusts for gender, age, respondent, and time.
#' It is best used in all cases unless a team is trying to look at values over time
#' Use the dataframe from [build_summary()] to produce t-scores
#'
#' @param df should be a data.frame from [build_summary()]
#'
#' @returns A data frame with t-scores
#'
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr contains
#'
run_model <- function(df = NULL) {

  ia_subdomain <- mkvars(1,9, 'swan')
  hi_subdomain <- mkvars(10, 18, 'swan')

  #### Produce t-scores with gender
  df_mod <- df |>
    dplyr::mutate(swan_gender_pred = -4.0630359 - 0.3384133  * age18 + 1.7004264 * female + 1.5455007 *
                    p_respondent - 8.3141252 * female * p_respondent) |>
    dplyr::mutate(swan_gender_low = as.numeric((swan_pro - swan_gender_pred) < 0)) |>
    dplyr::mutate(swan_gender_sd_pred = sqrt(325.95663 -
                                               7.12465 * age18 +
                                               13.4144 * female -
                                               229.07860 * p_respondent +
                                               106.69317 * swan_gender_low  +
                                               29.39191 * age18 * p_respondent -
                                               44.74060 * female * p_respondent)) |>
    dplyr::mutate(res_adj = dplyr::case_when(female == 0 & youth == 0  ~ -0.06113885,
                                             female == 0 & youth == 1  ~ -0.06736433,
                                             female == 1  & youth == 0 ~ -0.07421668,
                                             female ==1  & youth ==1 ~ -0.06374547)) |>
    dplyr::mutate(sd_adj = dplyr::case_when(female == 0 & youth == 0  ~ 0.9986281,
                                            female == 0 & youth == 1  ~ 0.9879085,
                                            female == 1  & youth == 0 ~ 0.9976924,
                                            female ==1  & youth ==1 ~ 0.9972819)) |>
    dplyr::mutate(swan_tot_gender_tscores = dplyr::case_when(age18 < 12 & p_respondent == 0 ~ NA,
                                                             T ~ (((swan_pro - swan_gender_pred) / swan_gender_sd_pred) + res_adj) / (sd_adj) * 10 + 50))

  #### Full Test Across Gender
  df_mod <- df_mod |>
    dplyr::mutate(swan_pred = -3.3512518 - 0.3206639 * age18 - 2.5708190 * p_respondent) |>
    dplyr::mutate(swan_low = as.numeric((swan_pro - swan_pred) < 0)) |>
    dplyr::mutate(swan_sd_pred = sqrt(314.405841 -
                                        4.962281 * age18 -
                                        252.382387 * p_respondent +
                                        156.755038 * swan_low +
                                        30.436026 * age18 * p_respondent -
                                        5.112260 * age18 * swan_low)) |>
    dplyr::mutate(res_adj = dplyr::case_when(youth == 0  ~ -0.06810488,
                                             youth == 1  ~ -0.05023800)) |>
    dplyr::mutate(sd_adj = dplyr::case_when(youth == 0  ~ 0.9993120,
                                            youth == 1  ~ 0.9889011)) |>
    dplyr::mutate(swan_tot_tscores = dplyr::case_when(age18 < 12 & p_respondent == 0 ~ NA,
                                                      T ~ (((swan_pro - swan_pred) / swan_sd_pred) + res_adj) / (sd_adj) * 10 + 50))

  #### Inattentive Models with
  df_mod <- df_mod |>
    dplyr::mutate(swan_ia_gender_pred = 5.8020600 + 0.1024968 * age18 +
                    3.4245032 * female + 1.9036940 * p_respondent -
                    0.1688897 * age18 * female - 4.8939896 * female * p_respondent) |>
    dplyr::mutate(swan_ia_gender_low = as.numeric((swan_ia_pro - swan_ia_gender_pred) < 0)) |>
    dplyr::mutate(swan_ia_gender_sd_pred = sqrt(48.549114  +
                                                  1.769995 * age18 +
                                                  3.912498 * female -
                                                  38.303116 *  p_respondent +
                                                  45.919611 * swan_ia_gender_low +
                                                  6.907424 *  age18 * p_respondent -
                                                  2.897395 * age18 * swan_ia_gender_low -
                                                  12.385480 * female * p_respondent))|>
    dplyr::mutate(res_adj = dplyr::case_when(female == 0 & youth == 0  ~ -0.047669334,
                                             female == 0 & youth == 1  ~ -0.001994747,
                                             female == 1  & youth == 0 ~ -0.052532166,
                                             female ==1  & youth ==1 ~ -0.011352696)) |>
    dplyr::mutate(sd_adj = dplyr::case_when(female == 0 & youth == 0  ~ 0.9983462,
                                            female == 0 & youth == 1  ~ 0.9892343,
                                            female == 1  & youth == 0 ~ 1.0007317,
                                            female ==1  & youth ==1 ~ 0.9974842)) |>
    dplyr::mutate(swan_ia_gender_tscores = dplyr::case_when(age18 < 12 & p_respondent == 0 ~ NA,
                                                            T ~ (((swan_ia_pro - swan_ia_gender_pred) / swan_ia_gender_sd_pred) + res_adj) / (sd_adj) * 10 + 50))




  #### Inattentive across gender
  df_mod <- df_mod |>
    dplyr::mutate(swan_ia_pred = -3.7744681 - 0.6639846 * p_respondent) |>
    dplyr::mutate(swan_ia_low = as.numeric((swan_ia_pro - swan_ia_pred) < 0)) |>
    dplyr::mutate(swan_ia_sd_pred = sqrt(43.782402 +
                                           2.444014 * age18 -
                                           43.067165 * p_respondent +
                                           57.574635 * swan_ia_low +
                                           7.026785 * age18 * p_respondent -
                                           4.044906 * age18 * swan_ia_low)) |>
    dplyr::mutate(res_adj = dplyr::case_when(youth == 0  ~ -0.050939400,
                                             youth == 1  ~ -0.001064299)) |>
    dplyr::mutate(sd_adj = dplyr::case_when(youth == 0  ~ 0.9993775,
                                            youth == 1  ~ 0.9932011)) |>
    dplyr::mutate(swan_ia_tscores = dplyr::case_when(age18 < 12 & p_respondent == 0 ~ NA,
                                                     T ~ (((swan_ia_pro - swan_ia_pred) / swan_ia_sd_pred) + res_adj) / (sd_adj) * 10 + 50))


  #### Hyperactive with gender
  df_mod <- df_mod |>
    dplyr::mutate(swan_hi_gender_pred = -4.17675209 - 0.04167303 * age18 + 0.70229528 * female + 5.13833773 *
                    p_respondent - 0.35179042 * age18 * p_respondent - 4.35765313 * female * p_respondent) |>
    dplyr::mutate(swan_hi_gender_low = as.numeric((swan_hi_pro - swan_hi_gender_pred) < 0)) |>
    dplyr::mutate(swan_hi_gender_sd_pred = sqrt(97.2118786  -
                                                  1.6070110 * age18 -
                                                  17.5869608 * female -
                                                  62.4036863 * p_respondent +
                                                  7.5639752 * swan_hi_gender_low  +
                                                  0.5922562 * age18 * female +
                                                  6.4226127 * age18 * p_respondent +
                                                  1.3285283 * age18 * swan_hi_gender_low +
                                                  4.5001409  * female * p_respondent +
                                                  78.7787513 * female * swan_hi_gender_low +
                                                  19.6268534 * p_respondent * swan_hi_gender_low -
                                                  3.7360877 * age18 * female *  swan_hi_gender_low -
                                                  49.7954285 * female * p_respondent * swan_hi_gender_low))|>
    dplyr::mutate(res_adj = dplyr::case_when(female == 0 & youth == 0  ~ -0.07854678,
                                             female == 0 & youth == 1  ~ -0.07142296,
                                             female == 1  & youth == 0 ~ -0.08761418,
                                             female ==1  & youth ==1 ~ -0.09748210)) |>
    dplyr::mutate(sd_adj = dplyr::case_when(female == 0 & youth == 0  ~ 1.0002075,
                                            female == 0 & youth == 1  ~ 0.9772225,
                                            female == 1  & youth == 0 ~ 0.9956266,
                                            female ==1  & youth ==1 ~ 0.9976900)) |>
    dplyr::mutate(swan_hi_gender_tscores = dplyr::case_when(age18 < 12 & p_respondent == 0 ~ NA,
                                                            T ~ (((swan_hi_pro - swan_hi_gender_pred) / swan_hi_gender_sd_pred) + res_adj) / (sd_adj) * 10 + 50))


  #### Hyperactive across Gender
  df_mod <- df_mod |>
    dplyr::mutate(swan_hi_pred = -3.85277159 -
                    0.03568567 * age18 + 2.93347689 * p_respondent -
                    0.34794969 * age18 *
                    p_respondent) |>
    dplyr::mutate(swan_hi_low = as.numeric((swan_hi_pro - swan_hi_pred) < 0)) |>
    dplyr::mutate(swan_hi_sd_pred = sqrt(95.490458 -
                                           1.760477 * age18 -
                                           63.468222 * p_respondent +
                                           38.526578 * swan_hi_low +
                                           6.763566 * age18 * p_respondent)) |>
    dplyr::mutate(res_adj = dplyr::case_when(youth == 0  ~ -0.08224601,
                                             youth == 1  ~ -0.08580553)) |>
    dplyr::mutate(sd_adj = dplyr::case_when(youth == 0  ~ 0.9980346,
                                            youth == 1  ~ 0.9868408)) |>
    dplyr::mutate(swan_hi_tscores = dplyr::case_when(age18 < 12 & p_respondent == 0 ~ NA,
                                                     T ~ (((swan_hi_pro - swan_hi_pred) / swan_hi_sd_pred) + res_adj) / (sd_adj) * 10 + 50))

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

#' @name run_model_old
#'
#' @title An old version of the model, initially written in Annie's language
#'
#' @description This is the generic model used to create t-scores. It adjusts for gender, age, respondent, and time.
#' It is best used in all cases unless a team is trying to look at values over time
#' Use the dataframe from [build_summary()] to produce t-scores
#'
#' @param df should be a data.frame from [build_summary()]
#'
#' @returns A data frame with t-scores
#'
#' @importFrom dplyr case_when
#'
run_model_old <- function(df = NULL) {

  #### Produce t-scores with gender
  swan_gender_pred <-
    -4.0630359 - 0.3384133  * df$age18 + 1.7004264 * df$female + 1.5455007 *
    df$p_respondent - 8.3141252 * df$female * df$p_respondent

  swan_gender_low <- as.numeric((df$swan_pro - swan_gender_pred) < 0)

  swan_gender_sd_pred <- sqrt(
    325.95663 - 7.12465 * df$age18 + 13.4144 * df$female -
      229.07860 *
      df$p_respondent + 106.69317 * swan_gender_low  + 29.39191 * df$age18 *
      df$p_respondent - 44.74060 * df$female * df$p_respondent
  )

  res_adj <-
    dplyr::case_when(
      !(df$female) &
        !(df$youth)  ~ -0.06113885,
      !(df$female) &
        as.logical(df$youth)  ~ -0.06736433,
      as.logical(df$female)  &
        !(df$youth) ~ -0.07421668,
      as.logical(df$female)  &
        as.logical(df$youth) ~ -0.06374547
    )

  sd_adj <-
    dplyr::case_when(
      !(df$female) &
        !(df$youth)  ~ 0.9986281,
      !(df$female) &
        as.logical(df$youth)  ~ 0.9879085,
      as.logical(df$female)  &
        !(df$youth) ~ 0.9976924,
      as.logical(df$female)  &
        as.logical(df$youth) ~ 0.9972819
    )

  df$swan_tot_gender_tscores <- (((df$swan_pro - swan_gender_pred) / swan_gender_sd_pred
  ) + res_adj) / (sd_adj) * 10 + 50

  df$swan_tot_gender_tscores <-
    ifelse((df$age18 < 12) &
             (df$p_respondent == 0),
           NA,
           df$swan_tot_gender_tscores
    )

  #### Full Test Across Gender

  swan_pred <- -3.3512518 - 0.3206639 * df$age18 - 2.5708190 * df$p_respondent

  swan_low <- as.numeric((df$swan_pro - swan_pred) < 0)

  swan_sd_pred <- sqrt(
    314.405841 - 4.962281 * df$age18 - 252.382387 * df$p_respondent + 156.755038 *
      swan_low + 30.436026 * df$age18 * df$p_respondent -
      5.112260 * df$age18 * swan_low
  )

  res_adj <-
    dplyr::case_when(
      !(df$youth)  ~ -0.06810488,
      as.logical(df$youth)  ~ -0.05023800
    )
  sd_adj <-
    dplyr::case_when(
      !(df$youth)  ~ 0.9993120,
      as.logical(df$youth)  ~ 0.9889011
    )

  df$swan_tot_tscores <- (((df$swan_pro - swan_pred) / swan_sd_pred) + res_adj) / (sd_adj) * 10 + 50

  df$swan_tot_tscores <-
    ifelse((df$age18 < 12) &
             (df$p_respondent == 0),
           NA,
           df$swan_tot_tscores
    )

  #### Inattentive Models with gender
  swan_ia_gender_pred <- -5.8020600 + 0.1024968 * df$age18 +
    3.4245032 * df$female + 1.9036940 * df$p_respondent -
    0.1688897 * df$age18 * df$female - 4.8939896 * df$female * df$p_respondent

  swan_ia_gender_low <- as.numeric((df$swan_ia_pro - swan_ia_gender_pred) < 0)

  swan_ia_gender_sd_pred <- sqrt(
    48.549114  + 1.769995 * df$age18 + 3.912498 * df$female -
      38.303116 *  df$p_respondent + 45.919611 * swan_ia_gender_low +
      6.907424 *  df$age18 * df$p_respondent -
      2.897395 * df$age18 * swan_ia_gender_low -
      12.385480 * df$female * df$p_respondent
  )

  res_adj <-
    dplyr::case_when(
      !(df$female) &
        !(df$youth)  ~ -0.047669334,
      !(df$female) &
        as.logical(df$youth)  ~ -0.001994747,
      as.logical(df$female)  &
        !(df$youth) ~ -0.052532166,
      as.logical(df$female)  &
        as.logical(df$youth) ~ -0.011352696
    )
  sd_adj <-
    dplyr::case_when(
      !(df$female) &
        !(df$youth)  ~ 0.9983462,
      !(df$female) &
        as.logical(df$youth)  ~ 0.9892343,
      as.logical(df$female)  &
        !(df$youth) ~ 1.0007317,
      as.logical(df$female)  &
        as.logical(df$youth) ~ 0.9974842
    )

  df$swan_ia_gender_tscores <- (((df$swan_ia_pro - swan_ia_gender_pred) / swan_ia_gender_sd_pred
  ) + res_adj) / (sd_adj) * 10 + 50

  df$swan_ia_gender_tscores <-
    ifelse((df$age18 < 12) &
             (df$p_respondent == 0),
           NA,
           df$swan_ia_gender_tscores
    )

  #### Inattentive across gender
  swan_ia_pred <- -3.7744681 - 0.6639846 * df$p_respondent

  swan_ia_low <- as.numeric((df$swan_ia_pro - swan_ia_pred) < 0)

  swan_ia_sd_pred <- sqrt(
    43.782402 + 2.444014 * df$age18 - 43.067165 * df$p_respondent + 57.574635 *
      swan_ia_low + 7.026785 * df$age18 * df$p_respondent -
      4.044906 * df$age18 * swan_ia_low
  )

  res_adj <-
    dplyr::case_when(
      !(df$youth)  ~ -0.050939400,
      as.logical(df$youth)  ~ -0.001064299
    )
  sd_adj <-
    dplyr::case_when(
      !(df$youth)  ~ 0.9993775,
      as.logical(df$youth)  ~ 0.9932011
    )

  df$swan_ia_tscores <- (((df$swan_ia_pro - swan_ia_pred) / swan_ia_sd_pred
  ) + res_adj) / (sd_adj) * 10 + 50

  df$swan_ia_tscores <-
    ifelse((df$age18 < 12) &
             (df$p_respondent == 0),
           NA,
           df$swan_ia_tscores
    )


  #### Hyperactive with gender
  swan_hi_gender_pred <- -4.17675209 - 0.04167303 * df$age18 + 0.70229528 * df$female + 5.13833773 *
    df$p_respondent -
    0.35179042 * df$age18 * df$p_respondent - 4.35765313 * df$female * df$p_respondent

  swan_hi_gender_low <- as.numeric((df$swan_hi_pro - swan_hi_gender_pred) < 0)

  swan_hi_gender_sd_pred <- sqrt(
    97.2118786  - 1.6070110 * df$age18 - 17.5869608 * df$female -
      62.4036863 * df$p_respondent + 7.5639752 * swan_hi_gender_low  + 0.5922562 *
      df$age18 * df$female + 6.4226127 * df$age18 * df$p_respondent +
      1.3285283 * df$age18 * swan_hi_gender_low + 4.5001409  * df$female *
      df$p_respondent + 78.7787513 * df$female *
      swan_hi_gender_low + 19.6268534 *
      df$p_respondent * swan_hi_gender_low - 3.7360877 * df$age18 * df$female *
      swan_hi_gender_low - 49.7954285 * df$female * df$p_respondent * swan_hi_gender_low
  )

  res_adj <-
    dplyr::case_when(
      !(df$female) &
        !(df$youth)  ~ -0.07854678,
      !(df$female) &
        as.logical(df$youth)  ~ -0.07142296,
      as.logical(df$female)  &
        !(df$youth) ~ -0.08761418,
      as.logical(df$female)  &
        as.logical(df$youth) ~ -0.09748210
    )
  sd_adj <-
    dplyr::case_when(
      !(df$female) &
        !(df$youth)  ~ 1.0002075,
      !(df$female) &
        as.logical(df$youth)  ~ 0.9772225,
      as.logical(df$female)  &
        !(df$youth) ~ 0.9956266,
      as.logical(df$female)  &
        as.logical(df$youth) ~ 0.9976900
    )

  df$swan_hi_gender_tscores <- (((df$swan_hi_pro - swan_hi_gender_pred) / swan_hi_gender_sd_pred
  ) + res_adj) / (sd_adj) * 10 + 50

  df$swan_hi_gender_tscores <-
    ifelse((df$age18 < 12) &
             (df$p_respondent == 0),
           NA,
           df$swan_hi_gender_tscores
    )

  #### Hyperactive across Gender
  swan_hi_pred <- -3.85277159 - 0.03568567 * df$age18 + 2.93347689 * df$p_respondent -
    0.34794969 * df$age18 *
    df$p_respondent

  swan_hi_low <- as.numeric((df$swan_hi_pro - swan_hi_pred) < 0)

  swan_hi_sd_pred <- sqrt(
    95.490458 - 1.760477 * df$age18 - 63.468222 * df$p_respondent + 38.526578 *
      swan_hi_low + 6.763566 * df$age18 * df$p_respondent
  )

  res_adj <-
    dplyr::case_when(
      !(df$youth)  ~ -0.08224601,
      as.logical(df$youth)  ~ -0.08580553
    )
  sd_adj <-
    dplyr::case_when(
      !(df$youth)  ~ 0.9980346,
      as.logical(df$youth)  ~ 0.9868408
    )

  df$swan_hi_tscores <- (((df$swan_hi_pro - swan_hi_pred) / swan_hi_sd_pred
  ) + res_adj) / (sd_adj) * 10 + 50

  df$swan_hi_tscores <-
    ifelse((df$age18 < 12) &
             (df$p_respondent == 0),
           NA,
           df$swan_hi_tscores
    )

  return(df = df)
}
