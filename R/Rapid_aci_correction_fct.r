#' Rapid_aci_correction function
#'
#' @param dark_df A dataframe containing a "sample_ID" column and a "Photo" column
#'   (default = NULL)
#' @param delta_max Difference between two measure points under which we consider the
#'   curve as stable. If not sure, verify the diagnostic plots to see if correct
#' @param skip_first  The number of measures to drop at the begining of measurements
#'   (default = 10)
#' @param min_CO2 Minimum value of CO2 that is considered correct
#' @param max_degree  The maximum polynomial order to consider for fitting the empty
#'   chamber measurement curve in order to correct the Rapid A-Ci measures. This parameter
#'   takes value from 1 up to 5 (as suggested by Stinziano _et al._, 2017).
#' @param priority_curve For the very specific case when coefficients can be retrieve from
#'   both the positive and the negative portion of the empty chamber A-Ci curve, a
#'   priority has to be set. This argument can only take "positive" or "negative" (default
#'   = "postive").
#' @param diagnostic_plots Output a series of plot to diagnose and verify that the
#'   portions of the measures that are used to obtain the correction coefficients from the
#'   empty chamber are correct
#'
#' @return The function return a list of elements by A-Ci measurement file that contains
#'   the sample ID, the leaf area used, the timestamp, the paths to the A-Ci and empty
#'   chamber file used, important data associated with the empty chamber file (the
#'   recalculated ones if leaf area were provided), the lag between the curves, the
#'   coefficients of the fitting curve used to correct values, as well as corrected Aleaf
#'   and Ci. correction for the Rapid A-Ci curve as well as the original values and
#'   parameters used for the correction.
#'
#' @export


Rapid_aci_correction <- function(list_files,
                                 delta_max = 0.05,
                                 skip_first = 10,
                                 min_CO2 = 20,
                                 max_degree = 3,
                                 priority_curve = "positive",
                                 diagnostic_plots = FALSE) {

  # Files with no START_time are more than likely useless for analysis
  list_files <- list_files[complete.cases(list_files[, "START_time"]),]

  # Initialization of results list 
  match_timestamps <- unique(list_files[, "timestamp"])
  lst <- rep(list(list()), nrow(dplyr::filter(list_files, chamber != "EMPTY")))
  
  # Input of basic information into the results list: for each raci file of each timestamp 
  z = 0
  for(i in 1:length(match_timestamps$timestamp)) {

    match_gr <- dplyr::filter(list_files, timestamp == match_timestamps$timestamp[i])
    empty_chamber <- match_gr[match_gr$chamber == "EMPTY", ]

    if(dim(empty_chamber)[1] == 0) {
      stop(paste("\nThere is NO empty chamber file for the timestamp",
                 match_timestamps$timestamp[i]))
    } else if(dim(empty_chamber)[1] != 1) {
      stop(paste("\nThere is more than one empty chamber files with the timestamp",
                 match_timestamps$timestamp[i]))
    }

    raci_files <- match_gr[match_gr$chamber == "FAST", ]

    if(length(raci_files) < 1) {
      message(paste("There is no raci file corresponding to timestamp",
                    match_timestamps$timestamp[i], "\n"))
    }

    for(j in 1:nrow(raci_files)) {
      
      xx <- best_coefs(empty_file = empty_chamber$path,
                       leafArea_cm2 = raci_files$leafArea_mm2[j] / 100,
                       delta_max = delta_max, skip_first = skip_first,
                       min_CO2 = min_CO2, max_degree = max_degree,
                       diagnostic_plots = diagnostic_plots)
      
      lst[[j+i-1+z]]$Sample_ID <- raci_files$sample_ID[j]
      lst[[j+i-1+z]]$leafArea_cm2 <- raci_files$leafArea_mm2[j] / 100
      lst[[j+i-1+z]]$match_timestamp <- as.character(empty_chamber$timestamp)
      lst[[j+i-1+z]]$ACi_file <- raci_files$path[j]
      lst[[j+i-1+z]]$ACi_data <-  getFromExcel(raci_files$path[j], 
                                               leafArea_cm2 = raci_files$leafArea_mm2[j] / 100,
                                               variables = c("GasEx_E", "GasEx_A", "GasEx_Ca", 
                                                             "GasEx_Ci", "GasEx_gtc", "GasEx_gsw",
                                                             "GasEx_TleafCnd","Meas_CO2_r", 
                                                             "Meas_Tleaf", "Meas_Tleaf2", 
                                                             "Meas_Qamb_in")) %>%
                                  mutate(deltaA  = c(0, diff(.$GasEx_A)), 
                                         deltaCi = c(0, diff(.$GasEx_Ci)), n = 1:n(),
                                         directn = ifelse(deltaA > 0, "positive", "negative"))
      lst[[j+i-1+z]]$empty_chamber_file <- empty_chamber$path
      lst[[j+i-1+z]]$empty_chamber_data <- xx[[1]]$empty_data
      lst[[j+i-1+z]]$lag_between_curves <- xx[[1]]$lag
      lst[[j+i-1+z]]$posCurve_coefs <- xx[[1]]$positive
      lst[[j+i-1+z]]$negCurve_coefs <- xx[[1]]$negative
    }
    z = z + j - 1
  }


  for(i in seq_along(lst)) {

    if(priority_curve == "positive") {
      if(sum(is.na(lst[[i]]$posCurve_coefs)) == 0) {
        x <- correct_raci(lst[[i]], "positive")
        lst[[i]]$correction_curve_used <- "positive"
        lst[[i]]$correction_factor <- x[1]
        lst[[i]]$Aleaf <- x[2]
        lst[[i]]$Ci_corrected <- x[3]  
      } else if(sum(is.na(lst[[i]]$negCurve_coefs)) == 0) {
        x <- correct_raci(lst[[i]], "negative")
        lst[[i]]$correction_curve_used <- "negative"
        lst[[i]]$correction_factor <- x[1]
        lst[[i]]$Aleaf <- x[2]
        lst[[i]]$Ci_corrected <- x[3]
      } else {
        lst[[i]]$correction_curve_used <- lst[[i]]$correction_factor <- lst[[i]]$Aleaf <- lst[[i]]$Ci_corrected <- NA
      }
    } else if(priority_curve == "negative") {
      if(sum(is.na(lst[[i]]$negCurve_coefs)) == 0) {
        x <- correct_raci(lst[[i]], "negative")
        lst[[i]]$correction_curve_used <- "negative"
        lst[[i]]$correction_factor <- x[1]
        lst[[i]]$Aleaf <- x[2]
        lst[[i]]$Ci_corrected <- x[3]  
      } else if(sum(is.na(lst[[i]]$posCurve_coefs)) == 0) {
        x <- correct_raci(lst[[i]], "positive")
        lst[[i]]$correction_curve_used <- "positive"
        lst[[i]]$correction_factor <- x[1]
        lst[[i]]$Aleaf <- x[2]
        lst[[i]]$Ci_corrected <- x[3]
      } else {
        lst[[i]]$correction_curve_used <- lst[[i]]$correction_factor <- lst[[i]]$Aleaf <- lst[[i]]$Ci_corrected <- NA
      }
    } else {
      stop("Priority_curve argument is incorrect, it only takes one of 'positive' or 'negative' as value")
    }
  }      

  names(lst) <- lapply(lst, `[[`, 1)
  return(lst)
}
