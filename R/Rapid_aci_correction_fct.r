#' Rapid_aci_correction function
#'
#' @param list_files  A list of files as generated by the build_list function
#' @param delta_max Difference between two measure points under which we consider the
#' curve as stable. If not sure, verify the diagnostic plots to see if correct
#' @param skip_first  The number of measures to drop at the begining of measurements (default = 10)
#' @param min_CO2 Minimum value of CO2 that is considered correct
#' @param max_degree  The maximum polynomial order to consider for fitting the empty
#' chamber measurement curve in order to correct the Rapid A-Ci measures. This parameter takes value
#' of 2, 3, 4 or 5 only as suggested by Stinziano _et al._ (2017).
#' @param diagnostic_plots Output a series of plot to diagnose/verify the process
#'
#' @return The function return a list of elements for each measurement file that contains
#' the corrected Rapid A-Ci curve as well as the original values and parameters used for the
#' correction.
#'
#' @export
#'
#' @examples

Rapid_aci_correction <- function(list_files,
                                 delta_max = 0.05,
                                 skip_first = 10,
                                 min_CO2 = 20,
                                 max_degree = 3,
                                 diagnostic_plot = FALSE,
                                 curve_plot = FALSE) {

  list_files <- list_files[complete.cases(list_files[ , "START_time"]),]
  
  match_timestamps <- unique(list_files[, "timestamp"])
  lst <- rep(list(list()), nrow(dplyr::filter(list_files, chamber != "EMPTY")))

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
      lst[[j+i-1+z]]$Rapci_file <- raci_files$path[j]
      lst[[j+i-1+z]]$empty_file <- empty_chamber$path
      lst[[j+i-1+z]]$match_timestamp <- empty_chamber$timestamp
      lst[[j+i-1+z]]$originals <- keep_data(raci_files$path[j])
      lst[[j+i-1+z]]$posCurve_coefs <- best_coefs(list_files = empty_chamber$path,
                                                  delta_max = delta_max,
                                                  skip_first = skip_first,
                                                  min_CO2 = min_CO2,
                                                  max_degree = max_degree,
                                                  diagnostic_plot = diagnostic_plot,
                                                  curve_plot = curve_plot)[[1]]$positive
      lst[[j+i-1+z]]$negCurve_coefs <- best_coefs(list_files = empty_chamber$path,
                                                  delta_max = delta_max,
                                                  skip_first = skip_first,
                                                  min_CO2 = min_CO2,
                                                  max_degree = max_degree,
                                                  diagnostic_plot = FALSE,
                                                  curve_plot = FALSE)[[1]]$negative
      
      # will be put outside best_coef...
      #diagnostic_plot = diagnostic_plot,
      #curve_plot = curve_plot)[[1]]$negative
    }
    z = z + j - 1
  }

  for(i in 1:length(lst)) {
    if(!is.na(lst[[i]]$posCurve_coefs)) {
      x <- correct_raci(lst[i], "positive")
      lst[[i]]$posCurve_correction <- x[1]
      lst[[i]]$posCurve_Aleaf <- x[2]
      lst[[i]]$posCurve_ci_corrected <- x[3]
    } else {
      lst[[i]]$posCurve_correction <- lst[[i]]$posCurve_Aleaf <- lst[[i]]$posCurve_ci_corrected <- NA
    }
    if(!is.na(lst[[i]]$negCurve_coefs)) {
      x <- correct_raci(lst[i], "negative")
      lst[[i]]$negCurve_correction <- x[1]
      lst[[i]]$negCurve_Aleaf <- x[2]
      lst[[i]]$negCurve_ci_corrected <- x[3]
    } else {
      lst[[i]]$negCurve_correction <- lst[[i]]$negCurve_Aleaf <- lst[[i]]$negCurve_ci_corrected <- NA
    }
  }

  return(lst)
}
