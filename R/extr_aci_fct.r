#' extr_aci function
#'
#' @description Function that will rapidly extract the data required for the analysis with
#'   plantecophys
#'
#' @param result_list The results list produced by the Rapid_aci_correction function
#'
#' @return A list of dataframes including the corrected A and Ci, and other variables, to
#'   use with plantecophys
#'
#' @export

extr_aci <- function(result_list) {

  lst <- result_list
  Raci <-  vector("list", length = length(lst))

  for(i in seq_along(lst)) {

    Raci[[i]] <- bind_cols(lst[[i]]$ACi_data, 
                           lst[[i]]$Aleaf, lst[[i]]$Ci_corrected) %>%
                 select(Photo = V1, Ci = V2, Tleaf = Meas_Tleaf, PARi = Meas_Qamb_in, everything()) %>%
                 dplyr::filter(deltaA > 0 & deltaCi >= 0 & Ci >= 0) %>%
                 mutate(Rd = lst[[i]]$Rd)
 
    # Raci[[i]] <- bind_cols(dplyr::filter(lst[[i]]$ACi_data, directn == lst[[i]]$correction_curve_used), 
    #                        lst[[i]]$Aleaf, lst[[i]]$Ci_corrected) %>%
    #   select(Photo = V1, Ci = V2, Tleaf = Meas_Tleaf, PARi = Meas_Qamb_in, everything()) %>%
    #   dplyr::filter(deltaCi >= 0 & Ci >= 0) %>%
    #   mutate(Rd = lst[[i]]$Rd)
    
  }
  
  names(Raci) <- names(lst)
  return(Raci)
}