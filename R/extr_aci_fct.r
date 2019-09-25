#' extr_aci function
#'
#' @description Hidden function (for now) that could help build a data.frame of leaf
#'   surface area by tree using the WinSEEDLE software
#'
#' @param result_list The name of the resulting list produced by Rapid_aci_correction
#'   function
#'
#' @return A list of dataframes including the corrected A and Ci, as well as other
#'   variables, that are used by plantecophys
#'
#' @export

extr_aci <- function(result_list) {

  lst <- result_list
  Raci <-  vector("list", length = length(lst))
  
  # for(i in seq_along(lst)) {
  # 
  #   if(lst[[i]]$correction_curve_used == "positive") {
  #     Raci[[i]] <- bind_cols(dplyr::filter(lst[[i]]$ACi_data, directn == "positive"), 
  #                            lst[[i]]$Aleaf, lst[[i]]$Ci_corrected) %>%
  #       select(Photo = V1, Ci = V2, Tleaf = Meas_Tleaf, PARi = Meas_Qamb_in, everything()) %>%
  #       dplyr::filter(deltaCi >= 0 & Ci >= 0)
  #   } else if (lst[[i]]$correction_curve_used == "negative") {
  #     Raci[[i]] <- bind_cols(dplyr::filter(lst[[i]]$ACi_data, directn == "negative"), 
  #                            lst[[i]]$Aleaf, lst[[i]]$Ci_corrected) %>%
  #       select(Photo = V1, Ci = V2, Tleaf = Meas_Tleaf, PARi = Meas_Qamb_in, everything()) %>%
  #       dplyr::filter(deltaCi >= 0 & Ci >= 0)
  #   }
  # }
  
  for(i in seq_along(lst)) {

    Raci[[i]] <- bind_cols(dplyr::filter(lst[[i]]$ACi_data, directn == lst[[i]]$correction_curve_used), 
                           lst[[i]]$Aleaf, lst[[i]]$Ci_corrected) %>%
                 select(Photo = V1, Ci = V2, Tleaf = Meas_Tleaf, PARi = Meas_Qamb_in, everything()) %>%
                 dplyr::filter(deltaCi >= 0 & Ci >= 0) %>%
                 mutate(Rd = lst[[i]]$Rd)
  }
  
  names(Raci) <- names(lst)
  return(Raci)
}