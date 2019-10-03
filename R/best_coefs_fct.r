#' best_coefs function
#'
#' @description Internal function to retrieve data from empty chamber measurement files
#'   (section 1) and coefficients of the best fitting polynomial (section 2)
#'
#' @param empty_file Inherited argument from Rapid_aci_correction function
#' @param leafArea_cm2 Leaf area in cm2
#' @param delta_max Inherited argument from Rapid_aci_correction function
#' @param max_degree Inherited argument from Rapid_aci_correction function
#'
#' @return Data and coefficients from the best fitting polynomial

best_coefs <- function(empty_file,
                       leafArea_cm2 = NA,
                       delta_max,
                       max_degree) {
  
  
  # SECTION 1 : data acquisition
  ##############################
  
  # Each empty chamber file has its values recalculated after the modification of Const_S 
  df <- get_fromExcel(empty_file, leafArea_cm2 = leafArea_cm2,
                      variables = c("GasEx_A", "GasEx_Ci", "GasEx_gtc", "GasEx_gsw", 
                                    "GasEx_TleafCnd", "Meas_CO2_r", "Meas_Tleaf", 
                                    "Meas_Tleaf2", "Meas_Qamb_in"))
  
  # SECTION 2 : GETTING THE COEFS OF BEST FITTING CURVE
  #####################################################
  
  data <- df %>%
    dplyr::filter(GasEx_A > 1 | GasEx_A < -1) %>%
    mutate(curve = ifelse(GasEx_A < 0, "negative", "positive"), n = 1:n()) %>%
    group_by(curve) %>%
    mutate(delta = GasEx_A - dplyr::lag(GasEx_A)) %>%
    mutate(good = ifelse(delta < delta_max & delta >= -delta_max, 1, 0)) %>%
    dplyr::filter(!is.na(good))

  results_p <- vector("list")
  pos <- dplyr::filter(data, curve == "positive",  good == 1)
  neg <- dplyr::filter(data, curve == "negative",  good == 1)
    
  if(dim(pos)[1] == 0) {
    results_p[[empty_file]]$positive <- NA
  } else {
    results_p[[empty_file]]$positive <- best_fit(pos, max_degree)
  }
  
  if(dim(neg)[1] == 0) {
    results_p[[empty_file]]$negative <- NA
  } else {
    results_p[[empty_file]]$negative <- best_fit(neg, max_degree)
  }
    
  results_p[[empty_file]]$empty_data <- data

  return(results_p)
}
