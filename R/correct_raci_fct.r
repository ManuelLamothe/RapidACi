#' Correct_raci function
#'
#' @description Internal function to correct A and Ci using the non-orthogonal
#'   coefficients obtained from the polynomial fitted to the matching empty chamber
#'   measurements.
#'
#' @param data The umpteenth element of the list under construction
#' @param curve Portion of the curve to use when there are both up (increasing [CO2]) and
#'   down (decreasing [CO2]) ramps (either "positive" or "negative")
#'
#' @return The correction factor used, the A corrected (Aleaf) and the Ci corrected


correct_raci <- function(data, curve) {

  if(curve == "positive") x <- data$posCurve_coefs
  if(curve == "negative") x <- data$negCurve_coefs
  
  y <- data$ACi_data
  
  if (length(x) == 2) {
    correction <- (x[[2]]*y$Meas_CO2_r) + x[[1]]

  } else if (length(x) == 3) {
    correction <- (x[[3]]*y$Meas_CO2_r^2) + (x[[2]]*y$Meas_CO2_r) + x[[1]]

  } else if  (length(x) == 4) {
    correction <- (x[[4]]*y$Meas_CO2_r^3) + (x[[3]]*y$Meas_CO2_r^2) + (x[[2]]*y$Meas_CO2_r) + 
                   x[[1]]

  } else if  (length(x) == 5) {
    correction <- (x[[5]]*y$Meas_CO2_r^4) + (x[[4]]*y$Meas_CO2_r^3) + (x[[3]]*y$Meas_CO2_r^2) +
                  (x[[2]]*y$Meas_CO2_r) + x[[1]]

  } else if  (length(x) == 6) {
    correction <- (x[[6]]*y$Meas_CO2_r^5) + (x[[5]]*y$Meas_CO2_r^4) + (x[[4]]*y$Meas_CO2_r^3) + 
                  (x[[3]]*y$Meas_CO2_r^2) + (x[[2]]*y$Meas_CO2_r) + x[[1]]
  }

  Aleaf <- y$GasEx_A - correction 
  Ci_corr <- (((y$GasEx_gtc - y$GasEx_E/2) * y$GasEx_Ca) - Aleaf) / (y$GasEx_gtc + y$GasEx_E/2)

  out <- list(correction, Aleaf, Ci_corr)
  return(out)
}
