#' best_fit function
#'
#' @param data Data points to fit the 
#' @param max_degree Inherited argument from Rapid_aci_correction function
#'
#' @return The list of coefficients of the best fitted curve


best_fit <- function(data, max_degree) {

  if(max_degree%%1 != 0 | max_degree < 1 | max_degree > 5) {
    stop("max_degree only takes values up to 5")
  }

  x <- lm(GasEx_A ~ Meas_CO2_r, data)
  y <- lm(GasEx_A ~ Meas_CO2_r + I(Meas_CO2_r^2), data)

  suppressWarnings(
  z1 <- lm(GasEx_A ~ Meas_CO2_r + I(Meas_CO2_r^2) + I(Meas_CO2_r^3), data))
  suppressWarnings(
  z2 <- lm(GasEx_A ~ Meas_CO2_r + I(Meas_CO2_r^2) + I(Meas_CO2_r^3) + I(Meas_CO2_r^4), data))
  suppressWarnings(
  z3 <- lm(GasEx_A ~ Meas_CO2_r + I(Meas_CO2_r^2) + I(Meas_CO2_r^3) + I(Meas_CO2_r^4) + I(Meas_CO2_r^5), data))

  bic <- c(BIC(x), BIC(y), BIC(z1), BIC(z2), BIC(z3))
  bic <- bic[1:max_degree]

  if(which.min(bic) == 1) return(coef(x))
  if(which.min(bic) == 2) return(coef(y))
  if(which.min(bic) == 3) return(coef(z1))
  if(which.min(bic) == 4) return(coef(z2))
  if(which.min(bic) == 5) return(coef(z3))

}
