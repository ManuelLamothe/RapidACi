#' best_fit function
#'
#' @param data
#' @param max_degree
#'
#' @return
#'
#' @examples

best_fit <- function(data, max_degree = max_degree) {

  if(max_degree%%1 != 0 | max_degree < 2 | max_degree > 5) {
    stop("max_degree only takes values: 2, 3, 4 or 5")
  }

  x <- lm(A ~ CO2_r, data)
  y <- lm(A ~ CO2_r + I(CO2_r^2), data)

  suppressWarnings(
  z1 <- lm(A ~ CO2_r + I(CO2_r^2) + I(CO2_r^3), data))
  suppressWarnings(
  z2 <- lm(A ~ CO2_r + I(CO2_r^2) + I(CO2_r^3) + I(CO2_r^4), data))
  suppressWarnings(
  z3 <- lm(A ~ CO2_r + I(CO2_r^2) + I(CO2_r^3) + I(CO2_r^4) + I(CO2_r^5), data))

  bic <- c(BIC(x), BIC(y), BIC(z1), BIC(z2), BIC(z3))
  bic <- bic[1:max_degree]

  if(which.min(bic) == 1) return(coef(x))
  if(which.min(bic) == 2) return(coef(y))
  if(which.min(bic) == 3) return(coef(z1))
  if(which.min(bic) == 4) return(coef(z2))
  if(which.min(bic) == 5) return(coef(z3))

}
