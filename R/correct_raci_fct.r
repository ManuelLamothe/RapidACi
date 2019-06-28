#' Correct_raci function
#'
#' @param data
#' @param curve
#'
#' @return
#'
#' @examples

correct_raci <- function(data, curve) {

  data <- flatten(data)

  dt <- suppressMessages(read_excel(data$Rapci_file, skip = 14, sheet = "Measurements")) %>%
    select(A, CO2_r, gtc, E, Ca) %>%
    slice(-1) %>%
    mutate_each(list(as.numeric))

  dt$deltaA <- c(0, diff(dt$A))

  dt<- dt %>%
       mutate(n = 1:n(), positif = ifelse(deltaA > 0, TRUE, FALSE)) %>%
       split(.$positif)

  if(curve == "positive") {x <- data$posCurve_coefs; y <- dt$`TRUE`}
  if(curve == "negative") {x <- data$negCurve_coefs; y <- dt$`FALSE`}

  if (length(x) == 2) {
    correction <- (x[[2]]*y$CO2_r) + x[[1]]

  } else if (length(x) == 3) {
    correction <- (x[[3]]*y$CO2_r^2) + (x[[2]]*y$CO2_r) + x[[1]]

  } else if  (length(x) == 4) {
    correction <- (x[[4]]*y$CO2_r^3) + (x[[3]]*y$CO2_r^2) + (x[[2]]*y$CO2_r) + x[[1]]

  } else if  (length(x) == 5) {
    correction <- (x[[5]]*y$CO2_r^4) + (x[[4]]*y$CO2_r^3) + (x[[3]]*y$CO2_r^2) + (x[[2]]*y$CO2_r) + x[[1]]

  } else if  (length(x) == 6) {
    correction <- (x[[6]]*y$CO2_r^5) + (x[[4]]*y$CO2_r^3) + (x[[3]]*y$CO2_r^2) + (x[[2]]*y$CO2_r) + x[[1]]
  }

  Aleaf <- y$A - correction
  ci_corr <- (((y$gtc - y$E/2) * y$Ca) - Aleaf) / (y$gtc + y$E/2)

  out <- list(correction, Aleaf, ci_corr)
  return(out)
}
