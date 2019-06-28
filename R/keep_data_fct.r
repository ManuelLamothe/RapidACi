#' keep_data function
#'
#' @param path
#'
#' @return
#'
#' @examples

keep_data <- function(path) {

  dt <- suppressMessages(readxl::read_excel(path, skip = 14, sheet = "Measurements")) %>%
    select(A, Ci, CO2_r, gtc, gsw, E, Ca, Tleaf, Qamb_in) %>%
    slice(-1) %>%
    mutate_each(list(as.numeric)) 

  dt$deltaA  <- c(0, diff(dt$A))
  dt$deltaCi <- c(0, diff(dt$Ci))
  
  dt<- dt %>%
    mutate(n = 1:n(), positif = ifelse(deltaA > 0, TRUE, FALSE)) %>%
    split(.$positif)

  return(dt)
}
