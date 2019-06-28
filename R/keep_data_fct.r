#' keep_data function
#'
#' @param path
#'
#' @return
#'
#' @examples

keep_data <- function(path) {

  dt <- suppressMessages(read_excel(path, skip = 14, sheet = "Measurements")) %>%
    select(A, Ci, CO2_r, gtc, gsw, E, Ca, Tleaf, Qamb_in) %>%
    slice(-1) %>%
    mutate_each(funs(as.numeric)) %>%
    as.data.table()

  dt[, deltaA  := c(0, diff(A)), ]
  dt[, deltaci := c(0, diff(Ci)),]

  dt<- dt %>%
    mutate(n = 1:n(), positif = ifelse(deltaA > 0, TRUE, FALSE)) %>%
    split(.$positif)

  return(dt)
}
