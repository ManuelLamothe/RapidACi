#' extr_leafArea function
#'
#' @description Hidden function. Retrieve a dataframe of leaf surface area by sample from
#'   the WinSEEDLE software output files
#'
#' @param WinSEEDLE_filepath path for a winSEEDLE file (from the working directory)
#'
#' @return A dataframe of sample_ID with date and leaf area

extr_leafArea <- function(WinSEEDLE_filepath) {
  
  x <- read_delim(WinSEEDLE_filepath, delim = "\t", skip = 4, col_names = FALSE) %>%
       dplyr::filter(X2 == "GLOBAL") %>%
       select(sample_ID = "X1", date = "X6", leafArea_mm2 = "X19")
  
  return(x)
}
