#' extr_leafArea function
#'
#' @description Hidden function (for now) that could help build a data.frame of leaf
#'   surface area by tree using the WinSEEDLE software
#'
#' @param WinSEEDLE_filepath path for a winSEEDLE file (from the working directory)
#'
#' @return tree_Id list with date and leaf area

extr_leafArea <- function(WinSEEDLE_filepath) {
  
  x <- read_delim(WinSEEDLE_filepath, delim = "\t", skip = 4, col_names = FALSE) %>%
       dplyr::filter(X2 == "GLOBAL") %>%
       select(sample_ID = "X1", date = "X6", leafArea_mm2 = "X19")
  
  return(x)
}
