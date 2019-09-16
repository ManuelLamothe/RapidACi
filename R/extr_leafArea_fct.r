#' extr_timestamps function
#'
#' @description Hidden function (for now) that could help build a data.frame of leaf
#'   surface area by tree using the WinSEEDLE software
#'
#' @param winSEEDLE_filepath path for a winSEEDLE file (from the working directory)
#'
#' @return tree_Id list with date and leaf area
#' 

extr_leafArea <- function(WinSEEDLE_filepath) {
  
  x <- read_delim(winSEEDLE_filepath, delim = "\t", skip = 4, col_names = FALSE) %>%
    dplyr::filter(X2 == "GLOBAL") %>%
    select(Tree_ID = "X1", Date = "X6", LeafArea = "X19")
  
  return(x)
}