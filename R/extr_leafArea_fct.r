#' extr_timestamps function
#'
#' @description Hidden function (for now) that could help build a data.frame of leaf
#'   surface area by tree using the WinSEEDLE software
#'
#' @param winSEEDLE_filepath path for a winSEEDLE file (from the working directory)
#'
#' @return tree_Id list with date and leaf area
#' 
#' @examples 
#' \dontrun{
#' # Merging multiple files
#' leafArea_df <- NULL
#' dir <-"data/WinSEEDLE/"
#' list <- list.files(dir)
#' for(file in list) leafArea_df <- rbind(leafArea_df, extr_leafArea(paste0(dir, file)))
#' leafArea_df$sample_ID <- gsub("-", "_", leafArea_df$sample_ID)
#' }

extr_leafArea <- function(WinSEEDLE_filepath) {
  
    x <- read_delim(WinSEEDLE_filepath, delim = "\t", skip = 4, col_names = FALSE) %>%
    dplyr::filter(X2 == "GLOBAL") %>%
    select(sample_ID = "X1", date = "X6", leafArea_mm2 = "X19")
  
  return(x)
}
