#' extr_timestamps function
#'
#' @param list_files
#' @param timestamp_column
#'
#' @return Return file timestamp as read in Excel files produced by the LI-Cor portable
#'   photosynthesis systems.


extr_timestamps <- function(list_files,
                            timestamp_column) {

  timestamps <- vector("character", length(list_files))
  range <- paste0(timestamp_column, 17)
  
  for(i in 1:length(list_files)) {
    suppressMessages(x <- readxl::read_excel(list_files[i], range = range, col_names = FALSE))
    if(length(x) == 0) {
      timestamps[i] <- NA
    } else {
      timestamps[i] <- round(x, digits = 0)
    }
  }
  
  output <- unlist(timestamps)
  return(output)

}
