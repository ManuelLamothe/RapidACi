#' get_system function
#'
#' @param list_files
#'
#' @return Return the Li-Cor system model as read in the Excel files they produce. This
#'   should be in cell D3 (6800) or B4 (6400).


get_system <- function(list_files) {

  systems <- vector("character", length(list_files))
  
  suppressMessages(
  for(i in 1:length(list_files)) {
    if(identical(
         str_extract(readxl::read_excel(list_files[i], range = "D3", col_names = FALSE), 
                     "\\d+-\\d+"), character(0)) == TRUE) {
      if(identical(
           str_extract(readxl::read_excel(list_files[i], range = "B4", col_names = FALSE), 
                       "\\d+-\\d+"), character(0)) == TRUE) {
         systems[i] <- NA
      } else {
         systems[i] <- str_extract(readxl::read_excel(list_files[i], range = "B4", 
                                                      col_names = FALSE), "\\d+-\\d+")    
      }
    } else {
       systems[i] <- str_extract(readxl::read_excel(list_files[i], range = "D3", 
                                                    col_names = FALSE), "\\d+-\\d+")
    }
  }
  )
  
  output <- unlist(systems)
  return(output)
}




