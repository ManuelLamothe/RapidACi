#' extr_timestamps function
#'
#' @param list_files
#' @param match_timestamp_column
#'
#' @return
#'
#' @examples

extr_timestamps <- function(list_files,
                            match_timestamp_column) {

  timestamps <- .POSIXct(vector("character"))

  for(file in list_files) {
    suppressMessages(x <- readxl::read_excel(file, skip = 15, sheet = "Measurements"))
    timestamps[file] <- strptime(as.character(x[1, match_timestamp_column]), "%s")
  }

  return(timestamps)
}

