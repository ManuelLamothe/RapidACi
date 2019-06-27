#' build_list function
#'
#' @description  Generate a list of the files present in a specified directory.
#' The function use the MATCH timestamp to identify which 'empty chamber' file to use to
#' correct the measurements files (same timestamp)
#'
#'
#' @param path_to_licor_files Path where all files are stored (Empty)
#' @param pattern_for_empty Regex pattern that will match only filenames for empty chamber
#' files
#' @param pattern_for_measure Regex pattern that will only match filenames for measurement
#' files
#' @param match_timestamp_column Column index number that corresponds to the MATCH timestamp
#' in the Li-Cor measurement files. By default it should corresponds to the parameter default
#'
#' @return The function return a dataframe that includes the path to files, the type of file
#' and the MATCH timestamp.
#' @export
#'
#' @examples
#' @section TODO: find better pattern
#' @section TODO: behavior for absence of files
#' @section TODO: warnings for partial filename pattern match

build_list <- function(path_to_licor_files = "data/",
                       pattern_for_empty = "*[^(empty)]+.xlsx",
                       pattern_for_measure = "*[(empty)]+.xlsx",
                       match_timestamp_column = 66) {

  x <- path_to_licor_files
  source("R/extr_timestamps_fct.r")

  lst_A <- paste0(x, list.files(x, pattern = pattern_for_empty, ignore.case = TRUE))
  lst_B <- paste0(x, list.files(x, pattern = pattern_for_measure, ignore.case = TRUE))
  lst_AB <- c(lst_A, lst_B)

  df <- tibble(path = lst_AB,
               chamber = c(rep("EMPTY", length(lst_A)), rep("Measurements", length(lst_B))),
               timestamp = extr_timestamps(lst_AB, match_timestamp_column)) %>%
        arrange(timestamp)

  return(df)
}
