#' get_system_nfo function
#'
#' @description Internal function to retrieve Li-Cor portable photosynthesis system model
#'   in Excel files they produced
#'
#' @param lst A vector of file address ("character")
#' @param range Cell where LiCor system version will be find if no "Remarks" sheet present
#'
#' @return The Li-Cor system model. This should be in cell D3 (6800) or B4 (6400).

get_system_nfo <- function(lst = lst, sysv = "B4", osv = "C1") {   #, verbose = FALSE

  sys <- osv <- pth <- vector("character", length(lst))

  suppressMessages(
    try(
      for(i in 1:length(lst)) {
        nfo <- read_excel(lst[i], sheet = "Remarks", col_names = FALSE)
        pth[i] <- lst[i]
        osv[i] <- nfo[which(nfo[,1] == "Console ver"), 2]
        sys[i] <- nfo[which(nfo[,1] == "Chamber type"), 2]
      },
      silent = TRUE
    )
  )
  
  output <- bind_cols(file = unlist(pth), osv = unlist(osv), sys = unlist(sys))
  output[output == ""] <- NA
  
  suppressMessages(
  if(sum(is.na(output$sys)) > 0) {
    for(i in 1:length(lst)) {
      if(is.na(output[i, "sys"])) {
        output[i, "file"] <- lst[i]
        output[i, "sys"] <- readxl::read_excel(lst[i], sheet = 1, range = sysv, 
                                               col_names = FALSE, col_types = "text")
        # output[i, "osv"] <- readxl::read_excel(lst[i], sheet = 1, range = osv, 
        #                                        col_names = FALSE, col_types = "text")
      }
    }
  }
  )
  
  if(sum(is.na(output$sys)) > 0) {
    stop("LiCor System (e.g. '6400', '6800') cannot be retrieved from cell B4 or 'Remarks' sheet. 
    Use 'range' option to specify where this info can be found")
  }
  
  # if(sum(is.na(output$osv)) > 0) {
  #   suppressMessages(
  #     for(x in which(is.na(output$osv))) {
  #       output[x, "osv"] <- readxl::read_excel(lst[x], sheet = 1, range = "C1", col_names = FALSE,
  #                                              col_types = "text")
  #     }
  #   )
  #   if(verbose) 
  #     message("LiCor OS version used ('1.3.17', '1.4.05', etc) for DARK or SLOW files can 
  #     be read from cell C1. You can edit these files manually to include this value (optional)")
  # }
  
  return(output)
}



