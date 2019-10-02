#' correct_dark function
#'
#' @param darkfile_path 
#' @param leafArea 
#' @param LiCor_system 
#'
#' @return A subset dataframe with the variables given in arguments
#'
#' @export

correct_dark <- function(darkfile_path, 
                         leafArea,
                         LiCor_system) {
  
  wb <- XLConnect::loadWorkbook(darkfile_path)
  
  if(grepl("6800", LiCor_system)){
    nmB <- readWorksheet(wb, sheet = 1, startRow = 14, endRow = 14, header = FALSE) 
    nmA <- readWorksheet(wb, sheet = 1, startRow = 15, endRow = 15, header = FALSE)
    nmG <- paste0(nmB, "_", nmA)
    variables = c("GasEx_A", "Const_S")
  } else if(grepl("6400", LiCor_system)){
    nmB <- readWorksheet(wb, sheet = 1, startRow = 8, endRow = 8, header = FALSE) 
    nmA <- readWorksheet(wb, sheet = 1, startRow = 9, endRow = 9, header = FALSE)
    nmG <- paste0(nmB, "_", nmA)
    variables = c("Photo_out", "Area_in")
  }
  
  if(grepl("6800", LiCor_system)){
  message("##### Be careful, this function has never been tested with a LI6800. It assumes the the first UserDefVar column was used for sample_ID")
  message("check printed output")
  
    if(!is.null(leafArea)) {
      y <- readWorksheet(wb, sheet = 1, startCol = grep("UserDefVar", nmG)[1], 
                         header = FALSE,  endCol = grep("UserDefVar", nmG)[1])
      z <- dplyr::filter(leafArea, sample_ID %in% y[,])
      
      line <- vector("list", length = nrow(z))    
      for(i in 1:nrow(z)){ 
        x <- which(y[,] == z$sample_ID[i])
        writeWorksheet(wb, data = z[i, "leafArea_mm2"] / 100, sheet = 1, 
                       startRow = x, startCol = which(nmG == variables[2]), header = FALSE)
        
        line[[i]][1] <- z$sample_ID[i]
        line[[i]][2] <- readWorksheet(wb, sheet = 1, header = FALSE,
                                   startRow = x, endRow = x,
                                   startCol = which(nmG == variables[1]), 
                                   endCol = which(nmG == variables[1]))
      }
      Photo <- do.call(rbind, line) %>% as_tibble() %>% unnest() %>%
               set_names(c("sample_ID", "Rd"))
      xlcFreeMemory()
      
    } else {
      Photo <- readWorksheet(wb, sheet = 1, header = FALSE,
                             startRow = 17,
                             startCol = grep("UserDefVar", nmG)[1], 
                             endCol = which(nmG == variables[1])) %>%
               select(1, length(.)) %>%
               set_names(c("sample_ID", "Rd"))
    }

  } else if(grepl("6400", LiCor_system)){
    
    if(!is.null(leafArea)) {
      y <- readWorksheet(wb, sheet = 1, startCol = 3, endCol = 3, header = FALSE)
      z <- dplyr::filter(leafArea, sample_ID %in% y[,])
    
      line <- vector("list", length = nrow(z))    
      for(i in 1:nrow(z)){ 
        x <- which(y[,] == z$sample_ID[i])
        writeWorksheet(wb, data = z[i, "leafArea_mm2"] / 100, sheet = 1, 
                       startRow = x, startCol = which(nmG == variables[2]), header = FALSE)

        line[[i]][1] <- z$sample_ID[i]
        line[[i]][2] <- readWorksheet(wb, sheet = 1, header = FALSE,
                                   startRow = x, endRow = x,
                                   startCol = which(nmG == variables[1]), 
                                   endCol = which(nmG == variables[1]))
      }
      Photo <- do.call(rbind, line) %>% as_tibble() %>% unnest() %>% 
               set_names(c("sample_ID", "Rd"))
      xlcFreeMemory()
    
    } else {
      Photo <- readWorksheet(wb, sheet = 1, header = FALSE,
                             startRow = 10,
                             startCol = 3, 
                             endCol = which(nmG == variables[1])) %>%
               select(1, length(.)) %>%
               set_names(c("sample_ID", "Rd"))
    }
  }

  return(Photo)
}