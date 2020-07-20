#' correct_dark function
#'
#' @description Internal function used to retrieve the "Photo_out" value measured for the
#'   dark chamber (with the correction for the leaf area if available). This function can
#'   be used for both a LI-6800 and LI-6400 generated
#'
#' @param darkfile_path Path to a DARK file
#' @param leafArea_df A dataframe containing at least a "sample_ID" column and a
#'   "leafArea_mm2" column (default = NULL)
#' @param LiCor_sys The model of Li-Cor portable photosynthesis system (either
#'   "LI-6800" or "LI-6400")
#' @param LiCor_osv 
#'
#' @return The single value of "Photo_out"


correct_dark <- function(darkfile_path, 
                         leafArea_df,
                         LiCor_sys,
                         LiCor_osv) {
  
  wb <- XLConnect::loadWorkbook(darkfile_path)
  
  if(grepl("6800", LiCor_sys)){
    nmB <- readWorksheet(wb, sheet = 1, startRow = 14, endRow = 14, header = FALSE) 
    nmA <- readWorksheet(wb, sheet = 1, startRow = 15, endRow = 15, header = FALSE)
    nmG <- paste0(nmB, "_", nmA)
    variables = c("GasEx_A", "Const_S")
    startRow = 17
  } else if(grepl("6400", LiCor_sys)){
    nmB <- readWorksheet(wb, sheet = 1, startRow = 8, endRow = 8, header = FALSE) 
    nmA <- readWorksheet(wb, sheet = 1, startRow = 9, endRow = 9, header = FALSE)
    nmG <- paste0(nmB, "_", nmA)
    variables = c("Photo_out", "Area_in")
    startRow = 10
  }
  
 # Could be incomplete or incorrect for 6400 with newer os version
  UserDef <- case_when(grepl("1.3", LiCor_osv) & grepl("6800", LiCor_sys) ~ "UserDefVar", 
                       grepl("1.4", LiCor_osv) & grepl("6800", LiCor_sys) ~ "UserDefCon_Sample", 
                       TRUE ~ "Tree #_in")
    
  if(grepl("6800", LiCor_sys)){
 #   warning("This function assumes the first UserDefVar column was used for entering sample_ID")
  
    if(!is.null(leafArea_df)) {
      y <- readWorksheet(wb, sheet = 1, startCol = grep(UserDef, nmG)[1], 
                         header = FALSE,  endCol = grep(UserDef, nmG)[1],
                         startRow = startRow)
      z <- dplyr::filter(leafArea_df, sample_ID %in% y[,])
      
      line <- vector("list", length = nrow(z))    
      for(i in 1:nrow(z)){ 
        x <- which(y[,] == z$sample_ID[i])
        
        if(!is.na(z[i, "leafArea_mm2"])) {
          writeWorksheet(wb, data = z[i, "leafArea_mm2"] / 100, sheet = 1, 
                         startRow = x, startCol = which(nmG == variables[2]), header = FALSE)
          setForceFormulaRecalculation(wb, sheet = 1, TRUE)
        }
        
        line[[i]][1] <- z$sample_ID[i]
        line[[i]][2] <- readWorksheet(wb, sheet = 1, header = FALSE,
                                   startRow = x + startRow -1, endRow = x + startRow -1,
                                   startCol = which(nmG == variables[1]), 
                                   endCol = which(nmG == variables[1]))
      }
      
      Photo <- do.call(rbind, line) %>% 
        as_tibble(.name_repair = ~ c("sample_ID", "Rd")) %>% 
        unnest(cols = c(sample_ID, Rd))
      
      xlcFreeMemory()
      
    } else {
      Photo <- readWorksheet(wb, sheet = 1, header = FALSE,
                             startRow = startRow,
                             startCol = grep(UserDef, nmG)[1], 
                             endCol = which(nmG == variables[1])) %>%
               select(1, length(.)) %>%
               set_names(c("sample_ID", "Rd"))
    }

  } else if(grepl("6400", LiCor_sys)){
    
    if(!is.null(leafArea_df)) {
      y <- readWorksheet(wb, sheet = 1, startCol = 3, endCol = 3, startRow = startRow, header = FALSE)
      z <- dplyr::filter(leafArea_df, sample_ID %in% y[,])
    
      line <- vector("list", length = nrow(z))    
      for(i in 1:nrow(z)){ 
        x <- which(y[,] == z$sample_ID[i])
        
        if(!is.na(z[i, "leafArea_mm2"])) {
          writeWorksheet(wb, data = z[i, "leafArea_mm2"] / 100, sheet = 1, header = FALSE,
                         startRow = x + startRow -1, startCol = which(nmG == variables[2]))
          setForceFormulaRecalculation(wb, sheet = 1, TRUE)
        }
        
        line[[i]][1] <- z$sample_ID[i]
        line[[i]][2] <- readWorksheet(wb, sheet = 1, header = FALSE,
                                      startRow = x + startRow -1, endRow = x + startRow -1,
                                      startCol = which(nmG == variables[1]), 
                                      endCol = which(nmG == variables[1]))
      }
      Photo <- do.call(rbind, line) %>% 
               as_tibble(.name_repair = ~ c("sample_ID", "Rd")) %>% 
               unnest(cols = c(sample_ID, Rd))
      
      xlcFreeMemory()
    
    } else {
      Photo <- readWorksheet(wb, sheet = 1, header = FALSE,
                             startRow = startRow,
                             startCol = 3, 
                             endCol = which(nmG == variables[1])) %>%
               select(1, length(.)) %>%
               set_names(c("sample_ID", "Rd"))
    }
  }

  return(Photo)
}