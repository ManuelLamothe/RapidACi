#' best_coefs function
#'
#' @param empty_file
#' @param leafArea_cm2
#' @param delta_max
#' @param skip_first
#' @param min_CO2
#' @param max_degree
#' @param diagnostic_plots
#'
#' @return
#'
#'

getFromExcel <- function(filepath, leafArea_cm2 = NA) {

  wb <- XLConnect::loadWorkbook(filepath)
  nmB <- readWorksheet(wb, sheet = 1, startRow = 14, endRow = 14, header = FALSE) 
  nmA <- readWorksheet(wb, sheet = 1, startRow = 15, endRow = 15, header = FALSE)
  nmG <- paste0(nmB, "_", nmA)
  
  interest_var <- c("GasEx_A", "GasEx_Ci", "GasEx_gtc", "GasEx_TleafCnd",
                    "Meas_CO2_r", "Meas_Tleaf", "Meas_Tleaf2", "Meas_Qamb_in")

  if (!is.na(leafArea_cm2)) { 
    writeWorksheet(wb, data = rep(leafArea_cm2, getLastRow(wb, sheet = 1)), sheet = 1, 
                   startRow = 17, startCol = which(nmG == "Const_S"), header = FALSE)
  
    setForceFormulaRecalculation(wb, sheet = 1, TRUE)    
  
    index <- which(nmG %in% interest_var)

    dataF <- readWorksheet(wb, sheet = 1, startRow = 17, header = FALSE,
                           startCol = min(index), endCol = max(index)) %>%
             select(index_vec - min(index_vec) + 1) %>%
             set_names(interest_var)

  } else {
  
    dataF <- readWorksheet(wb, sheet = 1, startRow = 17, header = FALSE) %>%
               set_names(nmG) %>%
               select(interest_var)
  }

  xlcFreeMemory()  
  return(dataF)
}