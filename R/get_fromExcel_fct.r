#' get_fromExcel function
#'
#' @description This function allow the extraction of selected variables from an Excel
#'   file produced by the Li-Cor portable photosynthesis system LI-6800. If leaf area is
#'   provided, the data retrieved will be adjusted accordingly.
#'
#' @param filepath  Path to an Excel file
#' @param leafArea_cm2  Numeric value of the sample leaf area (default = NA, i.e. no
#'   correction is applied)
#' @param variables The list of variables to retrieve from the file
#' @param show.variables.names Logical argument to print the entire list of variable names
#'   to the console (default = FALSE)
#'
#' @return A dataframe with the variables given in arguments
#'
#' @export

get_fromExcel <- function(filepath, 
                          leafArea_cm2 = NA,
                          variables = c("GasEx_A", "GasEx_Ci", "GasEx_gtc", "GasEx_gsw",
                                        "GasEx_TleafCnd","Meas_CO2_r", "Meas_Tleaf", 
                                        "Meas_Tleaf2", "Meas_Qamb_in", "Const_S"),
                          show.variables.names = FALSE) {

  wb <- XLConnect::loadWorkbook(filepath)
  nmB <- readWorksheet(wb, sheet = 1, startRow = 14, endRow = 14, header = FALSE) 
  nmA <- readWorksheet(wb, sheet = 1, startRow = 15, endRow = 15, header = FALSE)
  nmG <- paste0(nmB, "_", nmA)

  if (!is.na(leafArea_cm2)) { 
    writeWorksheet(wb, data = rep(leafArea_cm2, getLastRow(wb, sheet = 1)), sheet = 1, 
                   startRow = 17, startCol = which(nmG == "Const_S"), header = FALSE)
  
    setForceFormulaRecalculation(wb, sheet = 1, TRUE)    
  
    index <- which(nmG %in% variables)

    dataF <- readWorksheet(wb, sheet = 1, startRow = 17, header = FALSE,
                           startCol = min(index), endCol = max(index)) %>%
             select(index - min(index) + 1) %>%
             set_names(nmG[which(nmG %in% variables)])

  } else {
  
    dataF <- readWorksheet(wb, sheet = 1, startRow = 17, header = FALSE) %>%
             set_names(nmG) %>%
             select(nmG[which(nmG %in% variables)])
  }

  xlcFreeMemory()  
  
  if(show.variables.names) {
    print(nmG); dataF <- GasEx_A <- NA
  } else {  
    dataF <- drop_na(dataF, GasEx_A); return(dataF)
  }
}