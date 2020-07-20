#' get_fromExcel function
#'
#' @description This function allow the extraction of selected variables from an Excel
#'   file produced by the Li-Cor portable photosynthesis system LI-6800. If leaf area is
#'   provided, the data retrieved will be adjusted accordingly after being recalculated.
#'
#' @param filepath  A path to a Li-Cor produced Excel file
#' @param leafArea_cm2  Numeric value of the sample leaf area (default = NA, which means that no
#'   correction will be applied)
#' @param variables The list of variables to retrieve from the file#' 
#' @param return Other than extracting a dataframe of the selected variables, the function can be
#'   used to extract the index of the first line of data and variable index ("startpos") or the 
#'   variable names present in a data file ("varnames") (default = "dataframe")
#'
#' @return A dataframe with the variables given in arguments
#'
#' @export

get_fromExcel <- function(filepath, 
                          leafArea_cm2 = NA,
                          variables = c("GasEx_A", "GasEx_Ci", "GasEx_gtc", "GasEx_gsw",
                                        "GasEx_TleafCnd","Meas_CO2_r", "Meas_Tleaf", 
                                        "Meas_Tleaf2", "Meas_Qamb_in", "Const_S"),
                          return = "dataframe") {  #startpos, varnames

  wb   <- XLConnect::loadWorkbook(filepath)
  
  row1 <- which(readWorksheet(wb, sheet = 1, startRow = 1, startCol = 1, endCol = 1, 
                              header = FALSE, autofitRow = FALSE)  == 1)
  varnames <- paste0(
    readWorksheet(wb, sheet = 1, startRow = row1 - 3, endRow = row1 - 3, header = FALSE), "_",
    readWorksheet(wb, sheet = 1, startRow = row1 - 2, endRow = row1 - 2, header = FALSE))
  
  if(return == "varnames") data <- varnames
  
  if(return == "startpos") {
    
    data <- c(row1, numeric(length(variables)))
  
    for(i in 1:length(variables)) {
      data[i + 1] <- ifelse(identical(which(variables[i] %in% varnames), integer(0)), NA, 
                        which(varnames == variables[i]))
    }
  }
  
  if(return == "dataframe") {  
    
    stopifnot("At least one variable not in file: check names using return = 'varnames' option" = 
                sum(!variables %in% varnames) == 0)
    
    data <- data.frame()
    
    # Leaf area correction 
    if (!is.na(leafArea_cm2)) { 
      writeWorksheet(wb, data = rep(leafArea_cm2, getLastRow(wb, sheet = 1)), sheet = 1, 
                     startRow = row1, startCol = which(varnames == "Const_S"), header = FALSE)
  
      setForceFormulaRecalculation(wb, sheet = 1, TRUE)    
    }
        
    data <- readWorksheet(wb, sheet = 1, startRow = row1, header = FALSE) %>%
            select(which(varnames %in% variables)) %>%
            set_names(variables)
    
    xlcFreeMemory()  
  }
  
  return(data)
}