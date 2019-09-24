#' best_coefs function
#'
#' @param empty_file Inherited argument from Rapid_aci_correction function
#' @param leafArea_cm2 Leaf area in cm2
#' @param delta_max Inherited argument from Rapid_aci_correction function
#' @param skip_first Inherited argument from Rapid_aci_correction function
#' @param min_CO2 Inherited argument from Rapid_aci_correction function
#' @param max_degree Inherited argument from Rapid_aci_correction function
#' @param diagnostic_plots Inherited argument from Rapid_aci_correction function
#'
#' @return Coefficients from the best 
#'

best_coefs <- function(empty_file,
                       leafArea_cm2,
                       delta_max,
                       skip_first,
                       min_CO2,
                       max_degree,
                       diagnostic_plots) {
  
  
  # SECTION 1 : data acquisition
  ##############################
  
  # Each empty chamber file has its values recalculated after leaf area value (Const_S) is 
  # modified
  if (!is.na(leafArea_cm2)) { 
    df <- getFromExcel(empty_file, leafArea_cm2 = leafArea_cm2,
                       variables = c("GasEx_A", "GasEx_Ci", "GasEx_gtc", "GasEx_gsw", 
                                     "GasEx_TleafCnd", "Meas_CO2_r", "Meas_Tleaf", 
                                     "Meas_Tleaf2", "Meas_Qamb_in"))
    
  # No correction needed to Const_S values
  } else {
    df <- getFromExcel(empty_file, leafArea_cm2 = NA,
                       variables = c("GasEx_A", "GasEx_Ci", "GasEx_gtc", "GasEx_gsw", 
                                     "GasEx_TleafCnd", "Meas_CO2_r", "Meas_Tleaf", 
                                     "Meas_Tleaf2", "Meas_Qamb_in"))
  }

  
  # SECTION 2 : GETTING THE COEFS OF BEST FITTING CURVE
  #####################################################
  
  data <- df %>%
    dplyr::filter(GasEx_A > 1 | GasEx_A < -1) %>%
    mutate(curve = ifelse(GasEx_A < 0, "negative", "positive"), n = 1:n()) %>%
    group_by(curve) %>%
    mutate(delta = GasEx_A - dplyr::lag(GasEx_A)) %>%
    mutate(good = ifelse(delta < delta_max & delta >= -delta_max, 1, 0))

  data <- data[-(1:skip_first),] %>%
    dplyr::filter(!is.na(good))

  # not so sure about this...
  # data_lag <- df %>%
  #   mutate(curve = ifelse(GasEx_A < 0, "negative", "positive"), n = 1:n()) %>%
  #   group_by(curve) %>%
  #   mutate(delta = GasEx_A - dplyr::lag(GasEx_A)) %>%
  #   dplyr::filter(!is.na(delta)) %>%
  #   mutate(lag_select = ifelse(delta < delta_max & delta >= -delta_max, 1, 0)) %>%
  #   ungroup() %>%
  #   mutate(lag_select = ifelse(curve == "negative" &  Meas_CO2_r > min_CO2, 1, lag_select)) %>%
  #   arrange(Meas_CO2_r) %>%
  #   group_by(curve) %>%
  #   mutate(n = cumsum(lag_select)) %>%
  #   dplyr::filter(n < 1) %>%
  #   ungroup() %>%
  #   summarise(lag_between_curves = n() + 2)

  results_p <- vector("list")
  
  pos <- dplyr::filter(data, curve == "positive",  good == 1)
  neg <- dplyr::filter(data, curve == "negative",  good == 1)
    
  if(dim(pos)[1] == 0) {
    results_p[[empty_file]]$positive <- NA
  } else {
    results_p[[empty_file]]$positive <- best_fit(pos, max_degree)
  }
  
  if(dim(neg)[1] == 0) {
    results_p[[empty_file]]$negative <- NA
  } else {
    results_p[[empty_file]]$negative <- best_fit(neg, max_degree)
  }
    
  results_p[[empty_file]]$empty_data <- df
  #results_p[[empty_file]]$lag  <- data_lag$lag_between_curves
  return(results_p)
    
    
  # SECTION 3 : DIAGNOSTIC PLOTS
  ##############################

  if(diagnostic_plots == TRUE) {
    p1 <-
      ggplot(data, aes(x = n, y= delta, group = curve, color = curve)) + 
       geom_point() +
       geom_hline(aes(yintercept = -delta_max)) + 
       geom_hline(aes(yintercept =  delta_max)) +
       labs(title = paste("Selected points (delta_max =", delta_max, ")")) +
       theme(legend.position = "none")
    
    p2 <-
      ggplot(data, aes(x = Meas_CO2_r, y = GasEx_A, color = as.factor(good))) + 
       geom_point() +
       labs(title = paste("Portion of the empty chamber curve used (in blue)")) +
       theme(legend.position = "none")
    
    deg <- length(results_p[[1]]$positive) - 1
    if(deg > 0){
      if(deg==1){oi<-"st"}else if(deg==2){oi<-"nd"}else if(deg==3){oi<-"rd"}else{oi<-"th"}
      p3 <-
        ggplot(pos, aes(x = Meas_CO2_r, y = GasEx_A)) + 
         geom_point() +
         geom_smooth(method='lm',formula = y~x + poly(x, deg), color = "green2", se = FALSE) +
         labs(title = paste("Best fitting on positive curve :", paste0(deg, oi), "degree"))
    } else {p3 <- NULL}
    
    dog <- length(results_p[[1]]$negative) - 1
    if(dog > 0){
      if(dog==1){oi<-"st"}else if(dog==2){oi<-"nd"}else if(dog==3){oi<-"rd"}else{oi<-"th"}
      p4 <-
        ggplot(neg, aes(x = Meas_CO2_r, y = GasEx_A)) + 
         geom_point() +
         geom_smooth(method='lm',formula=y~x + poly(x, dog), color = "green1", se = FALSE) +
         labs(title = paste("Best fitting on negative curve :", paste0(dog, oi), "degree"))
    } else {p4 <- NULL}
    
    plist <- list(p1, p2, p3, p4)

    png(paste0(str_split(empty_file, "\\.", n = 2, simplify = TRUE)[1], ".png"))
    #suppressWarnings(
    gridExtra::grid.arrange(grobs = plist[-which(sapply(plist, is.null))],
                            layout_matrix = rbind(c(1, 2), c(3, 4)))
    #)
    dev.off()
  }
}


