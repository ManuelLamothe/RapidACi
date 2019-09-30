#' diagnose_sample fucntion
#'
#' @param results 
#'
#' @return
#' @export
#'
#' 
#  
# diagnose_sample(results2, "SAB_513")
# delta_max = 0.05
# sample_name = "SAB_513"
# par(mfrow = c(2,2))
# results <- result_list


diagnose_sample <- function(results, sample_name){

  data <- results[[sample_name]]

 raw1 <- 
    ggplot(data$ACi_data, aes(GasEx_Ci, GasEx_A, color = directn)) +
      geom_point() +
      labs(title = paste("Raw A-Ci :", sample_name)) +
      theme(legend.position = "right")
  
 raw2 <- 
    ggplot(data$ACi_data, aes(Meas_CO2_r, GasEx_A, color = directn)) +
      geom_point() +
      labs(title = paste("Raw A-Ci :", sample_name)) +
      theme(legend.position = "right")
    
 raw3 <-     
    ggplot(data$ACi_data, aes(x = n, y= deltaA, color = directn)) + 
      geom_point() +
      geom_hline(aes(yintercept = -delta_max)) + 
      geom_hline(aes(yintercept =  delta_max)) +
      labs(title = paste("Selected points (delta_max =", delta_max, ")")) +
      theme(legend.position = "none")
    
 empty1 <-
    ggplot(data$empty_chamber_data, aes(x = Meas_CO2_r, y = GasEx_A, color = good)) + 
      geom_point() +
      labs(title = paste("Portion of the empty chamber curve used (in blue)")) +
      theme(legend.position = "none")
    
    
    
    
    
    
    
 # # Empty2
 #    deg <- length(dplyr::filter(data, directn == "positive")) - 1
 #    if(deg > 0){
 #      if(deg==1){oi<-"st"}else if(deg==2){oi<-"nd"}else if(deg==3){oi<-"rd"}else{oi<-"th"}
 #      p3 <-
 #        ggplot(pos, aes(x = Meas_CO2_r, y = GasEx_A)) + 
 #        geom_point() +
 #        geom_smooth(method='lm',formula = y~x + poly(x, deg), color = "green2", se = FALSE) +
 #        labs(title = paste("Best fitting on positive curve :", paste0(deg, oi), "degree"))
 #    } else {p3 <- NULL}
 #    
 #  # Empty3
 #    dog <- length(results_p[[1]]$negative) - 1
 #    if(dog > 0){
 #      if(dog==1){oi<-"st"}else if(dog==2){oi<-"nd"}else if(dog==3){oi<-"rd"}else{oi<-"th"}
 #      p4 <-
 #        ggplot(neg, aes(x = Meas_CO2_r, y = GasEx_A)) + 
 #        geom_point() +
 #        geom_smooth(method='lm',formula=y~x + poly(x, dog), color = "green1", se = FALSE) +
 #        labs(title = paste("Best fitting on negative curve :", paste0(dog, oi), "degree"))
 #    } else {p4 <- NULL}  
    
 # Corr1
    
 # Corr2

  plist <- c(raw1, raw2, raw3, empty1)
  png(sample_name)
  gridExtra::grid.arrange(grobs = plist[-which(sapply(plist, is.null))],
                          layout_matrix = rbind(c(1, 2), c(3, 4)))
  dev.off()
  
}  
