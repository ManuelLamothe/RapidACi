#' best_coefs function
#'
#' @param list_files
#' @param delta_max
#' @param skip_first
#' @param min_CO2
#' @param max_degree
#' @param diagnostic_plots
#' @param curve_plot
#'
#' @return
#'
#' @examples

best_coefs <- function(list_files,
                       delta_max,
                       skip_first,
                       min_CO2,
                       max_degree,
                       diagnostic_plots,
                       curve_plot) {

  results_p <- vector("list")

  for(i in 1:length(list_files)) {
    suppressMessages(names <- readxl::read_excel(list_files[i], skip = 14) %>% names())
    data_full <- readxl::read_excel(list_files[i], skip = 16, col_names = names, 
                                    sheet = 1)

    
    ## HERE: if not opened/saved, the cells with formulas will get a value of 0
    data <- data_full %>%
      dplyr::select(CO2_r, A) %>%
      dplyr::filter(A > 1 | A < -1) %>%
      mutate(curve = ifelse(A <= 0, "negative", "positive"), 
             n = 1:n()) %>%
      group_by(curve) %>%
      mutate(delta = A - dplyr::lag(A)) %>%
      mutate(good = ifelse(delta < delta_max & delta >= -delta_max, 1, 0))

    data <- data[-(1:skip_first),] %>%
      dplyr::filter(!is.na(good))

    data_lag <- data_full %>%
      select(CO2_r, A) %>%
      mutate(curve = ifelse(A <= 0, "negative", "positive"), n = 1:n()) %>%
      group_by(curve) %>%
      mutate(delta = A - dplyr::lag(A)) %>%
      dplyr::filter(!is.na(delta)) %>%
      mutate(lag_select = ifelse(delta < delta_max & delta >= -delta_max, 1, 0)) %>%
      ungroup() %>%
      mutate(lag_select = ifelse(curve == "negative" &  CO2_r > min_CO2, 1, lag_select)) %>%
      arrange(CO2_r) %>%
      group_by(curve) %>%
      mutate(n = cumsum(lag_select)) %>%
      dplyr::filter(n < 1) %>%
      ungroup() %>%
      summarise(lag_between_curves = n() + 2) 

    if(diagnostic_plots) {
      print(
        ggplot(data, aes(x = n, y= delta, group = curve, color = curve)) + geom_point() +
          geom_hline(aes(yintercept = -delta_max)) + geom_hline(aes(yintercept = delta_max)) +
          labs(title = paste("Plot 1 : Viewing the selected delta : ", list_files[i]))
      )
      print(
        ggplot(data, aes(x = CO2_r, y = A, color = as.factor(good))) + geom_point() +
          labs(title = paste("Plot 2 : Portion of curve analyzed :", list_files[i]))
      )
    }

    pos <- dplyr::filter(data, curve == "positive",  good == 1)
    neg <- dplyr::filter(data, curve == "negative",  good == 1)

    if(curve_plot) {
      print(
        ggplot(pos, aes(x = CO2_r, y = A)) + geom_point() +
          geom_smooth(method='lm',formula=y~x, color = "red", se = FALSE) +
          geom_smooth(method='lm',formula=y~x + poly(x, 2), color = "green", se = FALSE) +
          geom_smooth(method='lm',formula=y~x + poly(x, 3), color = "blue", se = FALSE) +
          labs(title = paste("Plot 3 : Positive curve :", list_files[i]))
      )
      print(
        ggplot(neg, aes(x = CO2_r, y = A)) + geom_point() +
          geom_smooth(method='lm',formula=y~x, color = "red", se = FALSE) +
          geom_smooth(method='lm',formula=y~x + poly(x, 2), color = "green", se = FALSE) +
          geom_smooth(method='lm',formula=y~x + poly(x, 3), color = "blue", se = FALSE) +
          labs(title = paste("Plot 4 : Negative Curve :", list_files[i]))
      )
    }


    if(dim(pos)[1] == 0) {
      results_p[[list_files[i]]]$positive <- NA
    } else {
      results_p[[list_files[i]]]$positive <- best_fit(pos, max_degree)
    }
    if(dim(neg)[1] == 0) {
      results_p[[list_files[i]]]$negative <- NA
    } else {
      results_p[[list_files[i]]]$negative <- best_fit(neg, max_degree)
    }


    results_p[[list_files[i]]]$lag <- data_lag$lag_between_curves
  }
  return(results_p)
}
