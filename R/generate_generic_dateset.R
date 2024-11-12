#' @export
generate_generic_dateset<- 
  function(
    volatility_distr, ## 
     time_unit_length, ## 
     N_time_units, ## 
     starting_price, ## 
     start_date_time  ## 
  ){
    
    ## take care of dates 
    if(time_unit_length == "day"){
      start_date_time <- as.Date(start_date_time)
    }
    
    if(time_unit_length == "day"){
        date_times <- seq.Date(from = start_date_time, by = "day", length.out = N_time_units)
    }
    if(time_unit_length == "hour"){
        date_times <- seq.POSIXt(from = start_date_time, by = "hour", length.out = N_time_units)
    }
    
    ## generate volatility vector for this case 
    sampled_vol <- sample(volatility_distr, size = N_time_units, replace = T)
    
    ## generate prices 
    prices = rep(NA, N_time_units)
    prices[1] <- starting_price
    for(i in 2:N_time_units){
      prices[i] <- prices[i-1] * sampled_vol[i]
    }
    
    ## final_data
    final_data <- 
      data.frame(
        datetime = date_times, 
        close = prices 
      )
    
    return(final_data)
  }
