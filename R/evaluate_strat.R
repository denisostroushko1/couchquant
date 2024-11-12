#' @export
evaluate_strat <- 
  function(data_in, 
           budget, 
           flat_buy_percent, 
           dynamic = F
           ){
    
    data_work <- data_in
    ## loop and collect data on trades: 
    ## roi 
    ## id trades 
    data_work$trade_roi = NA
    trade_id = 1
    for(i in 1:nrow(data_work)){
      if(data_work$decisions[i] == "buy"){last_buy <- data_work$close[i]}
      
      data_work$trade_roi[i] <- data_work$close[i]/last_buy
      
      if(data_work$decisions[i] == "sell"){last_buy <- NA}
    }
    
    
    results_data <- 
      data_work %>% 
      select(datetime, close, decisions) %>% 
      mutate(id = 1:nrow(.))
    
    results_data$current_cash <- NA
    results_data$current_cash[1] <- budget
    results_data$current_asset <- NA
    results_data$running_roi <- NA
    results_data$total_value <- NA
    results_data$asset_value <- NA
    #       results_data %>% head()
    
    for(i in 1:nrow(results_data)){
      
      if(results_data$decisions[i] == "buy" ){
        
        purchase_price <- results_data$close[i]
        
        #####
        ## as our value grows, maybe we tank to 
        ## take bigger bets
        
        ## simple step: take running ROI * flat_buy_percent
        if(dynamic == F){
          buy_percent <- flat_buy_percent
        }
        if(dynamic == T){
          buy_percent <- flat_buy_percent * results_data$running_roi[i] 
        }
        
        if(i == 1){
    
          puchase_amount <- flat_buy_percent * results_data$current_cash[i]/purchase_price
          results_data$current_cash[i] <- results_data$current_cash[i] - flat_buy_percent * results_data$current_cash[i]
          results_data$current_asset[i] <- puchase_amount
        }else{
    
          puchase_amount <- flat_buy_percent * results_data$current_cash[i-1]/purchase_price
          results_data$current_cash[i] <- results_data$current_cash[i-1] - flat_buy_percent * results_data$current_cash[i-1]
          results_data$current_asset[i] <- puchase_amount
          
        }
      }
      
      if(results_data$decisions[i] == "sell" ){
        
        sell_price <- results_data$close[i]
        sell_amount <- puchase_amount * sell_price
        
        results_data$current_cash[i] <- results_data$current_cash[i-1]  + sell_amount
        results_data$current_asset[i] <- 0
      }
      
      if(results_data$decisions[i] == '' | results_data$decisions[i] == 'nothing' ){
        results_data$current_cash[i] <- results_data$current_cash[i-1]
        results_data$current_asset[i] <- results_data$current_asset[i-1]
      }
      
      
    ## making these calculations inside the loop so that we 
    ## can enable dynamic trading 
    results_data$asset_value[i] <- with(results_data[i,], close * current_asset)
    results_data$total_value[i] <- with(results_data[i,], asset_value + current_cash)
    results_data$running_roi[i] <- with(results_data[i,], total_value / budget)
      
    }
    
    results_data$market_roi <- results_data$close / results_data$close[1]
    
    return(results_data)
}