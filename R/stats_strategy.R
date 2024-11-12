#' @export
stats_strategy <-
  function(results_data){
  data.frame(
    from_datetime = results_data$datetime %>% min(), 
    to_datetime = results_data$datetime %>% max(), 
    
    market_roi = results_data$close %>% tail(1)/ results_data$close %>% head(1), 
    strategy_roi = results_data$running_roi %>% tail(1), 
    
    market_top_roi = results_data$market_roi %>% max(), 
    market_bot_roi = results_data$market_roi %>% min(), 
    
    strategy_top_roi = results_data$running_roi %>% max(),
    strategy_bot_roi = results_data$running_roi %>% min(), 
    
    time_u_market_roi_pos =   length(which(results_data$market_roi > 1)),
    time_u_strategy_roi_pos =   length(which(results_data$strategy_roi > 1)), 
    
    market_strat_cor = with(results_data, cor(market_roi,running_roi ) )
    )
  }