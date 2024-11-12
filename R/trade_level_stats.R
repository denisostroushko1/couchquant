#' @export
trade_level_stats <- function(data_in){
  data_work <- data_in 
  
  data_work$trade_roi = NA
  data_work$trade_id = NA
  
  trade_id = 1
  
  for(i in 1:nrow(data_work)){
    if(data_work$decisions[i] == "buy"){
      last_buy <- data_work$close[i]
      flag <- 1
      }
  
    data_work$trade_roi[i] <- data_work$close[i]/last_buy
    data_work$trade_id[i] <- trade_id * flag
    
    if(data_work$decisions[i] == "sell"){
      
      trade_id = trade_id + 1
      last_buy <- NA
      flag <- NA
      }
  }
  
  df_return <- 
    inner_join(
      x = 
        data_work %>% 
        filter(decisions == "buy") %>% select(trade_id, datetime) %>% 
        rename(date_start = datetime), 
    
      y = 
      data_work %>% 
        filter(decisions == "sell") %>% 
        select(trade_id, datetime, trade_roi) %>% 
        rename(date_end = datetime), 
      
      by = 'trade_id'
    )

  
  
  return(df_return)
}