#' @export
assign_decisions <- 
  function(data_in, 
           decision_function, 
           stop_loss){
    
    data_out <- data_in 
    # vectors to help store values as we loop 
    decisions_vector  <- subset <- rep('', nrow(data_out)) 
    l_ <- last_trade <- ''
    last_buy_index <- 0
    
    ## loop over the data set to make sequential decisions  
    for(i in 1:nrow(data_out)){
      ##print(paste0(i, " of ", nrow(data_out)))
      if(i == 1){
        # on the first pass we just need to make a decision 
        decisions_vector[i] <- decision_function(day_data = data_in[i, ])
        }
    
      if(i != 1){
        ## this is a list of decisions that we have made so far 
        subset <- decisions_vector[1:i]
        # this is a list of buy or sell decisions 
        l_ <- subset[!(subset %in% c('', ' '))]
        
        if(length(l_) == 0){l_ = ''}
        # information about the last trade we made: either buy of sell 
        last_trade <- l_[length(l_)]
        # index in the loop when we last bought something 
        #   using this for running ROI of a given trade, and thus 
        #   used for a stop loss decision 
        if(length(which(decisions_vector == "buy")) != 0){
          last_buy_index <- which(decisions_vector == "buy") %>% max()
        }
        
        if(length(data_in$close[last_buy_index]) == 0){
          checking_price = data_in$close[i]
        }else{
            checking_price = data_in$close[last_buy_index]
          }
        ## actually make a decision 
        decisions_vector[i] <- 
          ifelse(
            # if we last bought and the running ROI of a trade is below the 
            # stop loss floor then we sell 
            last_trade == "buy" & 
              data_in$close[i]/ checking_price <= 1-stop_loss, 
            
            "sell", 
            # otherwise if we decide to do the same thing as our last trade, we do nothing
            ifelse(decision_function(day_data = data_in[i, ]) ==  last_trade, 
                   # do nothing 
                   '', 
                   # otherwise, the last option is either to buy or to sell 
                   decision_function(day_data = data_in[i, ]) 
                   )
          )
      }
    }
    data_out$decisions <- decisions_vector

    # finally, we need to chop off the first set of days before we actually buy 
    # sometimes we work with the data when the first decision is to sell
    # usually, we only need to take off like 10-20 time units 
    data_out <- 
      data_out %>% 
      filter(datetime >= data_out %>% filter(decisions == "buy") %>% select(datetime) %>% unlist() %>% min())
      
    return(data_out)
  }