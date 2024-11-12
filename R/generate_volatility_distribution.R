#' @export
generate_volatility_distribution <- 
  function(
    sample_from_data
    ){
    
    if (!is.list(sample_from_data)) {
      stop("Error: Argument 'sample_from_data' is not a list.")
    }
    
    volatility_ <- c()
    
    for( i in 1:length(sample_from_data)){
      sample_from_data[[i]] %>% 
        mutate(volatility = close / lag(close)) %>% 
        dplyr::select(volatility) %>% 
        na.omit() %>% 
        unlist() -> seq_1
      
      volatility_ <- c(volatility_, seq_1 )
    }
    
    return(volatility_)
    
  }
