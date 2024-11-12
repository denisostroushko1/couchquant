#' @export
get_dates <- function(data_in, days_long){
  
    I_ <- sample(days_long:nrow(data_in), size = 1)
    
    END_DATE  <- data_in[I_,]$datetime
    START_DATE <- END_DATE - as.difftime(days_long, units = "days") 
    
    ret_ <- c(START_DATE, END_DATE)
    names(ret_) <- c("start", "end")
    return(ret_)
}