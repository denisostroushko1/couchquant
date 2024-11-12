#' @export
plot_timeframe <- 
  function(results_data, log_scale = "Y"){
    
    add_title = ifelse(log_scale == "Y", ". Log Scale", "")
    
    p <- 
      ggplot(data = results_data) + 
        theme_classic() + 
        geom_line(aes(x = datetime, y = running_roi, color = "Strategy ROI")) + 
        geom_line(aes(x = datetime, y = market_roi, color = "Market ROI")) + 
        theme(legend.position = "bottom") +  
        
        geom_hline(yintercept = 1, color = "black", linetype = "dashed") + 
        labs(color = '', 
             y = paste0('Running ROI', add_title), 
             title = paste0("From ", 
                            as.Date(min(results_data$datetime)), 
                            " to ", 
                            as.Date(max(results_data$datetime)), 
                            
                            "\n Return over ", as.numeric(as.Date(max(d2$datetime)) - as.Date(min(d2$datetime))), " days: ", 
                            round(results_data$running_roi[nrow(results_data)]-1, 3)*100, "%", 
                            
                            "\n Market Return ", as.numeric(as.Date(max(d2$datetime)) - as.Date(min(d2$datetime))), " days: ", 
                            round(results_data$market_roi[nrow(results_data)]-1, 3)*100, "%"
                            )
             )
    
    if(log_scale == "Y"){
      p_f <- p + scale_y_continuous(transform = "log", labels = function(x){scales::percent(x-1, 0.1)}) 
    }
    
    if(log_scale != "Y"){
      p_f <- p + scale_y_continuous( labels = function(x){scales::percent(x-1, 0.1)}) 
    }
    
    return(p_f)

  }