#' @export
returns_hist <- function(results_df){
  
  M.1.1 <- min(results_df$strategy_roi)
  M.1.2 <- max(results_df$strategy_roi)
  
  M.2.1 <- min(results_df$market_roi)
  M.2.2 <- max(results_df$market_roi)
  
  p1 <- 
    ggplot(data = results_df, aes(x = strategy_roi)) + 
    theme_classic() + 
    geom_histogram(bins = 25, color = "grey", fill = "lightgrey") + 
    geom_vline(xintercept = 1, color = "red", linewidth = 1, linetype = "dashed") + 
      
    scale_x_continuous(limits = c(M.1.1, M.1.2), 
                       breaks =  seq(from = M.1.1, to = M.1.2, length.out = 5), 
                       labels = function(x){scales::percent(x-1, 0.1)} 
                       ) + 
    
    labs(x = "Returns", 
         title = paste0(
           "Strategy. Positives: ", 
           length(which(results_df$strategy_roi > 1)), 
           "(", 
           round(length(which(results_df$strategy_roi > 1))/nrow(results_df), 3)*100, 
           "%)"
           )
    )
  
  p2 <- 
    ggplot(data = results_df, aes(x = market_roi)) + 
    theme_classic() + 
    geom_histogram(bins = 25, color = "grey", fill = "lightgrey") + 
    geom_vline(xintercept = 1, color = "red", linewidth = 1, linetype = "dashed") + 
      
    scale_x_continuous(limits = c(M.2.1, M.2.2), 
                       breaks =  seq(from = M.2.1, to = M.2.2, length.out = 5), 
                       labels = function(x){scales::percent(x-1, 0.1)} 
                       ) + 
    
    labs(x = "Returns", 
         title = paste0(
           "Market Positives: ", 
           length(which(results_df$market_roi > 1)), 
           "(", 
           round(length(which(results_df$market_roi > 1))/nrow(results_df), 3)*100, 
           "%)"
           )
         )
    
    return(gridExtra::grid.arrange(p1, p2, nrow = 1))
}