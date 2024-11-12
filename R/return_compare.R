#' @export
return_compare <- function(results_df){
  
  M.1.1 <- min(results_df$strategy_roi)
  M.1.2 <- max(results_df$strategy_roi)
  
  M.2.1 <- min(results_df$market_roi)
  M.2.2 <- max(results_df$market_roi)
  
  ggplot(data = results_df, 
         aes(x = market_roi, strategy_roi)) + 
    theme_classic() + 
    
    geom_vline(xintercept = 1, color = "black", linetype = "dashed") + 
    geom_hline(yintercept = 1, color = "black", linetype = "dashed") + 
    geom_abline(color = "red", linetype = "dashed", intercept = 0, slope = 1) + 
    
    geom_point(aes(color = market_strat_cor)) + 
    # geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") + 
    
    scale_color_gradientn(colors = rainbow(5)  , 
                          breaks = seq(-1, 1, by = 0.5),
                          limits = c(-1, 1)
                          ) + 
    
    geom_smooth(se = F) + 
    
    labs(x = "Market Returns", 
         y = "Strategy Returns", 
         color = "Market-Strategy Correlation", 
         title = paste0(
           "Pairwise - Strategy > Market : \n   ", 
           length(which(results_df$strategy_roi > results_df$market_roi)), 
           " cases (", 
           round(length(which(results_df$strategy_roi > results_df$market_roi))/nrow(results_df), 3)*100, "%", 
           ")"
           )
         ) + 
    
    scale_y_continuous(limits = c(M.1.1, M.1.2), 
                       breaks = seq(from = M.1.1, to = M.1.2, length.out = 5), 
                       labels = function(x){scales::percent(x-1, 0.1)} 
                       ) + 
    
    scale_x_continuous(limits = c(M.2.1, M.2.2), 
                       breaks = seq(from = M.2.1, to = M.2.2, length.out = 5), 
                       labels = function(x){scales::percent(x-1, 0.1)} 
                       ) + 
    
    theme(legend.position = "bottom") + 
    
    guides(color = guide_colourbar(
      barwidth = 20,        # Length of the color bar
      barheight = 0.5       # Height of the color bar
    ))
}
