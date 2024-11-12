#' @export
returns_density <- function(results_df){
  
  M_ <- 
    min(
      min(results_df$market_roi), 
      min(results_df$strategy_roi)
    )
  
  M_2 <- 
    max(
      max(results_df$market_roi), 
      max(results_df$strategy_roi)
    )
  
  m1 <- max((density(results_df$market_roi)$y))
  m2 <- max((density(results_df$strategy_roi)$y))
  mm <- max(m1, m2)
  
  m1 <- min((density(results_df$market_roi)$y))
  m2 <- min((density(results_df$strategy_roi)$y))
  mm2 <- min(m1, m2)
  
  ggplot(data = results_df) + 
    theme_classic() + 
    geom_density(aes(x = strategy_roi, color = "Strategy ROI"), alpha = 0.5, linewidth = 1)+ 
    geom_density(aes(x = market_roi, color = "Market ROI"), alpha = 0.5, linewidth = 1) + 
    theme(legend.position = "bottom") + 
    
    geom_vline(xintercept = 1, color = "black", linetype = "dashed") + 
    
    labs(x = "Returns", 
         color = "") + 
    
    scale_x_continuous(limits = c(M_, M_2), 
                       breaks = seq(from = M_, to = M_2, length.out = 5), 
                       labels = function(x){scales::percent(x-1, 0.1)} 
                       ) 
  
}