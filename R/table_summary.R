#' @export
table_summary <- function(results_df){
    
  results_df %>% 
    mutate(
      type = ifelse(market_roi > 1, "Market Up",  "Market Down")
    ) %>% 
    group_by(type) %>% 
    summarise(n = n(), 
              p_strategy_up = sum(ifelse(strategy_roi > 1, 1, 0))/n(), 
              
              Avg_return = mean(strategy_roi), 
              Median_return = median(strategy_roi), 
              SD_returns = sd(strategy_roi), 
              
              Best_return = max(strategy_roi),
              Worst_return = min(strategy_roi)
              ) 
}