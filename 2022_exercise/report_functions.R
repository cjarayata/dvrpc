# pull number function
get_topline_numbers <- function(df, municipality_of_interest, variable){
  df %>% 
    filter(Municipality_County == municipality_of_interest) %>% 
    pull( {{variable}} )
    
}

retrieve_summary_info <- function(per_capita_data, crash_data, var_to_sort, recent_year){
  
  top_municipality <- per_capita_data %>% 
    filter(`Crash Year` == recent_year) %>% 
    arrange(desc( {{ var_to_sort}} )) %>% 
    head(1) %>% 
    pull(Municipality_County)
  # browser()
  
  pop <- get_topline_numbers(crash_data %>% filter(`Crash Year` == recent_year),
                    top_municipality,
                    "2015")
  crashes <- get_topline_numbers(crash_data %>% filter(`Crash Year` == recent_year),
                    top_municipality,
                    "TOTAL CRASH")
  people <- get_topline_numbers(crash_data %>% filter(`Crash Year` == recent_year),
                    top_municipality,
                    "TOTAL INJURED KILLED")
  vulnerable <- get_topline_numbers(crash_data %>% filter(`Crash Year` == recent_year),
                    top_municipality,
                    "TOTAL VULNERABLE CRASHES")
  
  list_output <- list(top_municipality, pop, crashes, people, vulnerable)
  names(list_output) <- c("municipality", "pop", "crashes", "people", "vulnerable")
  return(list_output)
  
}
