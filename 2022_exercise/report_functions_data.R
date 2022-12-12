# A collection of data loading and some functions

library(tidyverse)
library(janitor)
library(lubridate)
library(ckanr)
library(here)
library(arrow)
library(gt)
library(glue)
library(ggplot2)
library(quarto)

# load current-state data ####
# define/set up CKAN connection
ckan_con <- src_ckan("https://catalog.dvrpc.org/")
ckanr_setup("https://catalog.dvrpc.org/")

# read in crash data and metadata
crash_resource <- "ce75c010-3a79-4a67-b7b6-0e16fb83edaf"
crash_meta <- resource_show(id = crash_resource, as = "table") # meta data
crash_last_mod <- ymd_hms(crash_meta$last_modified)

crash_data <- tbl(src = ckan_con$con, from = crash_resource) %>% 
  as_tibble() %>% 
  select(-c('_full_text', '_id')) %>% 
  rename("Municipality" = "MCD Name") %>% 
  mutate(County = str_remove_all(County, " County"),
         `TOTAL INJURED KILLED` = `TOTAL KILLED` + `TOTAL INJURED`,
         `TOTAL VULNERABLE CRASHES` = `PEDESTRIAN COUNT` + `BICYCLE COUNT`,
         Municipality_County = glue("{Municipality} ({County})"))

# read in pop data and metadata
pop_resource <- "edbf137e-acc1-4543-8f71-c7ecbae67951"
pop_meta <- resource_show(id = pop_resource, as = "table")
pop_last_mod <- ymd_hms(pop_meta$last_modified)

pop_data <- tbl(src = ckan_con$con, from = pop_resource) %>% 
  as_tibble() %>% 
  select(-c('_full_text', '_id')) %>% 
  rename("Municipality" = "Municipality or District",
         "GEOID10" = "mcd/cpa id") %>% 
  mutate(GEOID10 = as.character(GEOID10))

# create a summary table of today's current state of the data
new_data_details <- tibble(date_written = today(),
                          data_name = c("crash", "population"),
                          num_rows = c(nrow(crash_data), nrow(pop_data)),
                          last_mod = c(crash_last_mod, pop_last_mod),
                          max_crash_year = max(crash_data$`Crash Year`))

# functions ####
# create a go-no-go decision by comparing cache vs. new data on several parameters,
# namely the max(last_mod_date), max(num_rows), or max(crash_year)
check_cache_vs_current_data <- function(cache, new){
  
  if(identical(cache %>% select(-date_written),
          new %>% select(-date_written))){
    run_report <- FALSE
  }
  if(max(new$last_mod) > max(cache$last_mod)){
    run_report <- TRUE
    message("Data is out of date: Crash or population data has been modified since cache file was last saved.")
  }
  if(max(new$num_rows) > max(cache$num_rows)){
    run_report <- TRUE
    message("Data is out of date: Newer version of crash or population data has more rows than existing cache file.")
  }
  if(max(new$max_crash_year) > max(cache$max_crash_year)){
    run_report <- TRUE
    message("Data is out of date: Newer version of crash data has more recent years of data than existing cache file.")
  }
  
  return(run_report)
}

# pull number function
get_topline_numbers <- function(df, municipality_of_interest, variable){
  df %>% 
    filter(Municipality_County == municipality_of_interest) %>% 
    pull( {{variable}} )
}

# find the top municipality that has the most [variable] per capita,
# then pull the 2015 population, number of crashes, number of people injured/killed
# and number of crashes involving vulnerable road users
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
  
  # save as a list so we can return multiple values that are easily retrievable
  list_output <- list(top_municipality, pop, crashes, people, vulnerable)
  names(list_output) <- c("municipality", "pop", "crashes", "people", "vulnerable")
  return(list_output)
  
}