# Purpose of script:
# to handle loading and processing of data

# knitr set-up 
library(tidyverse)
library(janitor)
library(lubridate)
library(ckanr)
library(here)
library(arrow)
library(gt)
library(glue)

# data sources:
# DVRPC crash data: https://catalog.dvrpc.org/dataset/crash-summary-and-fatal-counts
# DVRPC population forecast data: https://catalog.dvrpc.org/dataset/adopted-2050-population-employment-forecasts-zonal-data

cache_dataset_details <- read_parquet(here::here("2022_exercise", "cache_data_details.parquet"))

# this will be useful to put somewhere in the pipeline...
if(max(cache_dataset_details$last_mod) == max(dataset_details$last_mod)){
  message("Current version of data is up-to-date. No new report will be generated.")
  # return(TRUE)
} else {
  message("Newer version of data is available. Running new report.")
}

# data checking considerations
## not just when data is "new", but when it actually has additional rows that are not present in previous version


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
  mutate(County = str_remove_all(County, " County"))

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

# save a parquet table of some data / metadata / max year of data to check against later
dataset_details <- tibble(date_written = today(),
                          data_name = c("crash", "population"),
                          num_rows = c(nrow(crash_data), nrow(pop_data)),
                          last_mod = c(crash_last_mod, pop_last_mod),
                          max_crash_year = max(crash_data$`Crash Year`))

file_name <- "cache_data_details.parquet"
write_parquet(dataset_details, here::here("2022_exercise", file_name))

# running the report...

# join datasets
# fix names further to jive with each other...
# or downselect / rename to exactly what you need
# then, divide all crash cols by 2015 population
per_capita_data <- crash_data %>% 
  left_join(pop_data %>% 
              select('county id':'2015', 'GEOID10')) %>% 
  mutate_at(vars(`TOTAL CRASH`:`BICYCLISTS KILLED`), ~(. / `2015`))

# will have to think of some interesting groupings

# Report contents:
#   
#   1. simple tables of most crashes, most fatalities, faceting, etc.
#   2. simple map of same information?

# knitr

# gt table function

