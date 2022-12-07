# Purpose of script:
# to handle loading and processing of data

# data sources:

library(tidyverse)
library(janitor)
library(lubridate)
library(ckanr)
# library(arrow)

# DVRPC crash data: https://catalog.dvrpc.org/dataset/crash-summary-and-fatal-counts
# DVRPC population forecast data: https://catalog.dvrpc.org/dataset/adopted-2050-population-employment-forecasts-zonal-data

# define CKAN connection
ckan_con <- src_ckan("https://catalog.dvrpc.org/")

# read in crash data and metadata
crash_resource <- "ce75c010-3a79-4a67-b7b6-0e16fb83edaf"
crash_meta <- resource_show(id = crash_resource, as = "table") # meta data
crash_last_mod <- crash_meta$last_modified

crash_data <- tbl(src = ckan_con$con, from = crash_resource) %>% 
  as_tibble() %>% 
  select(-'_full_text')

# read in pop data and metadata
pop_resource <- "edbf137e-acc1-4543-8f71-c7ecbae67951"
pop_meta <- resource_show(id = pop_resource, as = "table")
pop_last_mod <- pop_meta$last_modified

pop_data <- tbl(src = ckan_con$con, from = pop_resource) %>% 
  as_tibble() %>% 
  select(-'_full_text') %>% 
  mutate(GEOID10 = as.character(`mcd/cpa id`))

# save a parquet table of some data / metadata / max year of data to check against later

# running the report...

# join datasets
# fix names further to jive with each other...
# or downselect / rename to exactly what you need
test <- crash_data %>% 
  left_join(pop_data %>% 
              select('county id':'2015', 'GEOID10'),
            by = "GEOID10")

# Report contents:
#   
#   1. simple tables of most crashes, most fatalities, faceting, etc.
#   2. simple map of same information?

# data checking considerations
## not just when data is "new", but when it actually has additional rows that are not present in previous version

if(data_not_new){
  text_to_return <- "There has been no data updates, and thus no new report has been generated."
}
