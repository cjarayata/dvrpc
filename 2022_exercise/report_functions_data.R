# A collection of functions and some data loading

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

# define resource IDs
crash_resource <- "ce75c010-3a79-4a67-b7b6-0e16fb83edaf"
pop_resource <- "edbf137e-acc1-4543-8f71-c7ecbae67951"

###############################
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
retrieve_summary_info <- function(per_capita_data, crash_data, variable, recent_year){
  
  top_municipality <- per_capita_data %>% 
    filter(`Crash Year` == recent_year) %>% 
    arrange(desc( {{ variable}} )) %>% 
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

# ggplot function
make_faceted_ggplot <- function(df, variable){
  df %>% 
    ggplot(aes(x = `Crash Year`, y = {{variable}}, color = Municipality)) +
    geom_line() +
    theme_classic() +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = seq(2008, 2020, 4)) + # note that as data gets 'newer' this will need tweaking...
    facet_wrap(~County, scales = "free_y")
}

# gt function
make_nice_gt <- function(df, recent_year, variable, ...){
  df %>% 
    filter(`Crash Year` == recent_year) %>% 
    group_by(County) %>% 
    slice_max(order_by = {{variable}}, n = 1) %>% 
    ungroup() %>% 
    select(County, Municipality, {{variable}}, `2015`) %>% 
    arrange(desc({{variable}})) %>% 
    gt() %>% 
    tab_header(...,
             subtitle = "Top Municipalites by County") %>% 
    fmt_number(columns = {{variable}},
               decimals = 3) %>%
    fmt_number(columns = `2015`, use_seps = T, decimals = 0) %>% 
    tab_source_note("Source: DVPRC Data Catalog") %>% 
    tab_style(
      locations = cells_column_labels(columns = everything()),
      style = list(
        cell_borders(sides = "bottom"),
        cell_text(weight = "bold")
      )
    ) %>% 
    tab_style(locations = cells_title(groups = "title"),
              style = list(cell_text(weight = "bold"))) %>% 
    cols_label(
      `2015` = "2015 Estimated Population"
    )
}

###############################
## load current-state data ####
# define/set up CKAN connection
ckan_con <- src_ckan("https://catalog.dvrpc.org/")
ckanr_setup("https://catalog.dvrpc.org/")

# read in crash data and metadata

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

###############################
## further clean data ####
# the raw data needs a bit of cleaning:
# two municipalities seem to have very low population; filter to only places
# with at least 100 projected residents in 2015 in both crash and population datasets
bad_municipalities <- pop_data %>% 
  filter(`2015` < 100) %>% 
  pull(Municipality)

crash_data <- crash_data %>% 
  filter(!Municipality %in% bad_municipalities)

# join datasets
# then, divide all crash cols by 2015 population to get per-capita data
crash_data <- crash_data %>% 
  left_join(pop_data %>% 
              select('2015', 'GEOID10'),
            by = "GEOID10")

per_capita_data <- crash_data %>% 
  mutate_at(vars(`TOTAL CRASH`:`TOTAL VULNERABLE CRASHES`), ~(. / `2015`))

# create some summary versions of the datasets
summary_per_capita_data <- per_capita_data %>% 
  group_by(County, `Crash Year`) %>% 
  summarise(mean_crash = round(mean(`TOTAL CRASH`, na.rm = T), 5),
            mean_injured_killed = round(mean(`TOTAL INJURED KILLED`, na.rm = T), 5),
            mean_vunerable = round(mean(`TOTAL VULNERABLE CRASHES`, na.rm = T), 6), # lower numbers so more digits
            .groups = "drop")

# get raw counts over the years
summed_data <- crash_data %>% 
  group_by(County, Municipality, GEOID10, Municipality_County) %>% 
  summarise(total_crashes = sum(`TOTAL CRASH`),
            total_people_injured_killed = sum(`TOTAL KILLED`) + sum(`TOTAL INJURED`),
            total_vulnerable_crashes = sum(`PEDESTRIAN COUNT`) + sum(`BICYCLE COUNT`))

# what is the most recent year of data? What is min?
recent_year <- max(crash_data$`Crash Year`)
min_year <- min(crash_data$`Crash Year`)

###############################
## saving off gt tables ####
# because of the current state of affairs with quarto, gt() tables don't render
# correctly with word output. see https://github.com/rstudio/gt/issues/1098
# and https://github.com/rstudio/gt/labels/Focus%3A%20Word%20Output
# the workaround is to generate them, save them, and then insert them into the document

crash_table <- make_nice_gt(per_capita_data, recent_year, `TOTAL CRASH`,
             title = "Total Crashes Per Capita") %>%
  cols_label(`TOTAL CRASH` = "Crashes")

people_table <- make_nice_gt(per_capita_data, recent_year, `TOTAL INJURED KILLED`,
             title = "Total People Killed/Injured Per Capita") %>% 
  cols_label(`TOTAL INJURED KILLED` = "Total People")

vulnerable_table <- make_nice_gt(per_capita_data, recent_year, `TOTAL VULNERABLE CRASHES`,
             title = "Total Vulnerable Road User Crashes Per Capita") %>% 
  cols_label(`TOTAL VULNERABLE CRASHES` = "Crashes")
