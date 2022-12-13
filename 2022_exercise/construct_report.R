# R script that, when appropriate, will render a new quarto crash report
# this could be called in a command line / task scheduler,
# e.g. "Rscript construct_report.R"

# load packages, etc.
source(here::here("2022_exercise", "report_functions_data.R"))

# basically, this file will source generate_data and, depending on the output, will either
# render sample_report.qmd or not

## for testing - manually make number of rows lower to trigger a fresh report
# cache_data_details <- read_parquet(here::here("2022_exercise", "cache_data_details.parquet"))
# cache_data_details$num_rows[1] <- 4700
# file_name <- here::here("2022_exercise", "cache_data_details.parquet")
# write_parquet(cache_data_details, file_name)

# read in cache data details - cache was saved during last report build
cache_data_details <- read_parquet(here::here("2022_exercise", "cache_data_details.parquet"))

# run comparison: go or no-go?
run_report <- check_cache_vs_current_data(cache_data_details, new_data_details)

if(run_report){
  
  # save off new plots to be included in plot
  gtsave(crash_table, filename = here::here("2022_exercise", "crash_table.png"))
  gtsave(people_table, filename = here::here("2022_exercise", "people_table.png"))
  gtsave(vulnerable_table, filename = here::here("2022_exercise", "vulnerable_table.png"))
  
  output_filename <- glue("{today()}_sample_report.docx") # this will run 'successfully' but puts the report in one level directory up from the .qmd file
  
  # output_filename <- here::here("2022_exercise", glue("{today()}_sample_report.docx")) # this doesn't quite work. it writes the file successfully in the correct directory, but errors out afterwards...
  
  message("Generating new report...")
  quarto_render(here::here("2022_exercise", "sample_report.qmd"), output_format = "docx",
                output_file = output_filename)

} else {
  message("Current data is identical to existing cache data. No new report will be generated.")
}
