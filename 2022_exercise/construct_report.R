# R script that, when appropriate, will render a new quarto crash report
# this could be called in a command line / task scheduler,
# e.g. "Rscript construct_report.R"

# load packages, etc.
source(here::here("2022_exercise", "report_functions_data.R"))

# basically, this file will source generate_data and, depending on the output, will either
# render sample_report.Rmd or not

# read in cache data details
cache_data_details <- read_parquet(here::here("2022_exercise", "cache_data_details.parquet"))

# run comparison: go or no-go?
run_report <- check_cache_vs_current_data(cache_data_details, new_data_details)

if(run_report){
  output_filename <- glue("{today()}_sample_report.docx")
  # output_filename <- here::here("2022_exercise", glue("{today()}_sample_report.docx")) # this doesn't quite work. it writes the file successfully, but errors out afterwards and does not return the message...
  quarto_render(here::here("2022_exercise", "sample_report.qmd"), output_format = "docx",
                output_file = output_filename)
  message("Generating new report...")
} else {
  message("Current data is identical to existing cache data. No new report will be generated.")
}