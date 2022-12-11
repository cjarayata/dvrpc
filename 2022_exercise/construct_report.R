# data check should happen in "generate_data.R"
# basically, this file will source generate_data and, depending on the output, will either
# render sample_report.Rmd or not

# read in existing cache file from last run
dataset_details <- read_parquet(here::here("2022_exercise", "cache_data_details.parquet"))


library(quarto)
library(lubridate)
library(glue)


if(good_to_go){
  output_filename <- glue("{today()}_sample_report.docx")
  quarto_render(here::here("2022_exercise", "sample_report.qmd"), output_format = "docx",
                output_file = here::here("2022_exercise", output_filename))
}
# save as temp file

# email to OSS