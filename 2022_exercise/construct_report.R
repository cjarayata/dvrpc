# data check should happen in "generate_data.R"
# basically, this file will source generate_data and, depending on the output, will either
# render sample_report.Rmd or not

# read in existing cache file from last run
dataset_details <- read_parquet(here::here("2022_exercise", "cache_data_details.parquet"))


library(quarto)


if(good_to_go){
  quarto_render(here::here("2022_exercise", "sample_report.Rmd"), output_format = "docx")
}
# save as temp file

# email to OSS