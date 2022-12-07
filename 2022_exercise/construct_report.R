# data check should happen in "generate_data.R"
# basically, this file will source generate_data and, depending on the output, will either
# render sample_report.Rmd or not


library(quarto)

source()

if(good_to_go){
  quarto_render("sample_report.Rmd", output_format = "word")
}
# save as temp file

# email to OSS