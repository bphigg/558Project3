education_levels <- c("Elementary", "HighSchool", "HighSchoolGrad", "College", "CollegeGrad")

output <- paste0(education_levels, ".md")

params = lapply(education_levels, FUN = function(x){list(education = x)})

reports <- tibble(output, params)

library(rmarkdown)
apply(reports, MARGIN = 1, FUN = function(x){
  render(input = "558_Project_3.Rmd", output_file = x[[1]], params = x[[2]])
})

render(input = "558_Project_3.Rmd", output_file = "Elementary.md", params = list(education = "Elementary"))
