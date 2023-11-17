education_levels <- c("Elementary", "HighSchool", "HighSchoolGrad", "College")

output <- paste0(education_levels, ".md")

params = lapply(education_levels, FUN = function(x){list(education = x)})

reports <- tibble(output, params)

library(rmarkdown)
apply(reports, MARGIN = 1, FUN = function(x){
  render(input = "558_Project_3.Rmd", output_file = x[[1]], params = x[[2]])
})

render(input = "558_Project_3.Rmd", output_file = "CollegeGrad.md", params = list(education = "CollegeGrad"))
