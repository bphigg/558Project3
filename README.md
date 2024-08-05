# Project 3 - Supervised Learning and Categorical Prediction
## Diabetes Prediction Models

This is the landing page for **558 Project 3** Below are the links to reports for each education level in the diabetes dataset as well as a list of the packages used in the project and the render() code used to automate the reports.

**R packages used for this project:**  
* library(ggplot2)  
* library(corrplot)  
* library(MASS)  
* library(tidyverse)  
* library(dplyr)  
* library(caret)  
* library(rmarkdown)  
* library(MLmetrics)  
* library(vcd)  
* library(doParallel)  
* library(parallel)

[Elementary](https://bphigg.github.io/558Project3/Elementary)

[High School](https://bphigg.github.io/558Project3/HighSchool)

[High School Graduate](https://bphigg.github.io/558Project3/HighSchoolGrad)

[College](https://bphigg.github.io/558Project3/College)

[College Grad](https://bphigg.github.io/558Project3/CollegeGrad)

**Render Code**
`education_levels <- c("Elementary", "HighSchool", "HighSchoolGrad", "College", "CollegeGrad")`  
`output <- paste0(education_levels, ".md")`  
`params = lapply(education_levels, FUN = function(x){list(education = x)})`  
`reports <- tibble(output, params)`  
`apply(reports, MARGIN = 1, FUN = function(x){`  
  `render(input = "558_Project_3.Rmd", output_file = x[[1]], params = x[[2]])`  
`})`  