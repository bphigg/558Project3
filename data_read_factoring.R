library(tidyverse)
diabetes <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")
str(diabetes)

### MentHlth & PhysHlth converted to binary
diabetes <- diabetes %>% 
  mutate(m_hlth = if_else(MentHlth == 0, 0, 1)) %>%
  mutate(p_hlth = if_else(PhysHlth == 0, 0, 1)) %>%
### create factors
  mutate(GenHlth = factor(GenHlth)) %>%
  mutate(GenHlth = recode(GenHlth, "1" = "excellent", "2" = "very_good", "3" = "good", "4" = "fair", "5" = "poor")) %>%
  mutate(Sex = factor(Sex)) %>%
  mutate(Sex = recode(Sex, "0" = "female", "1" = "male")) %>%
  mutate(Age = factor(Age)) %>%
  mutate(Age = recode(Age, "1" = "18-24", "2" = "25-29", "3" = "30-34", "4" = "35-39", "5" = "40-44", "6" = "45-49", "7" = "50-54", "8" = "55-59", "9" = "60-64", "10" = "65-69", "11" = "70-74", "12" = "75-79", "13" = "> 80")) %>%
  mutate(Education = if_else(Education <= 2, 1, if_else(Education == 3, 2, if_else(Education == 4, 3, if_else(Education == 5, 4, 5))))) %>%
  mutate(Education = factor(Education)) %>%
  mutate(Education = recode(Education, "1" = "elementary", "2" = "some_HS", "3" = "HS_grad", "4" = "some_college", "5" = "college_grad")) %>%
  mutate(Income = factor(Income)) %>%
  mutate(Income = recode(Income, "1" = "<10k", "2" = "<15k", "3" = "<20k", "4" = "<25k", "5" = "<35k", "6" = "<50k", "7" = "<75k", "8" = ">75k")) %>%
  mutate(Smoker = factor(Smoker)) %>%
  mutate(Smoker = recode(Smoker, "0" = "no", "1" = "yes")) %>%
  select(MentHlth, PhysHlth, everything())

### drop BMI, MentHlth, PhysHlth (original columns)
diabetes <- diabetes[ ,-1:-2]

####### CodeBook for factor levels "https://www.cdc.gov/brfss/annual_data/2015/pdf/codebook15_llcp.pdf"


