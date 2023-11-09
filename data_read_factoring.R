library(tidyverse)
data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")
diabetes <- data %>% mutate(Education = if_else(Education <= 2, 1, if_else(Education == 3, 2, if_else(Education == 4, 3, if_else(Education == 5, 4, 5))))) %>%
  mutate(Education = factor(Education)) %>%
  mutate(Education = recode(Education, "1" = "Elementary", "2" = "HighSchool", "3" = "HighSchoolGrad", "4" = "College", "5" = "CollegeGrad")) %>% 
# This is where automation will select the correct subset
  filter(Education == "Elementary") %>% 
  select(-Education)

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
  mutate(Age = recode(Age, "1" = "18_24", "2" = "25_29", "3" = "30_34", "4" = "35_39", "5" = "40_44", "6" = "45_49", "7" = "50_54", "8" = "55_59", "9" = "60_64", "10" = "65_69", "11" = "70_74", "12" = "75_79", "13" = "> 80")) %>%
  mutate(Income = factor(Income)) %>%
  mutate(Income = recode(Income, "1" = "10k", "2" = "15k", "3" = "20k", "4" = "25k", "5" = "35k", "6" = "50k", "7" = "75k", "8" = "greater_75k")) %>%
  mutate(Smoker = factor(Smoker)) %>%
  mutate(Smoker = recode(Smoker, "0" = "no", "1" = "yes")) %>%
### Drop original Ment&PhysHlth
  select(-MentHlth, -PhysHlth)

