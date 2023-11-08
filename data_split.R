library(caret)


#### Diabetes factor for modeling
diabetes_e$Diabetes_binary <- if_else(diabetes_e$Diabetes_binary == 0, "negative", "positive")
diabetes_e$Diabetes_binary <- as.factor(diabetes_e$Diabetes_binary)

### Data Split
set.seed(21)
diabetes_eIndex <- createDataPartition(diabetes_e$Diabetes_binary, p = 0.7, list = FALSE)
diabetes_eTrain <- diabetes_e[diabetes_eIndex, ]
diabetes_eTest <- diabetes_e[-diabetes_eIndex, ]
