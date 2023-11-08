library(caret)


#### Data Split
set.seed(21)
diabetes_eIndex <- createDataPartition(diabetes_e$Diabetes_binary, p = 0.7, list = FALSE)
diabetes_eTrain <- diabetes_e[diabetes_eIndex, ]
diabetes_eTest <- diabetes_e[-diabetes_eIndex, ]
