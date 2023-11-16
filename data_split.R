library(caret)


#### onehot encode
#head(diabetes)
#define one-hot encoding function
#dummy <- dummyVars(" ~ .", data=diabetes)
#perform one-hot encoding on data frame
#diabetes <- data.frame(predict(dummy, newdata=diabetes))
#view final data frame
#str(diabetes)

diabetes$Diabetes_fac <- if_else(diabetes$Diabetes_binary == 0, "negative", "positive")
diabetes$Diabetes_fac <- as.factor(diabetes$Diabetes_fac)

### Data Split
set.seed(21)
dfIndex <- createDataPartition(diabetes$Diabetes_fac, p = 0.7, list = FALSE)
diabetesTrain <- diabetes[dfIndex, ]
diabetesTest <- diabetes[-dfIndex, ]
test_logloss <- diabetesTest$Diabetes_binary
diabetesTrain <- diabetesTrain %>% dplyr::select(-Diabetes_binary)
diabetesTest <- diabetesTest %>% dplyr::select(-Diabetes_binary)

write_csv(diabetesTrain, "diabetesTrain.csv")
write_csv(diabetesTest, "diabetesTest.csv")
