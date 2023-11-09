library(caret)


#### onehot encode
#head(diabetes)
#define one-hot encoding function
#dummy <- dummyVars(" ~ .", data=diabetes)
#perform one-hot encoding on data frame
#diabetes <- data.frame(predict(dummy, newdata=diabetes))
#view final data frame
#str(diabetes)

diabetes$Diabetes_binary <- if_else(diabetes$Diabetes_binary == 0, "negative", "positive")
diabetes$Diabetes_binary <- as.factor(diabetes$Diabetes_binary)

### Data Split
set.seed(21)
dfIndex <- createDataPartition(diabetes$Diabetes_binary, p = 0.7, list = FALSE)
diabetesTrain <- diabetes[dfIndex, ]
diabetesTest <- diabetes[-dfIndex, ]

