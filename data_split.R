library(caret)


#### onehot encode
head(elementary)
#define one-hot encoding function
dummy <- dummyVars(" ~ .", data=elementary)
#perform one-hot encoding on data frame
elementary <- data.frame(predict(dummy, newdata=elementary))
#view final data frame
str(elementary)

elementary$Diabetes_binary <- if_else(elementary$Diabetes_binary == 0, "negative", "positive")
elementary$Diabetes_binary <- as.factor(elementary$Diabetes_binary)

### Data Split
set.seed(21)
diabetes_eIndex <- createDataPartition(diabetes_e$Diabetes_binary, p = 0.7, list = FALSE)
diabetes_eTrain <- diabetes_e[diabetes_eIndex, ]
diabetes_eTest <- diabetes_e[-diabetes_eIndex, ]
