### Logistic Regression

logreg_fit1 <- train(Diabetes_binary ~ ., data=diabetes_eTrain, method="glm", family="binomial",
                     metric="logLoss",
                  preProcess=c("center", "scale"),
                  trControl = trainControl(method="cv", number = 5, 
                                           classProbs=TRUE, summaryFunction=mnLogLoss))

logreg_fit2 <- train(Diabetes_binary ~ BMI, data=diabetes_eTrain, method="glm", family="binomial",
                     metric="logLoss",
                     preProcess=c("center", "scale"),
                     trControl = trainControl(method="cv", number = 5, 
                                              classProbs=TRUE, summaryFunction=mnLogLoss))

logreg_fit3 <- train(Diabetes_binary ~ BMI + Age + Income, data=diabetes_eTrain,
                     method="glm", family="binomial",
                     metric="logLoss",
                     preProcess=c("center", "scale"),
                     trControl = trainControl(method="cv", number = 5, 
                                              classProbs=TRUE, summaryFunction=mnLogLoss))
