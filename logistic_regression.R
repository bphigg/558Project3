### Logistic Regression

logreg_fit1 <- train(Diabetes_binary ~ ., data=diabetesTrain, method="glm", family="binomial",
                     metric="logLoss",
                  preProcess=c("center", "scale"),
                  trControl = trainControl(method="cv", number = 5, 
                                           classProbs=TRUE, summaryFunction=mnLogLoss))

logreg_fit2 <- train(Diabetes_binary ~ BMI + poly(Smoker, 2, raw=TRUE) + Fruits:Veggies + HighBP + HighChol, data=diabetesTrain, method="glm", family="binomial",
                     metric="logLoss",
                     preProcess=c("center", "scale"),
                     trControl = trainControl(method="cv", number = 5, 
                                              classProbs=TRUE, summaryFunction=mnLogLoss))

logreg_fit3 <- train(Diabetes_binary ~ BMI + Age + Income + BMI:Age + BMI:Income + 
                       HighBP:HighChol + Smoker, data=diabetesTrain,
                     method="glm", family="binomial",
                     metric="logLoss",
                     preProcess=c("center", "scale"),
                     trControl = trainControl(method="cv", number = 5, 
                                              classProbs=TRUE, summaryFunction=mnLogLoss))

logreg_pred <- predict(logreg_fit1, newdata = diabetesTest)
confusionMatrix(logreg_pred, diabetesTest$Diabetes_binary)

logreg_fit1$results
logreg_fit2$results
logreg_fit3$results
