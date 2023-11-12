### Random Forest

rf_fit <- train(Diabetes_binary ~., data = diabetesTrain, method = "rf",
                metric = "logLoss",
                preProcess = c("center", "scale"),
                trControl = trainControl(method = "cv", number = 5, 
                                         classProbs=TRUE, summaryFunction=mnLogLoss),
                tuneGrid = data.frame(mtry = 1:6))

rf_fit$results
rf_fit$bestTune

rf_pred <- predict(rf_fit, newdata = diabetesTest)
confusionMatrix(rf_pred, diabetesTest$Diabetes_binary)
