### Random Forest

rf_fit <- train(Diabetes_binary ~., data = diabetes, method = "rf",
                metric = "logLoss",
                preProcess = c("center", "scale"),
                trControl = trainControl(method = "cv", number = 5, 
                                         classProbs=TRUE, summaryFunction=mnLogLoss),
                tuneGrid = data.frame(mtry = 1:20))

rf_fit$results
rf_fit$bestTune
