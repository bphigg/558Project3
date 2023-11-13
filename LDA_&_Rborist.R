library(MASS)

### Linear Discriminate Analysis

lda_fit <- train(Diabetes_binary ~ ., data = diabetesTrain, method = "lda",
                 metric = "logLoss",
                 PreProcess = c("center", "scale"),
                 trControl = trainControl(method = "cv", number = 5, 
                                          classProbs=TRUE, summaryFunction=mnLogLoss))

lda_fit$results
lda_fit$bestTune

lda_pred <- predict(lda_fit, newdata = diabetesTest)
ax <- confusionMatrix(lda_pred, diabetesTest$Diabetes_binary)


### Rborist

install.packages("Rborist")
library(Rborist)

tune1 <- 5:15
tune2 <- c(1, 3, 5, 7)
tune_grid <- expand.grid(predFixed = tune1, minNode = tune2)
head(tune_grid)

rborist_fit <- train(Diabetes_binary ~., data = diabetesTrain, method = "Rborist",
                    metric = "logLoss",
                    PreProcess = c("center", "scale"),
                    trControl = trainControl(method = "cv", number = 5, 
                                             classProbs=TRUE, summaryFunction=mnLogLoss),
                    tuneGrid = tune_grid
                    )

rborist_fit$results
rborist_fit$bestTune

rborist_pred <- predict(rborist_fit, newdata = diabetesTest)
confusionMatrix(rborist_pred, diabetesTest$Diabetes_binary)
