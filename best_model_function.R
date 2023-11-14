library(caret)
library(tidyverse)

model_names <- c("logreg_fit1", "rf_fit", "lda_fit")
model <- c(logreg_fit1, rf_fit, lda_fit)

for(i in 1:length(model_names)){
  print(noquote(model_names[i]))
}

#################################
######## Final Model Selection
################################

#model_list <- list("logreg_fit1" = logreg_fit1, "rf_fit" = rf_fit, "lda_fit" = lda_fit)
model_list <- list(logreg_fit1, rf_fit, lda_fit)

best_model <- function(x){
  results <- list()
  for(i in 1:length(x)){
    pred <- predict(x[i], newdata = diabetesTest)
    result <- confusionMatrix(pred[[1]], diabetesTest$Diabetes_binary)
    results[i] <- result$overall[1]
  }
  names(results) <- c("logreg", "rf", "lda")
  return(results)
}

accuracy_results <- data.frame(best_model(model_list))
accuracy_results
which.max(accuracy_results)


###################################
####### End Final Model Selection
###################################

#fit <- paste0(x[i])
#mode <- x[i]
#model_name <- noquote(names(x[i]))

max(unlist(lapply(tab,FUN=max)))

results <- append(results, c( = confusionMatrix(predict(logreg_fit1, newdata = diabetesTest), diabetesTest$Diabetes_binary)$overall[1]))

logreg_pred <- predict(model_list[1], newdata = diabetesTest)
confusionMatrix(logreg_pred[[1]], diabetesTest$Diabetes_binary)

ab <- c("logita" = 0.887, "logitb" = 0.889)
names(ab) <- c("1st", "Second")

ab <- append(ab, c("logitc" = confusionMatrix(predict(logreg_fit1, newdata = diabetesTest), diabetesTest$Diabetes_binary)$overall[1]))

practice_list <- list("x" = "hello", "y" = "goodbye")
p_n <- names(practice_list[1])
