#### Summary Stats
library(ggplot2)
library(corrplot)

### Get a sense of what the diabetes rate is
mean(diabetes$Diabetes_binary)

### Group by Education
diabetes %>% group_by(Age) %>% summarize(mean(Diabetes_binary), sd(Diabetes_binary))

### Group by Sex
diabetes %>% group_by(Sex) %>% summarize(mean(Diabetes_binary), sd(Diabetes_binary))
table(diabetes$Sex, diabetes$Diabetes_binary)
### Contingency Tables
table(diabetes$Fruits, diabetes$Veggies)
diabetes %>% group_by(Income) %>% summarize(mean(Diabetes_binary), sd(Diabetes_binary))
### Histogram BMI/Diabetes

g <- ggplot(diabetes, aes(x=BMI))
g + geom_histogram(aes(fill = as.factor(Diabetes_binary), y = ..density..), position = "dodge") +
  labs(title = "Density Histogram - BMI", fill = "Diabetes")


### Bar Plot Fruits/Veggies
fruit_lab <- c("0"="no fruit", "1"="eats fruit")
g <- ggplot(diabetes, aes(x = GenHlth, y = BMI))
g + geom_point(aes(color = as.factor(Diabetes_binary)), position = "jitter") +
  labs(y = "BMI", title = "Self-Reported Health vs. BMI", x = "General Health") + guides(color = guide_legend(title = "diabetes"))

### corrplot all numerical variables
correlation <- cor(select(diabetes, Diabetes_binary, HighBP, HighChol, CholCheck, Stroke, HeartDiseaseorAttack, PhysActivity, Fruits, Veggies, HvyAlcoholConsump, AnyHealthcare, NoDocbcCost, DiffWalk, m_hlth, p_hlth), method = "spearman")
corrplot(correlation, type = 'upper', tl.pos = 'lt')

###
