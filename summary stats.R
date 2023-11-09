#### Summary Stats
library(ggplot2)
library(corrplot)

### Get a sense of what the diabetes rate is
mean(elementary$Diabetes_binary)

### Group by Education
elementary %>% group_by(Age) %>% summarize(mean(Diabetes_binary), sd(Diabetes_binary))

### Group by Sex
elementary %>% group_by(Sex) %>% summarize(mean(Diabetes_binary), sd(Diabetes_binary))

### Contingency Tables
table(elementary$Diabetes_binary, elementary$Income)

### Histogram BMI/Diabetes

g <- ggplot(elementary, aes(x=BMI))
g + geom_histogram(aes(fill = as.factor(Diabetes_binary), y = ..density..), position = "dodge") +
  labs(title = "Density Histogram - BMI", fill = "Diabetes")


### Bar Plot Income/Diabetes
g <- ggplot(elementary, aes(x = Income))
g + geom_bar(aes(fill = as.factor(Diabetes_binary)), position = "dodge") +
  labs(x = "Education", title = "Bar Plot Income/Diabetes", y = "Count") +
  scale_fill_discrete(name = "Diabetes", labels = c("No", "Yes"))

### corrplot all numerical variables
correlation <- cor(select(elementary, Diabetes_binary, HighBP, HighChol, CholCheck, Stroke, HeartDiseaseorAttack, PhysActivity, Fruits, Veggies, HvyAlcoholConsump, AnyHealthcare, NoDocbcCost, DiffWalk, m_hlth, p_hlth), method = "spearman")
corrplot(correlation, type = 'upper', tl.pos = 'lt')

###
