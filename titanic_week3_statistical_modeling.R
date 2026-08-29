# WEEK 3: STATISTICAL ANALYSIS AND PREDICTIVE MODELING USING R
# Dataset: Titanic train.csv

library(tidyverse)
library(caret)
library(pROC)

set.seed(123)

# Load data
titanic <- read.csv("train.csv", stringsAsFactors=FALSE)

# Clean data
titanic_clean <- titanic %>%
  select(-Cabin) %>%
  mutate(
    Age=ifelse(is.na(Age), median(Age,na.rm=TRUE), Age),
    Embarked=ifelse(is.na(Embarked),
                    names(sort(table(Embarked),decreasing=TRUE))[1],
                    Embarked),
    Sex=factor(Sex),
    Embarked=factor(Embarked),
    Pclass=factor(Pclass,ordered=TRUE)
  ) %>%
  select(-PassengerId,-Name,-Ticket)

# Hypothesis tests
sex_table <- table(titanic_clean$Sex,titanic_clean$Survived)
print(chisq.test(sex_table))
print(prop.table(sex_table,margin=1))

print(shapiro.test(titanic_clean$Age))
print(t.test(Age~Survived,data=titanic_clean))
print(wilcox.test(Age~Survived,data=titanic_clean))

# Correlation
numeric_data <- titanic_clean %>%
  select(Survived,Age,SibSp,Parch,Fare)
print(round(cor(numeric_data,use="pairwise.complete.obs"),3))

# Train/test split
idx <- createDataPartition(titanic_clean$Survived,p=.80,list=FALSE)
train_data <- titanic_clean[idx,]
test_data <- titanic_clean[-idx,]

# Cross-validation
train_data$Survived <- factor(ifelse(train_data$Survived==1,"Yes","No"),
                              levels=c("No","Yes"))

ctrl <- trainControl(
  method="cv", number=5, classProbs=TRUE,
  savePredictions="final", summaryFunction=twoClassSummary
)

cv_model <- train(
  Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,
  data=train_data, method="glm", family=binomial,
  metric="ROC", trControl=ctrl
)
print(cv_model)

# Final model
final_model <- glm(
  Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,
  data=train_data, family=binomial
)
print(summary(final_model))
print(exp(coef(final_model)))

# Test performance
test_prob <- predict(final_model,newdata=test_data,type="response")
test_pred <- ifelse(test_prob>=.50,"Yes","No")

actual <- factor(ifelse(test_data$Survived==1,"Yes","No"),
                 levels=c("No","Yes"))
predicted <- factor(test_pred,levels=c("No","Yes"))

print(confusionMatrix(predicted,actual,positive="Yes"))

roc_obj <- roc(actual,test_prob,levels=c("No","Yes"),direction="<")
print(auc(roc_obj))
plot(roc_obj,main="ROC Curve – Logistic Regression")

# Diagnostics
par(mfrow=c(2,2))
plot(final_model)

# Save outputs
write.csv(titanic_clean,"titanic_cleaned_week3.csv",row.names=FALSE)
