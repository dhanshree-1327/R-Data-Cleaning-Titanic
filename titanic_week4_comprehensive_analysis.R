# WEEK 4 – COMPREHENSIVE TITANIC ANALYSIS

library(tidyverse)
library(scales)

# Import
titanic <- read.csv("train.csv", stringsAsFactors=FALSE)

# Inspect
dim(titanic)
str(titanic)
summary(titanic)

# Missing values
missing_summary <- data.frame(
  Variable=names(titanic),
  Missing=sapply(titanic, function(x) sum(is.na(x))),
  Percentage=round(sapply(titanic, function(x) mean(is.na(x))*100),2)
)
print(missing_summary)

# Cleaning
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

# Summary
summary(titanic_clean)

# Visualization 1
ggplot(titanic_clean,aes(x=Sex,fill=factor(Survived)))+
  geom_bar(position="fill")+
  scale_y_continuous(labels=percent)+
  labs(title="Survival Proportion by Sex",
       x="Sex",y="Proportion",fill="Survived")+
  theme_minimal()

# Visualization 2
ggplot(titanic_clean,aes(x=Pclass,fill=factor(Survived)))+
  geom_bar(position="fill")+
  scale_y_continuous(labels=percent)+
  labs(title="Survival Proportion by Passenger Class",
       x="Passenger Class",y="Proportion",fill="Survived")+
  theme_minimal()

# Visualization 3
ggplot(titanic_clean,aes(x=Age))+
  geom_histogram(binwidth=5,na.rm=TRUE)+
  labs(title="Passenger Age Distribution",
       x="Age",y="Frequency")+
  theme_minimal()

# Visualization 4
ggplot(titanic_clean,aes(x=Fare))+
  geom_histogram(bins=30,na.rm=TRUE)+
  labs(title="Passenger Fare Distribution",
       x="Fare",y="Frequency")+
  theme_minimal()

# Logistic regression
model <- glm(
  Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
  data=titanic_clean,
  family=binomial
)

summary(model)

# Predictions and confusion matrix
prob <- predict(model,type="response")
prediction <- ifelse(prob>=0.5,1,0)
cm <- table(Predicted=prediction,Actual=titanic_clean$Survived)
print(cm)

# Basic accuracy
accuracy <- mean(prediction==titanic_clean$Survived)
print(accuracy)

# Odds ratios
print(exp(coef(model)))

# Save cleaned data
write.csv(titanic_clean,"titanic_cleaned_week4.csv",row.names=FALSE)
