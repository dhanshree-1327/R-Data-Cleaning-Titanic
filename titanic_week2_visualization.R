# WEEK 2: DATA VISUALIZATION AND INSIGHT COMMUNICATION USING R
# Dataset: Titanic train.csv

# install.packages(c("tidyverse","scales"))
library(tidyverse)
library(scales)

titanic <- read.csv("train.csv", stringsAsFactors = FALSE)

# Cleaning carried forward from Week 1
titanic_clean <- titanic %>%
  select(-Cabin) %>%
  mutate(
    Age = ifelse(is.na(Age), median(Age, na.rm=TRUE), Age),
    Embarked = ifelse(is.na(Embarked),
                      names(sort(table(Embarked), decreasing=TRUE))[1],
                      Embarked),
    Sex = factor(Sex),
    Embarked = factor(Embarked),
    Pclass = factor(Pclass, ordered=TRUE)
  )

# 1. Survival by sex
ggplot(titanic_clean, aes(x=Sex, fill=factor(Survived))) +
  geom_bar(position="fill") +
  scale_y_continuous(labels=percent) +
  labs(title="Survival Proportion by Sex",
       x="Sex", y="Proportion", fill="Survived") +
  theme_minimal()

# 2. Survival by passenger class
ggplot(titanic_clean, aes(x=Pclass, fill=factor(Survived))) +
  geom_bar(position="fill") +
  scale_y_continuous(labels=percent) +
  labs(title="Survival Proportion by Passenger Class",
       x="Passenger Class", y="Proportion", fill="Survived") +
  theme_minimal()

# 3. Age distribution
ggplot(titanic_clean, aes(x=Age)) +
  geom_histogram(binwidth=5, na.rm=TRUE) +
  labs(title="Distribution of Passenger Age",
       x="Age", y="Number of passengers") +
  theme_minimal()

# 4. Fare distribution
ggplot(titanic_clean, aes(x=Fare)) +
  geom_histogram(bins=30, na.rm=TRUE) +
  labs(title="Distribution of Passenger Fare",
       x="Fare", y="Frequency") +
  theme_minimal()

# Optional grouped summaries
titanic_clean %>%
  group_by(Sex) %>%
  summarise(Passengers=n(),
            Survivors=sum(Survived),
            Survival_Rate=round(mean(Survived)*100,2))

titanic_clean %>%
  group_by(Pclass) %>%
  summarise(Passengers=n(),
            Survivors=sum(Survived),
            Survival_Rate=round(mean(Survived)*100,2))
