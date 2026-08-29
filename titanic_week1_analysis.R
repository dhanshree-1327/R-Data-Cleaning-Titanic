# ============================================================
# WEEK 1: DATA CLEANING AND PRELIMINARY ANALYSIS WITH R
# Dataset: Titanic train.csv
# ============================================================

# 1. Packages
# install.packages(c("tidyverse", "janitor", "scales"))
library(tidyverse)
library(janitor)
library(scales)

# 2. Import
titanic <- read.csv("train.csv", stringsAsFactors = FALSE)
titanic <- clean_names(titanic)

# 3. Initial inspection
dim(titanic)
str(titanic)
summary(titanic)
head(titanic)

# 4. Missing-value report
missing_summary <- data.frame(
  Variable = names(titanic),
  Missing = sapply(titanic, function(x) sum(is.na(x))),
  Percentage = round(sapply(titanic, function(x) mean(is.na(x)) * 100), 2)
)
print(missing_summary[missing_summary$Missing > 0, ])

# 5. Cleaning
titanic_clean <- titanic %>%
  select(-cabin)

# Median imputation
titanic_clean$age[is.na(titanic_clean$age)] <-
  median(titanic_clean$age, na.rm = TRUE)

# Mode imputation
mode_embarked <- names(sort(table(titanic_clean$embarked),
                            decreasing = TRUE))[1]
titanic_clean$embarked[is.na(titanic_clean$embarked)] <- mode_embarked

# Remove identifiers / high-cardinality text
titanic_clean <- titanic_clean %>%
  select(-passenger_id, -name, -ticket)

# Convert categorical variables to factors
titanic_clean$sex <- factor(titanic_clean$sex)
titanic_clean$embarked <- factor(titanic_clean$embarked)
titanic_clean$pclass <- factor(
  titanic_clean$pclass,
  levels = c(1, 2, 3),
  ordered = TRUE
)

# 6. Verify missing values
print(colSums(is.na(titanic_clean)))

# 7. IQR outlier detection
iqr_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  which(x < lower | x > upper)
}

age_outliers <- iqr_outliers(titanic_clean$age)
fare_outliers <- iqr_outliers(titanic_clean$fare)

cat("Age IQR outliers:", length(age_outliers), "\n")
cat("Fare IQR outliers:", length(fare_outliers), "\n")

boxplot(titanic_clean$age, main="Age Boxplot", ylab="Age")
boxplot(titanic_clean$fare, main="Fare Boxplot", ylab="Fare")

# 8. Encoding
encoded <- model.matrix(
  ~ sex + embarked + pclass - 1,
  data = titanic_clean
)
encoded <- as.data.frame(encoded)

numeric_data <- titanic_clean %>%
  mutate(pclass = as.numeric(pclass)) %>%
  select(survived, pclass, age, sib_sp, parch, fare)

final_data <- bind_cols(numeric_data, encoded)

# 9. Standardization
final_data$age_scaled <- as.numeric(scale(final_data$age))
final_data$fare_scaled <- as.numeric(scale(final_data$fare))

# 10. Descriptive statistics
print(summary(titanic_clean))
print(summary(final_data))

# 11. Survival by sex
sex_summary <- titanic_clean %>%
  group_by(sex) %>%
  summarise(
    passengers = n(),
    survivors = sum(survived),
    survival_rate = round(mean(survived) * 100, 2)
  )
print(sex_summary)

ggplot(titanic_clean, aes(x = sex, fill = factor(survived))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(title="Survival Proportion by Sex",
       x="Sex", y="Proportion", fill="Survived")

# 12. Survival by class
class_summary <- titanic_clean %>%
  group_by(pclass) %>%
  summarise(
    passengers = n(),
    survivors = sum(survived),
    survival_rate = round(mean(survived) * 100, 2)
  )
print(class_summary)

ggplot(titanic_clean, aes(x = pclass, fill = factor(survived))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(title="Survival Proportion by Passenger Class",
       x="Passenger Class", y="Proportion", fill="Survived")

# 13. Correlation
cor_data <- titanic %>%
  select(survived, pclass, age, sib_sp, parch, fare)

cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")
print(round(cor_matrix, 2))

# 14. Save outputs
write.csv(final_data, "titanic_cleaned_week1.csv", row.names = FALSE)
write.csv(missing_summary, "titanic_missing_value_report.csv", row.names = FALSE)

# ============================================================
# END OF SCRIPT
# ============================================================
