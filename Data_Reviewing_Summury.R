# Load necessary library
library(haven)

# Load the dataset
support <- read_dta("support.dta")

# View the first few rows and Summary statistics
head(support)  # Shows the first few rows
summary(support)  # Summary statistics for each variable

# Check variable names
names(support)

# Load caTools library for data splitting
library(caTools)

# Set seed for reproducibility
set.seed(42)

# Split the data: 80% training, 20% testing
split <- sample.split(support$totcst, SplitRatio = 0.8)
train_data <- subset(support, split == TRUE)
test_data <- subset(support, split == FALSE)

# Check Class Balance
summary(train_data$totcst)
summary(test_data$totcst)

# Data Splitting Validation
nrow(train_data)  # Number of rows in training data
nrow(test_data)   # Number of rows in test data

# Inspect Random Split Result
mean(train_data$num_co)
mean(test_data$num_co)


# Load necessary library for data manipulation
library(dplyr)

# Function to summarize each variable based on its type
summarize_variable <- function(data, name) {
  # Check if the variable is numeric
  if (is.numeric(data[[name]])) {
    var_summary <- summary(data[[name]])
    var_mean <- mean(data[[name]], na.rm = TRUE)
    var_na <- sum(is.na(data[[name]]))
    
    # Create a data frame with consistent structure for numeric variables
    data.frame(
      Variable = name,
      Min = ifelse("Min." %in% names(var_summary), var_summary["Min."], NA),
      Q1 = ifelse("1st Qu." %in% names(var_summary), var_summary["1st Qu."], NA),
      Median = ifelse("Median" %in% names(var_summary), var_summary["Median"], NA),
      Mean = var_mean,
      Q3 = ifelse("3rd Qu." %in% names(var_summary), var_summary["3rd Qu."], NA),
      Max = ifelse("Max." %in% names(var_summary), var_summary["Max."], NA),
      NA_Count = var_na,
      stringsAsFactors = FALSE
    )
  } else {
    # For non-numeric variables, provide the most frequent value and NA count
    var_na <- sum(is.na(data[[name]]))
    mode_summary <- summary(data[[name]])
    
    # Create a data frame with consistent structure for non-numeric variables
    data.frame(
      Variable = name,
      Min = NA,
      Q1 = NA,
      Median = NA,
      Mean = NA,
      Q3 = NA,
      Max = NA,
      Most_Frequent = ifelse(length(mode_summary) > 0, names(sort(-table(data[[name]])))[1], NA),
      Frequency = ifelse(length(mode_summary) > 0, sort(-table(data[[name]]))[1], NA),
      NA_Count = var_na,
      stringsAsFactors = FALSE
    )
  }
}

# Summarize each variable in entire data
summary <- do.call(rbind, lapply(names(support), function(var) summarize_variable(support, var)))

# Summarize each variable in training data
train_summary <- do.call(rbind, lapply(names(train_data), function(var) summarize_variable(train_data, var)))

# Summarize each variable in test data
test_summary <- do.call(rbind, lapply(names(test_data), function(var) summarize_variable(test_data, var)))

# View the summary tables
print(summary)
print(train_summary)
print(test_summary)

# Load ggplot2 for visual exploration
library(ggplot2)

# Visual Exploration

# Histogram for num_co in training data
ggplot(train_data, aes(x = num_co)) + 
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(title = "Histogram of Number of Comorbidities", x = "Number of Comorbidities", y = "Frequency")

# Histogram for totcst in training data
ggplot(train_data, aes(x = totcst)) + 
  geom_histogram(binwidth = 1000, fill = "green", alpha = 0.7) +
  labs(title = "Histogram of Total Hospital Cost", x = "Total Hospital Cost", y = "Frequency")

# Boxplot for totcst by num_co in training data
ggplot(train_data, aes(x = as.factor(num_co), y = totcst)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(title = "Boxplot of Total Hospital Cost by Number of Comorbidities", x = "Number of Comorbidities", y = "Total Hospital Cost")

# Scatterplot of num_co vs totcst in training data
ggplot(train_data, aes(x = num_co, y = totcst)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatterplot of Number of Comorbidities vs Total Hospital Cost", x = "Number of Comorbidities", y = "Total Hospital Cost")


