#Geddy Lucier Groundhog Day ML Model
#For Groundhog Day 
# Messing Around


library(randomForest)
library(tidyverse)
library(stringr)
library(dplyr)
library(xgboost)
library(caret)

setwd("~/Dropbox")
setwd("/Users/geddylucier/Dropbox/Scuba.Tech")


### background data 
groundhog <- read_csv("groundhog.csv")
ave_temp <- read_csv("ave_temp.csv")
precip <- read_csv("precipitation.csv")

# create binary variable for spring or more winter and  NA
unique(ground_temp$Anomaly)
summary(groundhog)
colnames(groundhog)
?full_join
colnames(ave_temp)

fivenum(groundhog$Year)

ground_temp <- groundhog %>%
    mutate(bin_var = ifelse(str_detect(Spring_var, "1"), 1, 0)) %>%
    left_join(ave_temp %>% rename(Year = Date), by = "Year") %>%
    filter(Year > 1897) %>%
    left_join(precip %>% rename(Year = Date), by = "Year")  %>%
    mutate(Value.x = as.numeric(Value.x),
           Value.y = as.numeric(Value.y)
           ) %>%
    rename(temp = Value.x, rain = Value.y)
  
  

summary(lm(bin_var ~ temp + rain, data = ground_temp))

#### xgboost
write_csv(ground_temp, "groundhog_cleaned.csv")


# Load dataset
groundhog <- read.csv("groundhog_cleaned.csv")

# Remove unnecessary columns
groundhog <- groundhog %>%
  select(-Year, -Spring_var, -Comments)  # Drop categorical/text variables

# Ensure bin_var is numeric (target variable)
groundhog$bin_var <- as.numeric(groundhog$bin_var)

# Split into training (80%) and testing (20%) sets
set.seed(42)
train_index <- createDataPartition(groundhog$bin_var, p = 0.8, list = FALSE)
train_data <- groundhog[train_index, ]
test_data <- groundhog[-train_index, ]

# Convert data to XGBoost format
train_matrix <- xgb.DMatrix(data = as.matrix(train_data %>% select(-bin_var)), label = train_data$bin_var)
test_matrix <- xgb.DMatrix(data = as.matrix(test_data %>% select(-bin_var)), label = test_data$bin_var)


# Set model parameters
params <- list(
  objective = "binary:logistic",  # Binary classification
  eval_metric = "logloss",  # Log loss for performance evaluation
  max_depth = 4,  # Tree depth
  eta = 0.1,  # Learning rate
  subsample = 0.8,  # Row sampling
  colsample_bytree = 0.8  # Feature sampling per tree
)

# Train the model
xgb_model <- xgb.train(
  params = params,
  data = train_matrix,
  nrounds = 100,  # Number of boosting rounds
  watchlist = list(test = test_matrix),
  verbose = 1
)

# Predict on test data
test_preds <- predict(xgb_model, newdata = test_matrix)

# Convert probabilities to binary (0 or 1)
test_preds_binary <- ifelse(test_preds > 0.5, 1, 0)

# Evaluate model performance
conf_matrix <- confusionMatrix(factor(test_preds_binary), factor(test_data$bin_var))
print(conf_matrix)

importance_matrix <- xgb.importance(feature_names = colnames(train_matrix), model = xgb_model)
xgb.plot.importance(importance_matrix)

############### Random Forest
# Load dataset
groundhog <- read.csv("groundhog_cleaned.csv")

# Remove unnecessary columns
groundhog <- groundhog %>%
  select(-Year, -Spring_var, -Comments)  # Drop categorical/text variables

# Ensure bin_var is a factor (for classification)
groundhog$bin_var <- as.factor(groundhog$bin_var)

# Split into training (80%) and testing (20%) sets
set.seed(42)
train_index <- createDataPartition(groundhog$bin_var, p = 0.8, list = FALSE)
train_data <- groundhog[train_index, ]
test_data <- groundhog[-train_index, ]

# Train the model (500 trees, using all predictors)
rf_model <- randomForest(bin_var ~ ., data = train_data, ntree = 500, mtry = 2, importance = TRUE)

# Print model summary
print(rf_model)

# Predict on test data
test_preds <- predict(rf_model, newdata = test_data)

# Confusion Matrix
conf_matrix <- confusionMatrix(test_preds, test_data$bin_var)
print(conf_matrix)

# Prepare data for 2025 prediction (using dataset averages as placeholders)
future_data <- data.frame(
  temp = mean(train_data$temp, na.rm = TRUE),
  rain = mean(train_data$rain, na.rm = TRUE),
  Anomaly.x = mean(train_data$Anomaly.x, na.rm = TRUE),
  Anomaly.y = mean(train_data$Anomaly.y, na.rm = TRUE)
)

# Predict outcome for 2025
future_prediction <- predict(rf_model, newdata = future_data)

# Print prediction
print(ifelse(future_prediction == 1, "Early Spring", "More Winter"))






  