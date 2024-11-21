# Load required libraries for modeling
library(dplyr)
library(xgboost)
library(lubridate)
library(tidyverse)
library(zoo)

# IMPORTING DATA
ml_data <- read.csv("ml_data.csv")

# Feature engineering
ml_data <- ml_data %>%
  mutate(
    sales_lag1 = lag(sales, 1),
    sales_lag7 = lag(sales, 7),
    sales_lag30 = lag(sales, 30),
    sales_ma7 = zoo::rollmean(sales, 7, fill = NA, align = "right"),
    month_sin = sin(2 * pi * month/12),
    month_cos = cos(2 * pi * month/12),
    is_promo = sample(c(0,1), n, replace=TRUE, prob=c(0.9, 0.1))
  )

ml_data <- na.omit(ml_data)

# Features for model
features <- c("weekday", "month", "is_holiday", "is_promo", 
              "sales_lag1", "sales_lag7", "sales_lag30", "sales_ma7",
              "month_sin", "month_cos")

# Split data
train_data <- ml_data[ml_data$date <= as.Date("2023-06-30"), ]
test_data <- ml_data[ml_data$date > as.Date("2023-06-30"), ]

# Prepare matrices
train_matrix <- xgb.DMatrix(
  data = as.matrix(train_data[features]),
  label = train_data$sales
)

test_matrix <- xgb.DMatrix(
  data = as.matrix(test_data[features]),
  label = test_data$sales
)

# Train model
final_model <- xgb.train(
  params = list(
    objective = "reg:squarederror",
    max_depth = 6,
    eta = 0.1,
    min_child_weight = 1,
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  data = train_matrix,
  nrounds = 100,
  verbose = 0
)

# Generate predictions
final_predictions <- predict(final_model, test_matrix)

# Save model and test data for dashboard
saveRDS(final_model, "final_model.rds")
saveRDS(test_data, "test_data.rds")
saveRDS(final_predictions, "final_predictions.rds")