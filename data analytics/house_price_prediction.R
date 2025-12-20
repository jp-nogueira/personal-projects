# House Price Prediction Model
# This script trains an elastic net regression model to predict house sale prices

# Setup and Libraries ----
rm(list = ls())

# Note: setwd() with rstudioapi is removed to make script more portable
# Users should set their working directory manually or run from the correct location

library(tidyverse)
library(glmnet)
library(lubridate)

# Data Loading ----
train <- read.csv("TrainingData.csv") %>%
  dplyr::mutate(transaction_date = dmy(transaction_date))

test <- read.csv("TestData.csv") %>%
  dplyr::mutate(transaction_date = dmy(transaction_date))

# Helper Function: Feature Engineering ----
engineer_features <- function(df) {
  df %>%
    filter(sale_price > 12000 | is.na(sale_price)) %>%
    dplyr::mutate(
      house_age = year(transaction_date) - built_year,
      is_new = ifelse(house_age <= 2, 1, 0),
      has_been_remodeled = ifelse(remodeled_year != 0 & !is.na(remodeled_year), 1, 0),
      years_since_remodeled = ifelse(has_been_remodeled == 1, 
                                     year(transaction_date) - remodeled_year, 
                                     NA),
      # Correct remodeling flags if remodeling happened after transaction
      has_been_remodeled = ifelse(years_since_remodeled < 0, 0, has_been_remodeled),
      years_since_remodeled = ifelse(years_since_remodeled < 0, NA, years_since_remodeled),
      # Additional features
      bath_to_bed_ratio = no_of_bedroom / pmax(no_of_bathroom, 1),
      vacant_flag = ifelse(vacant_flag == "Y", 1, 0),
      walkout_basement_flag = ifelse(walkout_basement_flag == "Y", 1, 0),
      transaction_year = year(transaction_date),
      transaction_month = month(transaction_date)
    )
}

# Helper Function: Handle Missing Values ----
handle_missing_values <- function(df, reference_medians = NULL) {
  # Numeric columns
  numeric_cols <- c("improvement_sf", "total_garage_sf", "total_porch_sf",
                    "total_finished_basement_sf", "total_unfinished_basement_sf",
                    "no_of_fireplace", "no_of_bedroom", "no_of_bathroom",
                    "built_as_sf", "no_of_story", "average_story_height",
                    "total_net_acres", "sprinkler_coverage_sf")
  
  for(col in numeric_cols) {
    if(col %in% names(df)) {
      # For size/count variables, NA often means 0
      if(grepl("sf$|^no_of|^total", col)) {
        df[[col]][is.na(df[[col]])] <- 0
      } else {
        # For other numerics, use median from reference or self
        if(!is.null(reference_medians) && col %in% names(reference_medians)) {
          df[[col]][is.na(df[[col]])] <- reference_medians[[col]]
        } else {
          df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
        }
      }
    }
  }
  
  # Categorical columns
  categorical_cols <- c("style", "quality_code", "condition", "hvac",
                        "exterior_construction_type", "interior_finish_type",
                        "roof_material_code", "floor_material_type_code",
                        "built_as_code", "property_type_code")
  
  for(col in categorical_cols) {
    if(col %in% names(df)) {
      df[[col]][is.na(df[[col]]) | df[[col]] == ""] <- "Unknown"
      df[[col]] <- as.factor(df[[col]])
    }
  }
  
  return(df)
}

# Apply Feature Engineering ----
train <- engineer_features(train)
test <- engineer_features(test)

# Calculate medians from training data before any modifications
train_medians <- list()
for(col in c("average_story_height", "total_net_acres")) {
  if(col %in% names(train)) {
    train_medians[[col]] <- median(train[[col]], na.rm = TRUE)
  }
}

# Handle Missing Values ----
train <- handle_missing_values(train)
test <- handle_missing_values(test, reference_medians = train_medians)

# Calculate Neighborhood Statistics from Training Data ----
neighborhood_stats <- train %>%
  filter(!is.na(sale_price)) %>%
  group_by(neighborhood_code) %>%
  summarize(
    neighborhood_median_price = median(sale_price, na.rm = TRUE),
    neighborhood_avg_price = mean(sale_price, na.rm = TRUE),
    neighborhood_count = n(),
    .groups = 'drop'
  )

# Join neighborhood statistics
train <- train %>%
  left_join(neighborhood_stats, by = "neighborhood_code")

test <- test %>%
  left_join(neighborhood_stats, by = "neighborhood_code")

# Define columns to remove (before removing them)
cols_to_remove <- c("address_number", "pre_direction_code", "street_name",
                    "street_type_code", "unit_no", "location_zip_code", 
                    "city_name", "grantor", "grantee", 
                    "section", "township", "range", "quarter",
                    "land_economic_area_code", "style", "built_as", 
                    "quality_code", "property_type_code", "roof_construction_type_code",
                    "exterior_construction_type", "interior_finish_type",
                    "roof_material_code", "floor_material_type_code",
                    "transaction_date")

# Remove unnecessary columns
train <- train %>%
  dplyr::select(-any_of(cols_to_remove)) %>%
  mutate(across(where(is.character), as.factor))

test <- test %>%
  dplyr::select(-any_of(cols_to_remove)) %>%
  mutate(across(where(is.character), as.factor))

# Split into Training/Validation/Test Sets ----
# Training data (exclude records marked for prediction)
train_only <- train %>%
  filter(predict != 1 | is.na(predict))

# Test data (records marked for prediction)
test_data <- train %>%
  filter(predict == 1)

# Validation set: Nov-Dec 2011 transactions
validation_set <- train_only %>%
  filter(transaction_year == 2011 & transaction_month %in% c(11, 12))

# Training set: all other records
training_set <- train_only %>%
  anti_join(validation_set, by = "property_id")

# Prepare Model Matrices ----
# Define feature columns (exclude target and ID columns)
feature_cols <- setdiff(names(training_set), c("sale_price", "property_id", "predict"))

# Create formula
formula_str <- paste("sale_price ~", paste(feature_cols, collapse = " + "))
formula_obj <- as.formula(formula_str)

# Create model matrices for training and validation
X_train <- model.matrix(formula_obj, data = training_set)[, -1]  # Remove intercept
y_train <- training_set$sale_price

X_val <- model.matrix(formula_obj, data = validation_set)[, -1]
y_val <- validation_set$sale_price

# Prepare test set model matrix
available_features <- intersect(feature_cols, names(test))
formula_test <- as.formula(paste("~", paste(available_features, collapse = " + ")))
X_test_temp <- model.matrix(formula_test, data = test)

# Remove intercept if present
if("(Intercept)" %in% colnames(X_test_temp)) {
  X_test <- X_test_temp[, -1, drop = FALSE]
} else {
  X_test <- X_test_temp
}

# Align columns across all datasets
common_cols <- intersect(colnames(X_train), colnames(X_val))
common_cols <- intersect(common_cols, colnames(X_test))

X_train <- X_train[, common_cols, drop = FALSE]
X_val <- X_val[, common_cols, drop = FALSE]
X_test <- X_test[, common_cols, drop = FALSE]

# Log transform target variable
y_train_log <- log(y_train)
y_val_log <- log(y_val)

# Train Elastic Net Model ----
set.seed(123)
cv_model <- cv.glmnet(
  x = X_train,
  y = y_train_log,
  alpha = 0.5,  # Elastic net (0.5 = equal mix of ridge and lasso)
  nfolds = 10,
  type.measure = "mse"
)

# Model Summary ----
cat("\nModel trained successfully!\n")
cat("Number of features:", ncol(X_train), "\n")
cat("Training set size:", nrow(X_train), "\n")
cat("Validation set size:", nrow(X_val), "\n")
cat("Test set size:", nrow(X_test), "\n")
cat("Best lambda:", cv_model$lambda.min, "\n")

# Make Predictions ----
# Predictions on validation set
val_pred_log <- predict(cv_model, newx = X_val, s = "lambda.min")
val_pred <- exp(val_pred_log)

# Calculate validation RMSE
val_rmse <- sqrt(mean((y_val - val_pred)^2))
cat("Validation RMSE: $", format(val_rmse, big.mark = ",", digits = 0), "\n", sep = "")

# Predictions on test set
test_pred_log <- predict(cv_model, newx = X_test, s = "lambda.min")
test_pred <- exp(test_pred_log)

# Store predictions
test$predicted_price <- test_pred

# Display first few predictions
cat("\nFirst few test predictions:\n")
print(head(data.frame(
  property_id = test$property_id,
  predicted_price = round(test_pred, 0)
), 10))
