# Testing Guide for House Price Prediction Script

## Prerequisites

Before testing, ensure you have:
- R installed (version 4.0+)
- Required packages: `tidyverse`, `glmnet`, `lubridate`
- Data files: `TrainingData.csv`, `TestData.csv`

## Installation

```r
# Install required packages
install.packages(c("tidyverse", "glmnet", "lubridate"))
```

## Basic Functionality Tests

### Test 1: Script Runs Without Errors
```r
# Set working directory to where your data files are
setwd("path/to/your/data")

# Source the script
source("house_price_prediction.R")

# Expected: Script should complete without errors
# Expected: Console output showing model summary and predictions
```

### Test 2: Verify Data Loading
```r
# After loading, check dimensions
cat("Train rows:", nrow(train), "\n")
cat("Test rows:", nrow(test), "\n")

# Expected: Both should be > 0
# Expected: No error messages about missing files
```

### Test 3: Verify Feature Engineering
```r
# Check that new features were created
expected_features <- c("house_age", "is_new", "has_been_remodeled", 
                       "years_since_remodeled", "bath_to_bed_ratio",
                       "transaction_year", "transaction_month")

for(feature in expected_features) {
  if(feature %in% names(train)) {
    cat("✓", feature, "created\n")
  } else {
    cat("✗", feature, "MISSING\n")
  }
}

# Expected: All features should show ✓
```

### Test 4: Verify Missing Value Handling
```r
# Check for NA values in numeric columns
numeric_cols <- c("improvement_sf", "total_garage_sf", "total_porch_sf",
                  "no_of_fireplace", "no_of_bedroom", "no_of_bathroom")

for(col in numeric_cols) {
  if(col %in% names(train)) {
    na_count <- sum(is.na(train[[col]]))
    cat(col, "- NA count:", na_count, "\n")
  }
}

# Expected: Should be 0 or very low for size/count variables
```

### Test 5: Verify Neighborhood Statistics
```r
# Check neighborhood stats were joined
if("neighborhood_median_price" %in% names(train)) {
  cat("✓ Neighborhood statistics joined successfully\n")
  cat("Non-NA neighborhood prices:", 
      sum(!is.na(train$neighborhood_median_price)), "/", nrow(train), "\n")
} else {
  cat("✗ Neighborhood statistics NOT joined\n")
}

# Expected: Column should exist
# Expected: Most rows should have non-NA values
```

### Test 6: Verify Data Split
```r
# Check splits were created correctly
cat("Training set size:", nrow(training_set), "\n")
cat("Validation set size:", nrow(validation_set), "\n")
cat("Test data size:", nrow(test_data), "\n")

# Verify validation set is Nov-Dec 2011
if(nrow(validation_set) > 0) {
  val_years <- unique(validation_set$transaction_year)
  val_months <- unique(validation_set$transaction_month)
  cat("Validation years:", paste(val_years, collapse=", "), "\n")
  cat("Validation months:", paste(val_months, collapse=", "), "\n")
}

# Expected: All sizes > 0
# Expected: Validation set should only have year=2011, months=11,12
```

### Test 7: Verify Model Matrices Have Same Features
```r
# Check that all matrices have the same columns
cat("X_train columns:", ncol(X_train), "\n")
cat("X_val columns:", ncol(X_val), "\n")
cat("X_test columns:", ncol(X_test), "\n")

# Check column names match
cols_match <- all(colnames(X_train) == colnames(X_val)) && 
              all(colnames(X_train) == colnames(X_test))

if(cols_match) {
  cat("✓ All matrices have matching columns\n")
} else {
  cat("✗ Column mismatch detected!\n")
}

# Expected: All should have same number of columns
# Expected: Column names should match exactly
```

### Test 8: Verify Model Training
```r
# Check model object exists and is valid
if(exists("cv_model") && class(cv_model)[1] == "cv.glmnet") {
  cat("✓ Model trained successfully\n")
  cat("Lambda min:", cv_model$lambda.min, "\n")
  cat("Lambda 1se:", cv_model$lambda.1se, "\n")
} else {
  cat("✗ Model training failed\n")
}

# Expected: Model object should exist
# Expected: Lambda values should be positive numbers
```

### Test 9: Verify Predictions Are Reasonable
```r
# Check validation predictions
cat("Validation predictions:\n")
cat("  Min:", format(min(val_pred), big.mark=",", digits=0), "\n")
cat("  Median:", format(median(val_pred), big.mark=",", digits=0), "\n")
cat("  Max:", format(max(val_pred), big.mark=",", digits=0), "\n")
cat("  Any NA:", any(is.na(val_pred)), "\n")

# Check test predictions
cat("\nTest predictions:\n")
cat("  Min:", format(min(test_pred), big.mark=",", digits=0), "\n")
cat("  Median:", format(median(test_pred), big.mark=",", digits=0), "\n")
cat("  Max:", format(max(test_pred), big.mark=",", digits=0), "\n")
cat("  Any NA:", any(is.na(test_pred)), "\n")

# Expected: All predictions > 12000 (filter threshold)
# Expected: No NA values
# Expected: Reasonable range for house prices
```

### Test 10: Verify No Data Leakage
```r
# Neighborhood stats should come from training only
train_neighborhoods <- unique(train_only$neighborhood_code)
val_neighborhoods <- unique(validation_set$neighborhood_code)

# All validation neighborhoods should exist in training
unknown_neighborhoods <- setdiff(val_neighborhoods, train_neighborhoods)
if(length(unknown_neighborhoods) == 0) {
  cat("✓ No data leakage: all validation neighborhoods in training\n")
} else {
  cat("⚠ Warning: validation has", length(unknown_neighborhoods), 
      "neighborhoods not in training\n")
}

# Expected: No unknown neighborhoods (or they should have imputed values)
```

## Edge Case Tests

### Test 11: Small Dataset
Create a small test dataset to verify the pipeline works:

```r
# Create minimal test data
small_train <- train[1:100, ]
small_test <- test[1:10, ]

# Run through pipeline (would need to wrap in function)
# Expected: Should complete without errors even with small data
```

### Test 12: Missing Neighborhood
```r
# Check what happens with neighborhoods not in training
new_neighborhood <- data.frame(
  neighborhood_code = "NEW_CODE",
  # ... other required fields ...
)

# This neighborhood should get NA for stats, which is OK
# The model should still make predictions
```

## Performance Tests

### Test 13: Validation RMSE
```r
# RMSE should be reasonable
cat("Validation RMSE: $", format(val_rmse, big.mark=",", digits=0), "\n")

# Calculate MAPE (Mean Absolute Percentage Error)
mape <- mean(abs((y_val - val_pred) / y_val)) * 100
cat("Validation MAPE:", round(mape, 2), "%\n")

# Expected: RMSE depends on house prices in your area
# Expected: MAPE typically < 20% for a decent model
```

### Test 14: Coefficient Analysis
```r
# Check which features are most important
coef_matrix <- coef(cv_model, s = "lambda.min")
coef_df <- data.frame(
  feature = rownames(coef_matrix),
  coefficient = as.vector(coef_matrix)
) %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient)))

cat("Top 10 most important features:\n")
print(head(coef_df, 10))

# Expected: Should show reasonable features (e.g., square footage, location)
# Expected: Coefficients should make sense (positive for good features)
```

## Troubleshooting

### Common Issues

**Issue**: "Error: object 'X_train' not found"
- **Solution**: Run the entire script from the beginning

**Issue**: "Error in read.csv: cannot open file"
- **Solution**: Check that CSV files are in the working directory
- **Solution**: Set working directory with `setwd()`

**Issue**: "Columns don't match between train and test"
- **Solution**: Ensure both datasets have the same structure
- **Solution**: Check for typos in column names

**Issue**: "Neighborhood stats are all NA"
- **Solution**: Check that neighborhood_code column exists in both datasets
- **Solution**: Verify train data has sale_price values

**Issue**: "Model predictions are all similar"
- **Solution**: Check feature variance - might need feature engineering
- **Solution**: Try different alpha values (0=ridge, 1=lasso)

## Success Criteria

The script is working correctly if:
- ✓ All data loads without errors
- ✓ Feature engineering creates expected columns
- ✓ No NA values in model matrices
- ✓ Train/val/test splits are correct sizes
- ✓ All model matrices have matching columns
- ✓ Model trains successfully
- ✓ Predictions are in reasonable range
- ✓ No warnings about missing or mismatched data
- ✓ RMSE is reasonable for your dataset

## Reporting Issues

If you encounter problems:
1. Note the exact error message
2. Check which test failed
3. Verify data file format matches expected structure
4. Check R and package versions
5. Review the BUG_ANALYSIS.md document for known issues
