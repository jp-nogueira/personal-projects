# Quick Reference: Common R Data Science Issues and Solutions

This guide provides quick solutions to the issues found in the original house price prediction script.

## Issue 1: Code Duplication in Data Preprocessing

### ❌ Bad Practice
```r
# Preprocessing train
train <- train %>%
  mutate(house_age = year(transaction_date) - built_year) %>%
  mutate(is_new = ifelse(house_age <= 2, 1, 0))

# Same code repeated for test
test <- test %>%
  mutate(house_age = year(transaction_date) - built_year) %>%
  mutate(is_new = ifelse(house_age <= 2, 1, 0))
```

### ✅ Good Practice
```r
# Create a function
preprocess_data <- function(df) {
  df %>%
    mutate(house_age = year(transaction_date) - built_year) %>%
    mutate(is_new = ifelse(house_age <= 2, 1, 0))
}

# Apply to both
train <- preprocess_data(train)
test <- preprocess_data(test)
```

---

## Issue 2: Using Train Statistics After Data Modification

### ❌ Bad Practice
```r
train <- train %>% select(-some_column)
# Later: trying to get median from removed column
test$some_column[is.na(test$some_column)] <- median(train$some_column, na.rm = TRUE)  # ERROR!
```

### ✅ Good Practice
```r
# Store statistics FIRST
train_stats <- list(
  some_column_median = median(train$some_column, na.rm = TRUE)
)

# THEN modify data
train <- train %>% select(-some_column)

# Use stored statistics
test$some_column[is.na(test$some_column)] <- train_stats$some_column_median
```

---

## Issue 3: Feature Engineering Order

### ❌ Bad Practice
```r
# Remove columns first
train <- train %>% select(-address, -transaction_date)

# Try to use removed columns (ERROR!)
train <- train %>%
  mutate(transaction_year = year(transaction_date))
```

### ✅ Good Practice
```r
# Create derived features FIRST
train <- train %>%
  mutate(transaction_year = year(transaction_date))

# THEN remove source columns
train <- train %>% select(-address, -transaction_date)
```

---

## Issue 4: Model Matrix Feature Alignment

### ❌ Bad Practice
```r
# Complex and error-prone
X_train <- model.matrix(formula, train)[, -1]
X_test <- model.matrix(formula, test)[, -1]
# Features might not match!
```

### ✅ Good Practice
```r
# Create matrices
X_train <- model.matrix(formula, train)[, -1]
X_val <- model.matrix(formula, val)[, -1]
X_test <- model.matrix(formula, test)[, -1]

# Find common features
common_cols <- Reduce(intersect, list(
  colnames(X_train), 
  colnames(X_val), 
  colnames(X_test)
))

# Subset all to common features
X_train <- X_train[, common_cols, drop = FALSE]
X_val <- X_val[, common_cols, drop = FALSE]
X_test <- X_test[, common_cols, drop = FALSE]
```

---

## Issue 5: Handling Missing Categorical Values

### ❌ Bad Practice
```r
# Different handling in different places
train$category[is.na(train$category)] <- "Unknown"
# ... later ...
train$category[train$category == ""] <- "Missing"  # Inconsistent!
```

### ✅ Good Practice
```r
# Consistent handling in one place
handle_categorical <- function(df, cols) {
  for(col in cols) {
    df[[col]][is.na(df[[col]]) | df[[col]] == ""] <- "Unknown"
    df[[col]] <- as.factor(df[[col]])
  }
  return(df)
}
```

---

## Issue 6: Safe Column Removal

### ❌ Bad Practice
```r
# Fails if columns don't exist
train <- train %>% select(-all_of(cols_to_remove))  # ERROR if column missing
```

### ✅ Good Practice
```r
# Safe removal - no error if column doesn't exist
train <- train %>% select(-any_of(cols_to_remove))
```

---

## Issue 7: Preserving Reference Data

### ❌ Bad Practice
```r
# Overwriting data before using it
train <- train %>% filter(condition)
# ... later need original train for test preprocessing (LOST!)
```

### ✅ Good Practice
```r
# Keep reference copy
train_reference <- train

# Create working copies
train_working <- train %>% filter(condition)

# Use reference for test preprocessing
test <- process_with_reference(test, train_reference)
```

---

## General Best Practices

1. **Calculate statistics before modifying data**
2. **Create functions for repeated operations**
3. **Process train and test consistently**
4. **Store reference data before modifications**
5. **Use meaningful variable names**
6. **Comment complex operations**
7. **Test with small datasets first**

---

## Checklist for ML Preprocessing Pipeline

- [ ] Load raw data
- [ ] Engineer features (same function for train/test)
- [ ] Calculate statistics from training data
- [ ] Handle missing values (use train stats for test)
- [ ] Create categorical features
- [ ] Join lookup tables (e.g., neighborhood stats)
- [ ] Remove unnecessary columns
- [ ] Convert character to factor
- [ ] Split into train/val/test
- [ ] Create model matrices
- [ ] Align features across datasets
- [ ] Transform target variable if needed
- [ ] Train model
- [ ] Validate predictions

This order ensures no data leakage and consistent preprocessing!
