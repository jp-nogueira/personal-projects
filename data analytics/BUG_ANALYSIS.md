# Detailed Bug Analysis and Fixes

## Critical Bugs in Original Code

### Bug 1: Redundant Preprocessing (Lines ~40-80 and ~150-180)
**Problem:**
```r
# First time - applied to train
train <- train %>%
  dplyr::mutate(house_age = year(transaction_date) - built_year) %>%
  dplyr::mutate(is_new = ifelse(house_age<=2,1,0)) %>%
  # ... more mutations

# Much later - applied to test (DUPLICATE CODE)
test <- test %>%
  dplyr::mutate(house_age = year(transaction_date) - built_year) %>%
  dplyr::mutate(is_new = ifelse(house_age<=2,1,0)) %>%
  # ... same mutations
```

**Why It's Bad:**
- Violates DRY (Don't Repeat Yourself) principle
- Increases risk of inconsistencies if one block is updated but not the other
- Makes code harder to maintain

**Fix:**
Created `engineer_features()` function applied consistently to both datasets.

---

### Bug 2: Missing Value Median Calculated After Column Removal
**Problem:**
```r
# Columns removed from train
train <- train %>%
  dplyr::select(-all_of(cols_to_remove))

# Later, trying to use train medians for test
test[[col]][is.na(test[[col]])] <- median(train[[col]], na.rm = TRUE)
```

**Why It's Bad:**
- If a column was removed from `train`, `median(train[[col]])` will fail
- Causes runtime errors or incorrect imputation

**Fix:**
Calculate and store medians BEFORE removing columns:
```r
train_medians <- list()
for(col in c("average_story_height", "total_net_acres")) {
  train_medians[[col]] <- median(train[[col]], na.rm = TRUE)
}
# Then remove columns
# Then use stored medians for test set
```

---

### Bug 3: Neighborhood Stats Calculated After Potential Data Loss
**Problem:**
Original code's order could cause issues where neighborhood stats might be calculated after modifications to the data.

**Fix:**
Ensure neighborhood statistics are calculated from clean training data before any column removal:
```r
# 1. Engineer features
# 2. Handle missing values
# 3. Calculate neighborhood stats
# 4. THEN remove columns
```

---

### Bug 4: Formula Recreation and Matrix Rebuilding
**Problem:**
```r
# First creation around line 90
formula_str <- paste("sale_price ~", paste(feature_cols, collapse = " + "))
X_train <- model.matrix(formula_obj, data = training_set)[, -1]

# ... code ...

# DUPLICATE creation around line 160
formula_str <- paste("sale_price ~", paste(feature_cols, collapse = " + "))
X_train <- model.matrix(formula_obj, data = training_set)[, -1]
```

**Why It's Bad:**
- Wastes computation rebuilding the same matrices
- Creates confusion about which version is used
- Risk of variable overwrites

**Fix:**
Create matrices once, use feature alignment only where needed.

---

### Bug 5: Confusing Test Data Variables
**Problem:**
```r
test <- read.csv("TestData.csv")  # External test file
# ... later ...
test_data <- train %>%
  filter(predict == 1)  # Internal "test" set from training data
```

**Why It's Confusing:**
- Two different concepts both called "test"
- `test` is external, `test_data` is internal
- Never clear which one to use for final predictions

**Fix:**
Maintained clear distinction:
- `test`: External test CSV data
- `test_data`: Internal records marked for prediction in training file
- Added comments explaining each

---

### Bug 6: Feature Alignment Complexity
**Problem:**
```r
# Multiple steps to align features
missing_cols <- setdiff(feature_cols, names(test))
available_features <- intersect(feature_cols, names(test))
# ... complex logic ...
common_cols <- intersect(colnames(X_train), colnames(X_val))
if(ncol(X_test) > 0) {
  common_cols <- intersect(common_cols, colnames(X_test))
  # ...
}
```

**Why It's Bad:**
- Overly complex with nested conditions
- Hard to debug if feature mismatch occurs
- Error-prone

**Fix:**
Simplified to three clean steps:
```r
# 1. Create matrices for train, val, and test
# 2. Find common columns across all three
common_cols <- intersect(intersect(colnames(X_train), colnames(X_val)), colnames(X_test))
# 3. Subset all three to common columns
```

---

### Bug 7: Character to Factor Conversion Timing
**Problem:**
```r
train <- train %>%
  mutate(across(where(is.character), as.factor))
# But this happens AFTER some categorical columns were already converted to factors
```

**Why It's Bad:**
- Redundant conversions
- Could cause unexpected behavior if a column is factor in one place but character in another

**Fix:**
Consolidate factor conversion in one place after all preprocessing.

---

## Best Practices Applied

1. **Function Extraction**: Common operations in helper functions
2. **Clear Ordering**: Logical flow from raw data to model
3. **Single Source of Truth**: Train medians calculated once, used everywhere
4. **Defensive Programming**: Check column existence before operations
5. **Meaningful Comments**: Section markers and explanations
6. **Error Prevention**: Using `any_of()` instead of `all_of()` for flexible column removal

## Testing Recommendations

1. **Test with missing values**: Ensure imputation works correctly
2. **Test with small datasets**: Verify no dimension mismatches
3. **Test feature alignment**: Ensure all three matrices have same columns
4. **Test neighborhood stats**: Verify new neighborhoods in test get appropriate defaults
5. **Validate predictions**: Check that predicted values are reasonable
