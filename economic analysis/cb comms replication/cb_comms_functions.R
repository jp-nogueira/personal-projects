#### Helper functions for the main analysis ####

#***************************************************************************************************
# Load packages ####
#***************************************************************************************************

installed <- require(tidyverse)
if (!installed) install.packages("tidyverse")
installed <- require(MASS)
if (!installed) install.packages("MASS")
installed <- require(stats)
if (!installed) install.packages("stats")
installed <- require(lmtest)
if (!installed) install.packages("lmtest")
installed <- require(sandwich)
if (!installed) install.packages("sandwich")
installed <- require(knitr)
if (!installed) install.packages("knitr")
installed <- require(texreg)
if (!installed) install.packages("texreg")
installed <- require(kableExtra)
if (!installed) install.packages("kableExtra")
installed <- require(paletteer)
if (!installed) install.packages("paletteer")

#***************************************************************************************************
# Helper functions ####
#***************************************************************************************************

#' Function to generate table with descriptive statistics
#' 
#' @param data data for analysis
#' @param cols columns from the data, which we desire to summarize
#' @param group_var column from data that indicates to each group participants belong

calc_stats <- function(data, cols, group_var) {
  
  # Function to calculate stats for a single column
  calc_single_col <- function(data, col, group_var) {
    # Calculate stats by group
    stats <- data %>%
      group_by(!!sym(group_var)) %>%
      summarise(
        mean = round(mean(!!sym(col), na.rm = TRUE), 2),
        sd = round(sd(!!sym(col), na.rm = TRUE), 2)
      )
    
    # Calculate total stats
    total_stats <- data %>%
      summarise(
        mean = round(mean(!!sym(col), na.rm = TRUE), 2),
        sd = round(sd(!!sym(col), na.rm = TRUE), 2)
      )
    
    # Perform one-way ANOVA
    formula_str <- paste(col, "~", group_var)
    anova_result <- aov(as.formula(formula_str), data = data)
    anova_summary <- summary(anova_result)
    p_value <- round(anova_summary[[1]]["Pr(>F)"][1, 1], 4)
    
    # Reshape stats to wide format
    stats_wide <- stats %>%
      pivot_longer(cols = c(mean, sd), 
                   names_to = "stat_type", 
                   values_to = "value") %>%
      mutate(col_name = paste0(!!sym(group_var), "_", stat_type)) %>%
      dplyr::select(-stat_type, -!!sym(group_var)) %>%
      pivot_wider(names_from = col_name, values_from = value)
    
    # Create final data frame
    result <- data.frame(
      variable = col,
      stats_wide,
      total_mean = total_stats$mean,
      total_sd = total_stats$sd,
      p_value = p_value
    )
    
    return(result)
  }
  
  # Apply to all columns and combine
  results <- lapply(cols, function(col) {
    calc_single_col(data, col, group_var)
  })
  
  # Combine all results into one data frame
  do.call(rbind, results)
}

#' Helper function to run pairs of Huber robust regressions and extract results
#' 
#' @param data Data to run the regression
#' @param groups Determines which groups are being compared in the regression
#' @param group_var Determines which group is the Treatment group
#' @param comparison_name Names the groups being compared in the regressions 
run_models <- function(data, groups, group_var, comparison_name) {
  subset_condition <- data$treatment_group %in% groups & data$attention_check == 1
  
  # Formulas with correct group var substituted
  f_nocontrols <- as.formula(paste("delta_expectations ~", group_var))
  f_controls <- as.formula(paste("delta_expectations ~", group_var, "+ age + proxy_household_income + studied_economics"))
  
  models <- list(
    NoControls = rlm(f_nocontrols, data = data, subset = subset_condition),
    WithControls = rlm(f_controls, data = data, subset = subset_condition)
  )
  
  results <- map(models, ~{
    ct <- coeftest(.x, vcov = vcovHC(.x, type = "HC3"))
    tidy_ct <- broom::tidy(ct) %>% filter(term == group_var)
    tidy_ct[, c("estimate", "std.error", "p.value")]
  })
  
  bind_cols(results) %>%
    rename_with(~ paste0(.x, c("_nocontrols", "_controls"))) %>%
    mutate(comparison = comparison_name, .before = 1)
}

#' Helper function to run pairs of OLS regressions and extract results
#' 
#' @param data Data to run the regression
#' @param groups Determines which groups are being compared in the regression
#' @param group_var Determines which group is the Treatment group
#' @param comparison_name Names the groups being compared in the regressions
run_models_ols <- function(data, groups, group_var, comparison_name) {
  subset_condition <- data$treatment_group %in% groups & data$attention_check == 1
  
  # Define formulas
  f_nocontrols <- as.formula(paste("delta_expectations ~", group_var))
  f_controls <- as.formula(paste("delta_expectations ~", group_var, "+ age + proxy_household_income + studied_economics"))
  
  models <- list(
    NoControls = lm(f_nocontrols, data = data, subset = subset_condition),
    WithControls = lm(f_controls, data = data, subset = subset_condition)
  )
  
  results <- map(models, ~{
    ct <- coeftest(.x, vcov = vcovHC(.x, type = "HC3"))
    tidy_ct <- broom::tidy(ct) %>% filter(term == group_var)
    tidy_ct[, c("estimate", "std.error", "p.value")]
  })
  
  bind_cols(results) %>%
    rename_with(~ paste0(.x, c("_nocontrols", "_controls"))) %>%
    mutate(comparison = comparison_name, .before = 1)
}

#' Helper function to run pairs of Median regressions and extract results
#' 
#' @param data Data to run the regression
#' @param groups Determines which groups are being compared in the regression
#' @param group_var Determines which group is the Treatment group
#' @param comparison_name Names the groups being compared in the regressions
run_models_median <- function(data, groups, group_var, comparison_name) {
  subset_condition <- data$treatment_group %in% groups & data$attention_check == 1
  
  # Define formulas
  f_nocontrols <- as.formula(paste("delta_expectations ~", group_var))
  f_controls <- as.formula(paste("delta_expectations ~", group_var, "+ age + proxy_household_income + studied_economics"))
  
  # Estimate median (quantile) regressions
  models <- list(
    NoControls = rq(f_nocontrols, tau = 0.5, data = data, subset = subset_condition),
    WithControls = rq(f_controls, tau = 0.5, data = data, subset = subset_condition)
  )
  
  # Extract coefficients, SEs, and p-values (robust)
  results <- map(models, ~{
    ct <- summary(.x, se = "boot")  # use bootstrap SEs for quantile regression
    tidy_ct <- broom::tidy(ct) %>% filter(term == group_var)
    tidy_ct[, c("estimate", "std.error", "p.value")]
  })
  
  bind_cols(results) %>%
    rename_with(~ paste0(.x, c("_nocontrols", "_controls"))) %>%
    mutate(comparison = comparison_name, .before = 1)
}

#' Helper function to run pairs of trimmed OLS regressions and extract results
#' 
#' @param data Data to run the regression
#' @param groups Determines which groups are being compared in the regression
#' @param group_var Determines which group is the Treatment group
#' @param comparison_name Names the groups being compared in the regressions
run_models_trimmed <- function(data, groups, group_var, comparison_name) {
  # Subset data first by group condition
  df_sub <- data %>%
    filter(treatment_group %in% groups, attention_check == 1)
  
  # Trim the dependent variable
  lower <- quantile(df_sub$delta_expectations, 0.10, na.rm = TRUE)
  upper <- quantile(df_sub$delta_expectations, 0.90, na.rm = TRUE)
  df_trim <- df_sub %>%
    filter(delta_expectations >= lower, delta_expectations <= upper)
  
  # Define formulas
  f_nocontrols <- as.formula(paste("delta_expectations ~", group_var))
  f_controls <- as.formula(paste("delta_expectations ~", group_var, "+ age + proxy_household_income + studied_economics"))
  
  # Estimate OLS regressions on trimmed sample
  models <- list(
    NoControls = lm(f_nocontrols, data = df_trim),
    WithControls = lm(f_controls, data = df_trim)
  )
  
  # Extract coefficients, SEs, and p-values
  results <- map(models, ~{
    ct <- coeftest(.x, vcov = vcovHC(.x, type = "HC3"))
    tidy_ct <- broom::tidy(ct) %>% filter(term == group_var)
    tidy_ct[, c("estimate", "std.error", "p.value")]
  })
  
  bind_cols(results) %>%
    rename_with(~ paste0(.x, c("_nocontrols", "_controls"))) %>%
    mutate(comparison = comparison_name, .before = 1)
}

#' Helper function to run pairs of Probit regressions and extract results
#' 
#' @param data Data to run the regression
#' @param groups Determines which groups are being compared in the regression
#' @param group_var Determines which group is the Treatment group
#' @param comparison_name Names the groups being compared in the regressions
run_models_probit <- function(data, groups, group_var, comparison_name) {
  subset_condition <- data$treatment_group %in% groups & data$attention_check == 1
  
  # Define formulas
  f_nocontrols <- as.formula(paste("closer_to_target ~", group_var))
  f_controls <- as.formula(paste("closer_to_target ~", group_var, "+ age + proxy_household_income + studied_economics"))
  
  # Estimate probit models
  models <- list(
    NoControls = glm(f_nocontrols, family = binomial(link = "probit"), data = data, subset = subset_condition),
    WithControls = glm(f_controls, family = binomial(link = "probit"), data = data, subset = subset_condition)
  )
  
  # Extract coefficient, robust SE, and p-value for group_var
  results <- map(models, ~{
    ct <- coeftest(.x, vcov = vcovHC(.x, type = "HC1"))
    tidy_ct <- broom::tidy(ct) %>% filter(term == group_var)
    tidy_ct[, c("estimate", "std.error", "p.value")]
  })
  
  bind_cols(results) %>%
    rename_with(~ paste0(.x, c("_nocontrols", "_controls"))) %>%
    mutate(comparison = comparison_name, .before = 1)
}