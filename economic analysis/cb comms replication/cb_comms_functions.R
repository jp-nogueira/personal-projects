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

