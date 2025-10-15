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
installed <- require(quantreg)
if (!installed) install.packages("quantreg")
installed <- require(knitr)
if (!installed) install.packages("paletteer")
installed <- require(ggpubr)
if (!installed) install.packages("ggpubr")


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
  
  f_nocontrols <- as.formula(paste("delta_expectations ~", group_var))
  f_controls <- as.formula(paste("delta_expectations ~", group_var, "+ age + male + proxy_household_income + studied_economics"))
  
  extract_stats <- function(m) {
    ct <- coeftest(m, vcov = vcovHC(m, type = "HC3"))
    tidy_ct <- broom::tidy(ct) %>% filter(term == group_var)
    tibble(
      estimate = tidy_ct$estimate,
      std.error = tidy_ct$std.error,
      p.value = tidy_ct$p.value
    )
  }
  
  m_nocontrols <- rlm(f_nocontrols, data = data, subset = subset_condition)
  m_controls   <- rlm(f_controls, data = data, subset = subset_condition)
  
  bind_cols(
    extract_stats(m_nocontrols) %>% rename_with(~ paste0(.x, "_nocontrols")),
    extract_stats(m_controls)   %>% rename_with(~ paste0(.x, "_controls"))
  ) %>%
    mutate(comparison = comparison_name, method = "Huber", .before = 1)
}

#' Helper function to run pairs of OLS regressions and extract results
#' 
#' @param data Data to run the regression
#' @param groups Determines which groups are being compared in the regression
#' @param group_var Determines which group is the Treatment group
#' @param comparison_name Names the groups being compared in the regressions
run_models_ols <- function(data, groups, group_var, comparison_name) {
  subset_condition <- data$treatment_group %in% groups & data$attention_check == 1
  
  f_nocontrols <- as.formula(paste("delta_expectations ~", group_var))
  f_controls <- as.formula(paste("delta_expectations ~", group_var, "+ age + male + proxy_household_income + studied_economics"))
  
  extract_stats <- function(m) {
    ct <- coeftest(m, vcov = vcovHC(m, type = "HC3"))
    tidy_ct <- broom::tidy(ct) %>% filter(term == group_var)
    tibble(
      estimate = tidy_ct$estimate,
      std.error = tidy_ct$std.error,
      p.value = tidy_ct$p.value
    )
  }
  
  m_nocontrols <- lm(f_nocontrols, data = data, subset = subset_condition)
  m_controls   <- lm(f_controls, data = data, subset = subset_condition)
  
  bind_cols(
    extract_stats(m_nocontrols) %>% rename_with(~ paste0(.x, "_nocontrols")),
    extract_stats(m_controls)   %>% rename_with(~ paste0(.x, "_controls"))
  ) %>%
    mutate(comparison = comparison_name, method = "OLS", .before = 1)
}

run_models_median <- function(data, groups, group_var, comparison_name) {
  subset_condition <- data$treatment_group %in% groups & data$attention_check == 1
  
  f_nocontrols <- as.formula(paste("delta_expectations ~", group_var))
  f_controls <- as.formula(paste("delta_expectations ~", group_var, "+ age + male + proxy_household_income + studied_economics"))
  
  extract_stats <- function(m) {
    set.seed(1331)
    s <- summary(m, se = "boot")
    coefs <- as.data.frame(s$coefficients)
    row <- coefs[rownames(coefs) == group_var, , drop = FALSE]
    tibble(
      estimate = row[1, "Value"],
      std.error = row[1, "Std. Error"],
      p.value = row[1, "Pr(>|t|)"]
    )
  }
  
  m_nocontrols <- rq(f_nocontrols, tau = 0.5, data = data, subset = subset_condition)
  m_controls   <- rq(f_controls, tau = 0.5, data = data, subset = subset_condition)
  
  bind_cols(
    extract_stats(m_nocontrols) %>% rename_with(~ paste0(.x, "_nocontrols")),
    extract_stats(m_controls)   %>% rename_with(~ paste0(.x, "_controls"))
  ) %>%
    mutate(comparison = comparison_name, method = "Median", .before = 1)
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
  f_controls <- as.formula(paste("delta_expectations ~", group_var, "+ age + male + proxy_household_income + studied_economics"))
  
  # Estimate OLS regressions on trimmed sample
  model_nocontrols <- lm(f_nocontrols, data = df_trim)
  model_controls   <- lm(f_controls, data = df_trim)
  
  # Function to extract stats
  extract_stats <- function(model, group_var) {
    ct <- coeftest(model, vcov = vcovHC(model, type = "HC3"))
    tidy_ct <- broom::tidy(ct) %>% filter(term == group_var)
    tidy_ct[, c("estimate", "std.error", "p.value")]
  }
  
  # Extract and label correctly
  res_nocontrols <- extract_stats(model_nocontrols, group_var) %>%
    rename_with(~ paste0(.x, "_nocontrols"))
  
  res_controls <- extract_stats(model_controls, group_var) %>%
    rename_with(~ paste0(.x, "_controls"))
  
  # Combine both and add labels
  bind_cols(
    tibble(comparison = comparison_name),
    res_nocontrols,
    res_controls
  )
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
  f_controls   <- as.formula(paste("closer_to_target ~", group_var, 
                                   "+ age + male + proxy_household_income + studied_economics"))
  
  # Fit models
  model_nocontrols <- glm(f_nocontrols, family = binomial(link = "probit"),
                          data = data, subset = subset_condition)
  model_controls   <- glm(f_controls, family = binomial(link = "probit"),
                          data = data, subset = subset_condition)
  
  # Function to extract coefficient, robust SE, and p-value
  extract_stats <- function(model, group_var) {
    ct <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
    tidy_ct <- broom::tidy(ct) %>% filter(term == group_var)
    tidy_ct[, c("estimate", "std.error", "p.value")]
  }
  
  # Extract results for each model and label properly
  res_nocontrols <- extract_stats(model_nocontrols, group_var) %>%
    rename_with(~ paste0(.x, "_nocontrols"))
  
  res_controls <- extract_stats(model_controls, group_var) %>%
    rename_with(~ paste0(.x, "_controls"))
  
  # Combine results and return tidy output
  bind_cols(
    tibble(comparison = comparison_name),
    res_nocontrols,
    res_controls
  )
}

#' Helper function to plot group means
#' 
#' @param data Data to plot the figure
#' @param group_var Determines the variable which we're plotting
#' @param labels Determines the name of the group variables
#' @param fills Determines the colors of each group variable
#' @param x_label Label for the X axis
plot_group_mean <- function(data, group_var, var_name, labels, fills, x_label) {
  intercepts <- data %>%
    group_by({{ group_var }}) %>%
    summarise(Intercept = coef(rlm(reformulate("1", var_name), data = cur_data()))[1]) %>%
    mutate({{ group_var }} := factor({{ group_var }}, labels = labels))
  
  ggplot(intercepts, aes(x = {{ group_var }}, y = Intercept, fill = {{ group_var }})) +
    geom_bar(stat = "identity", width = 0.6, color = "black") +
    geom_text(aes(label = round(Intercept, 2)),
              colour = "white", vjust = 1.5, size = 7) +
    labs(x = x_label, y = "Mean") +
    scale_fill_manual(values = fills) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(color = "#0d4073", linewidth = 0.3),
      axis.ticks.length = unit(0, "mm"),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(size = 10),
      axis.text.y  = element_text(color = "#0d4073"),
      legend.position = "None"
    )
}

#' Helper function to generate .tex file with regression tables
#' 
#' @param df Data Frame with regression results 
#' @param caption Table caption
#' @param label Table label
#' @param file Name of the table output
#' @param notes Table footnotes
generate_latex_table <- function(df,
                                 caption = "Caption",
                                 label = "tab:N",
                                 file = "table_results.tex",
                                 notes = "The table reports the average change in inflation expectations of individuals in each treatment group relative to those in the highlighted treatment group. Treatments are described in detail in the text. The second column uses the same specification as the first, but augmented with respondent-specific controls. Results are from Regression type. Robust standard errors are reported in parenthesis. * p < 0.1; ** p < 0.05; *** p < 0.01.") {
  
  # Helper function to add significance stars
  add_stars <- function(est, pval) {
    stars <- ifelse(pval < 0.01, "***",
                    ifelse(pval < 0.05, "**",
                           ifelse(pval < 0.1, "*", "")))
    sprintf("%.3f%s", est, stars)
  }
  
  # Apply stars to estimates
  df <- df %>%
    dplyr::mutate(
      est_nc = add_stars(estimate_nocontrols, p.value_nocontrols),
      est_c  = add_stars(estimate_controls, p.value_controls)
    )
  
  # Construct LaTeX table rows
  rows <- c()
  
  for (comp in unique(df$comparison)) {
    subset <- df[df$comparison == comp, ]
    comp_name <- switch(comp,
                        "1_vs_2" = "\\multicolumn{3}{l}{\\textbf{Relative to original COPOM statement (Treatment 1)}} \\\\",
                        "1_vs_3" = "\\multicolumn{3}{l}{\\textbf{Relative to original COPOM statement (Treatment 1)}} \\\\",
                        "3_vs_4" = "\\multicolumn{3}{l}{\\textbf{Relative to condensed COPOM statement (Treatment 3)}} \\\\",
                        "2_vs_4" = "\\multicolumn{3}{l}{\\textbf{Relative to original G1 article (Treatment 2)}} \\\\",
                        paste0("\\multicolumn{3}{l}{\\textbf{Comparison ", comp, "}} \\\\")
    )
    
    rows <- c(rows, comp_name)
    rows <- c(rows,
              sprintf("T%s & %s & %s \\\\", 
                      gsub(".*_vs_", "", comp),
                      subset$est_nc,
                      subset$est_c))
    rows <- c(rows,
              sprintf("& (%.3f) & (%.3f) \\\\", 
                      subset$std.error_nocontrols,
                      subset$std.error_controls),
              "\\midrule")
  }
  
  body <- paste(rows, collapse = "\n")
  
  latex <- sprintf("
\\begin{table}[H]
\\centering
\\caption{%s}
\\label{%s}
\\begin{tabular}{lcc}
\\toprule
\\textbf{} & \\multicolumn{2}{c}{\\textbf{Inflation Expectations}}\\\\ 
\\cmidrule(lr){2-3}
& (1) & (2) \\\\ 
\\midrule
%s
Demographic Controls & No & Yes \\\\
Remove Outliers & Yes & Yes \\\\
\\bottomrule
\\end{tabular}
\\begin{minipage}{\\textwidth}
    {\\fontsize{8}{8}\\selectfont\\textit{Notes:} %s}
\\end{minipage}
\\end{table}", caption, label, body, notes)
  
  # Write to file
  writeLines(latex, file)
  message("✅ LaTeX table written to: ", normalizePath(file))
  
  invisible(latex)
}

#' Helper function to generate .tex file with Descriptive Statistics table
#' 
#' @param df Data frame with table 
#' @param caption Table caption
#' @param label Table label
#' @param file Name of the table output
#' @param notes Table footnotes
generate_descriptive_table <- function(df,
                                       caption = "Descriptive Statistics",
                                       label = "tab:1",
                                       file = "table_descriptive.tex",
                                       notes = "The table reports the mean and standard deviation (in parenthesis) for the demographics as well as the prior beliefs about the economy of the subjects in each treatment group. Household Income is the mean household income of the subjects' self reported neighborhood of residence. Trust in the Central Bank refers to trust in the BCB to care about the economic well-being of all Brazilians. The final column displays the p-values from a one-way Analysis of Variance (ANOVA) test, which compares the means across groups to verify whether they are significantly different.") {
  # Normalize column names to lowercase
  names(df) <- tolower(names(df))
  
  # Helper to format mean (sd)
  fmt <- function(mean, sd) sprintf("%.2f (%.2f)", mean, sd)
  
  # Build each row
  rows <- apply(df, 1, function(x) {
    sprintf("%s & %s & %s & %s & %s & %s & %.3f\\\\\n\\addlinespace",
            gsub("_", " ", x["variable"]),  # prettify variable names
            fmt(as.numeric(x["x1_mean"]), as.numeric(x["x1_sd"])),
            fmt(as.numeric(x["x2_mean"]), as.numeric(x["x2_sd"])),
            fmt(as.numeric(x["x3_mean"]), as.numeric(x["x3_sd"])),
            fmt(as.numeric(x["x4_mean"]), as.numeric(x["x4_sd"])),
            fmt(as.numeric(x["total_mean"]), as.numeric(x["total_sd"])),
            as.numeric(x["p_value"]))
  })
  
  body <- paste(rows, collapse = "\n")
  
  # LaTeX structure
  latex <- sprintf("
\\begin{table}[!h]
\\centering
\\caption{%s}
\\label{%s}
\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{
\\begin{tabular}[t]{llllllr}
\\toprule
Variable & Group 1 & Group 2 & Group 3 & Group 4 & Total & p-value\\\\
\\midrule
%s
\\bottomrule
\\end{tabular}}
\\begin{minipage}{\\textwidth}
    {\\fontsize{8}{8}\\selectfont\\textit{Notes:} %s}
\\end{minipage}
\\end{table}
", caption, label, body, notes)
  
  # Save file
  writeLines(latex, file)
  message("✅ LaTeX table written to: ", normalizePath(file))
  
  invisible(latex)
}

