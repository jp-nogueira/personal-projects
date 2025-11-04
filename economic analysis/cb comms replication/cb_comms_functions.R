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
installed <- require(paletteer)
if (!installed) install.packages("paletteer")
installed <- require(ggpubr)
if (!installed) install.packages("ggpubr")
installed <- require(rlang)
if (!installed) install.packages("rlang")


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

#' Helper function to prepare data to be ploted as a histogram
#' 
#' @param df Data to be prepared
#' @param var Variable whose frequency we wish to determine
#' @param orig_breaks Original breaks from panel (a) figure (to keep both figures consistent)
#' @param min_val Minimum value for the X-axis
#' @param max_value Maximum value for the X-axis 
prepare_histogram <- function(df,
                              var = "elicited_12_month_ahead_expectations",
                              orig_breaks,
                              min_val = -14,
                              max_val = 14) {
  # --- 1. Compute interval width from original breaks ---
  w <- orig_breaks[2] - orig_breaks[1]
  
  # --- 2. Extend breaks to cover full range while staying aligned ---
  k <- ceiling((orig_breaks[1] - min_val) / w)
  first_break <- orig_breaks[1] - k * w
  n_steps <- ceiling((max_val - first_break) / w)
  breaks_ext <- seq(from = first_break, by = w, length.out = n_steps + 1)
  
  # --- 3. Filter data ---
  filtered_df <- df %>%
    filter(
      .data[["attention_check"]] == 1,
      !is.na(.data[[var]])
    )
  
  # --- 4. Count observations per bin ---
  bins_factor <- cut(filtered_df[[var]],
                     breaks = breaks_ext,
                     right = FALSE,
                     include.lowest = TRUE)
  
  counts <- as.integer(table(bins_factor))
  percent <- counts / sum(counts) * 100
  
  # --- 5. Assemble plotting data frame ---
  plot_df <- data.frame(
    class_start = breaks_ext[-length(breaks_ext)],
    class_end = breaks_ext[-1],
    n = counts,
    percent = percent
  )
  
  plot_df <- plot_df %>%
    mutate(across(c(class_start, class_end, percent), ~ round(., 2)))
  
  invisible(plot_df)
  
}

#' Helper function to plot group means
#' 
#' @param data Data to plot the figure
#' @param group_var Determines the variable which we're plotting
#' @param labels Determines the name of the group variables
#' @param fills Determines the colors of each group variable
#' @param x_label Label for the X axis
plot_group_mean <- function(data, group_var, var_name, labels, fills, x_label) {
  # Compute robust means per group (using rlm on a constant)
  intercepts <- data %>%
    group_by({{ group_var }}) %>%
    summarise(Intercept = coef(rlm(reformulate("1", var_name), data = cur_data()))[1]) %>%
    mutate({{ group_var }} := factor({{ group_var }}, labels = labels))
  
  # Plot violins + robust mean lines
  ggplot(data, aes(x = factor({{ group_var }}, labels = labels),
                   y = .data[[var_name]],
                   fill = factor({{ group_var }}, labels = labels))) +
    geom_violin(trim = FALSE, color = "black", alpha = 0.7) +
    
    # Add robust mean line for each group
    geom_segment(data = intercepts,
                 aes(x = as.numeric({{ group_var }}) - 0.3,
                     xend = as.numeric({{ group_var }}) + 0.3,
                     y = Intercept, yend = Intercept),
                 color = "white", linewidth = 1.2, linetype = "dashed") +
    
    # Add text labels above the line
    geom_text(data = intercepts,
              aes(x = {{ group_var }}, y = Intercept, label = round(Intercept, 2)),
              vjust = -1.2, color = "white", size = 5, fontface = "bold") +
    
    labs(x = x_label, y = "Value") +
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

#' Helper function to plot binned scatterplot 
#' 
#' @param df Data Frame with regression results 
#' @param x_var X-axis variable
#' @param y_var Y-axis variable
#' @param bins Number of bins
#' @param title Plot title
#' @param x_label X-axis label
#' @param y_label Y-axis label
plot_expectations_vs_inflation <- function(df,
                                           x_var = past_12_month_inflation,
                                           y_var = elicited_12_month_ahead_expectations,
                                           bins = 12,
                                           title = " ",
                                           x_label = "Perceived Inflation, %",
                                           y_label = "Expected Inflation, %") {
  # Variables and names
  xq <- enquo(x_var)
  yq <- enquo(y_var)
  x_name <- as_name(xq)
  y_name <- as_name(yq)
  
  # Filtering data
  df_filt <- df %>%
    filter(
      !is.na(.data[[x_name]]),
      !is.na(.data[[y_name]]),
      attention_check == 1,
      .data[[x_name]] > -2,
      .data[[x_name]] < 12,
      .data[[y_name]] > -2,
      .data[[y_name]] < 12
    )
  
  # Formula for regressions
  frm <- as.formula(paste0(y_name, " ~ ", x_name))
  
  # Regressions
  m7 <- rlm(frm, data = df_filt)
  ols <- lm(frm, data = df_filt)
  
  # R-squared
  r2_val <- summary(ols)$r.squared
  r2_label <- bquote(R^2 == .(round(r2_val, 2)))
  
  # Data for the plot
  plt_df <- df_filt %>%
    mutate(predicted = predict(m7))
  
  # Plots the bin scatter
  p <- ggplot(plt_df, aes(x = !!xq, y = !!yq)) +
    stat_summary_bin(fun = mean, bins = bins, geom = "point", color = "#0d4073", size = 2) +
    geom_line(aes(y = predicted), color = "#d40000", size = 1) +
    annotate("text", x = Inf, y = -Inf, label = as.expression(r2_label),
             hjust = 1.1, vjust = -1.2, size = 4, color = "#d40000", parse = TRUE) +
    scale_x_continuous(breaks = seq(-2, 12, by = 2), labels = function(x) round(x, 1)) +
    scale_y_continuous(breaks = seq(-2, 12, by = 2), labels = function(y) round(y, 1)) +
    labs(title = title, x = x_label, y = y_label) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(color = "lightgrey", linewidth = 0.1),
      axis.line.x = element_line(color = "black", linewidth = 0.6),
      axis.line.y = element_line(color = "black", linewidth = 0.6),
      axis.ticks = element_line(color = "black"),
      axis.text = element_text(color = "black")
    )
  
  p
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

