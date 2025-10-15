rm(list = ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("cb_comms_functions.R")

#***************************************************************************************************
# READ IN FILES HERE ####
#***************************************************************************************************

df <- readRDS("Data.rds")

#***************************************************************************************************
# REGRESSION SPECIFICATIONS ####
#***************************************************************************************************

# descriptive statistics specifications

columns <- c("age", "male", "studied_economics", "proxy_household_income", 
             "financial_literacy", "cb_knowledge", 
             "elicited_12_month_ahead_expectations", "prior_trust")
group_var <- "treatment_group"

# regression specifications

comparisons <- list(
  "1_vs_2" = list(group_var = "group_2", groups = c(1, 2)),
  "1_vs_3" = list(group_var = "group_3", groups = c(1, 3)),
  "2_vs_4" = list(group_var = "group_4", groups = c(2, 4)),
  "3_vs_4" = list(group_var = "group_4", groups = c(3, 4))
)


#***************************************************************************************************
# TABLES ####
#***************************************************************************************************

#### Table 2 ####

descriptive_stats <- calc_stats(df %>% dplyr::filter(attention_check==1),columns,group_var)

generate_descriptive_table(
  descriptive_stats)

#### Table 3 ####

table_3 <- map_dfr(names(comparisons), function(name) {
  comp <- comparisons[[name]]
  run_models(df, comp$groups, comp$group_var, name)
})

generate_latex_table(
  table_3,
  caption = "Average Response to Treatments",
  label = "tab:3",
  file = "table_3.tex",
  notes = "The table reports the average change in inflation expectations of individuals in each treatment group relative to those in the highlighted treatment group. Treatments are described in detail in the text. The second column uses the same specification as the first, but augmented with respondent-specific controls. Results are from a Huber regression. Robust standard errors are reported in parenthesis. * p < 0.1; ** p < 0.05; *** p < 0.01."
)

#### Table A.1 ####

table_a1 <- map_dfr(names(comparisons), function(name) {
  comp <- comparisons[[name]]
  run_models_ols(df, comp$groups, comp$group_var, name)
})

generate_latex_table(
  table_a1,
  caption = "Average Response to Treatments, OLS Regression",
  label = "tab:a1",
  file = "table_a1.tex",
  notes = "The table reports the average change in inflation expectations of individuals in each treatment group relative to those in the highlighted treatment group. Treatments are described in detail in the text. The second column uses the same specification as the first, but augmented with respondent-specific controls. Results are from OLS Regression. Robust standard errors are reported in parenthesis. * p < 0.1; ** p < 0.05; *** p < 0.01."
)

#### Table A.2 ####

table_a2 <- map_dfr(names(comparisons), function(name) {
  comp <- comparisons[[name]]
  run_models_median(df, comp$groups, comp$group_var, name)
})

generate_latex_table(
  table_a2,
  caption = "Average Response to Treatments, Median Regression",
  label = "tab:a2",
  file = "table_a2.tex",
  notes = "The table reports the average change in inflation expectations of individuals in each treatment group relative to those in the highlighted treatment group. Treatments are described in detail in the text. The second column uses the same specification as the first, but augmented with respondent-specific controls. Results are from Median regressions. Robust standard errors are reported in parenthesis. * p < 0.1; ** p < 0.05; *** p < 0.01."
)

#### Table A.3 ####

table_a3 <- map_dfr(names(comparisons), function(name) {
  comp <- comparisons[[name]]
  run_models_trimmed(df, comp$groups, comp$group_var, name)
})

generate_latex_table(
  table_a3,
  caption = "Average Response to Treatments, OLS Regression, Expectation Revisions trimmed at bottom and top 10%",
  label = "tab:a3",
  file = "table_a3.tex",
  notes = "The table reports the average change in inflation expectations of individuals in each treatment group relative to those in the highlighted treatment group. Treatments are described in detail in the text. The second column uses the same specification as the first, but augmented with respondent-specific controls. Results are from OLS regressions. Robust standard errors are reported in parenthesis. * p < 0.1; ** p < 0.05; *** p < 0.01."
)

#### Table 4 ####

table_4 <- map_dfr(names(comparisons), function(name) {
  comp <- comparisons[[name]]
  run_models_probit(df, comp$groups, comp$group_var, name)
})

generate_latex_table(
  table_4,
  caption = "Convergence in Beliefs",
  label = "tab:4",
  file = "table_4.tex",
  notes = "The table reports the probability that subjects' inflation expectation converge to the BCB's target inflation rate of 3% in each treatment group relative to those in the highlighted treatment group. Treatments are described in detail in the text. The second column uses the same specification as the first, but augmented with respondent-specific controls. Results are from Probit regressions. Robust standard errors are reported in parenthesis. * p < 0.1; ** p < 0.05; *** p < 0.01."
)

#***************************************************************************************************
# FIGURES ####
#***************************************************************************************************

#### Figure A1 #####

df %>%
  filter(attention_check==1) %>%
  ggplot(aes(x=delta_expectations,group = treatment_group,fill=as.factor(treatment_group))) +
  geom_density(adjust=1.5,alpha=0.8) +
  scale_fill_paletteer_d("ggsci::default_jama",name="Treatment Group") +
  labs(x = "Revision of Inflation Expectation",
       y = "Density") +
  theme(panel.background = element_rect(fill="white"),
        axis.line.x = element_line(color="black",linewidth = 0.6),
        axis.line.y = element_line(color="black",linewidth = 0.6),
        legend.position = c(0.8,0.8),
        axis.text = element_text(color="black")
  )

#### Figure 1 ####

# Panel (a) #

plt1 <- plot_group_mean(
  data = df %>% filter(!is.na(male)),
  group_var = male,
  var_name = "elicited_12_month_ahead_expectations",
  labels = c("Female", "Male"),
  fills = c("#006633", "#0d4073"),
  x_label = "Gender"
)

# Panel (b) #

income_med <- quantile(df$proxy_household_income, 0.5, na.rm = TRUE)
df <- df %>%
  mutate(income_group = ifelse(proxy_household_income < income_med, 0, 1))

plt2 <- plot_group_mean(
  data = df %>% filter(!is.na(income_group)),
  group_var = income_group,
  var_name = "elicited_12_month_ahead_expectations",
  labels = c("Bottom 50%", "Top 50%"),
  fills = c("#006633", "#0d4073"),
  x_label = "Income Quantile"
)

# Panel (c) #

df <- df %>%
  mutate(FL_group = ifelse(financial_literacy < 2, 0, 1))

plt3 <- plot_group_mean(
  data = df %>% filter(!is.na(FL_group)),
  group_var = FL_group,
  var_name = "elicited_12_month_ahead_expectations",
  labels = c("Under 2", "2 or higher"),
  fills = c("#006633", "#0d4073"),
  x_label = "Financial Literacy Score"
)

# Panel (d) #


# Arranging all plots #

ggarrange(
  plt1, plt2, plt3,
  labels = c("(a)", "(b)", "(c)"),
  font.label = list(size = 10, color = "black"),
  label.x = 0.5,
  nrow = 2, ncol = 2
)

#### Figure 4 ####

df %>%
  dplyr::select(c(6:10)) %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(everything(),
               names_to = "x",
               values_to = "y") %>%
  mutate(
    x = c("Official Sources", "Traditional Media", "Social Media", "Friends", "None"),
    y = y * 100
  ) %>%
  ggplot(aes(x = x, y = y)) +
  geom_bar(stat = "identity", fill = "#003366", color = "#003366", width = 0.6) +
  labs(
    title = NULL,
    x = "Source used",
    y = "Percentage"
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(color = "#0d4073", linewidth = 0.3),
    axis.ticks.length = unit(0, "mm"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(size = 10),
    axis.text.y  = element_text(color = "#0d4073"),
    axis.line.x = element_line(color = "black", linewidth = 0.6)
  )
