rm(list = ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

set.seed(1331)

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

formulas <- list(
  "NoControls" = ~ .x ~ group_var_placeholder,
  "WithControls" = ~ .x ~ group_var_placeholder + age + male + proxy_household_income + studied_economics
)


#***************************************************************************************************
# TABLES ####
#***************************************************************************************************

#### Table 2 ####

descriptive_stats <- calc_stats(df %>% dplyr::filter(attention_check==1),columns,group_var)


#### Table 3 ####

table_3 <- map_dfr(names(comparisons), function(name) {
  comp <- comparisons[[name]]
  run_models(df, comp$groups, comp$group_var, name)
})

#### Table A.1 ####

table_a1 <- map_dfr(names(comparisons), function(name) {
  comp <- comparisons[[name]]
  run_models_ols(df, comp$groups, comp$group_var, name)
})

#### Table A.2 ####

table_a2 <- map_dfr(names(comparisons), function(name) {
  comp <- comparisons[[name]]
  run_models_median(df, comp$groups, comp$group_var, name)
})

#### Table A.3 ####

table_a3 <- map_dfr(names(comparisons), function(name) {
  comp <- comparisons[[name]]
  run_models_trimmed(df, comp$groups, comp$group_var, name)
})

#### Table 4 ####

table_4 <- map_dfr(names(comparisons), function(name) {
  comp <- comparisons[[name]]
  run_models_probit(df, comp$groups, comp$group_var, name)
})

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

#### Figure 4 ####

plt_data <- df %>%
  dplyr::select(c(6:10))
means <- colMeans(plt_data)
plt_df <- data.frame(
  x = c("Official Sources","Traditional Media","Social Media","Friends","None"),
  y = means*100,
  row.names = NULL
)

ggplot(plt_df, aes(x = x, y = y)) +
  geom_bar(stat = "identity", fill = "#003366", color = "#003366", width = 0.6) +
  labs(title = " ",
       x = "Source used",
       y = "Percentage") +
  scale_y_continuous(expand = c(0,0)) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(color = "#0d4073", linewidth = 0.3),
    axis.ticks.length = unit(0, "mm"),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(size=10),
    axis.text.y  = element_text(color="#0d4073"),
    axis.line.x = element_line(color="black",linewidth = 0.6)
  )

rm(plt_df,plt_data,means)

