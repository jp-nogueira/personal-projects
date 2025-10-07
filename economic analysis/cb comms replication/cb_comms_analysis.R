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


# color and size formatting for plots


#***************************************************************************************************
# TABLES AND FIGURES ####
#***************************************************************************************************

#### Table 2 ####

descriptive_stats <- calc_stats(df %>% dplyr::filter(attention_check==1),columns,group_var)

