################################################################################
# Program Name: 06a_Sensor_Models_DID.R
# Program Purpose: Run main DID models on the sensor data
# Author: Katherine Burley Farr
# Contact: kburley@ad.unc.edu
# Affiliation: UNC Department of Public Policy, Data-Driven EnviroLab
################################################################################

rm(list=ls())

{
  library(tidyverse)
  library(pdftools)
  library(stringr)
  library(sf)
  library(broom)
  library(gtsummary)
  library(lubridate)
  library(readxl)
  library(ggplot2)
  library(ggpubr)
  library(eventstudyr)
  library(fixest)
  library(sandwich)
  library(lmtest)
  library(zoo)
  library(plm)
  library(stats)
  library(clubSandwich)
  library(stargazer)
  library(car)
  library(corrplot)
  library(weathermetrics)
  library(modelsummary)
}

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 1. Bring in Data  ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

did_data <- read_csv("../Data/Analysis/05_Analysis_Data.csv") %>% # Too large for GH, stored on DDL Google Drive
  filter(!month(date) %in% c(6)) 

did_data_hourly <- read_csv("../Data/Analysis/05_Analysis_Data_Hourly.csv") %>%
  filter(!month(date) %in% c(6))

did_data_daily <- read_csv("../Data/Analysis/05_Analysis_Data_Daily.csv") %>%
  filter(!month(date) %in% c(6))

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2. Data Checks  ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Correlations to select controls, accounting for multicollinearity
corr_data <- did_data %>% 
  select(Temperature, temp_f_rdu_fill, dewpoint_f_rdu_fill, rh_pct, 
         precip_in_rdu_fill,sunny, sunny_hours, sunny_hours_l1, daylight_mins,
         daylight_mins_l1, sfc_sw_down_wgt, sfc_sw_down_wgt_l1,
         sfc_sw_down_mean, sfc_sw_down_mean_l1, toh_rad_wm2_fill,
         avg_rad_wm2_fill) %>%
  # Remove missings for correlation
  filter(!is.na(temp_f_rdu_fill)) %>%
  filter(!is.na(sunny_hours_l1)) %>%
  filter(!is.na(sfc_sw_down_mean_l1))

cor_matrix <- cor(corr_data, use="everything", method="pearson")
corrplot(cor_matrix, method = "circle")

corr_data_daily <- did_data_daily %>%
  select(Temperature, temp_f_rdu_fill, dewpoint_f_rdu_fill, rh_pct, 
         precip_in_rdu_fill, sunny_hours, sunny_hours_l1, daylight_mins,
         daylight_mins_l1, sfc_sw_down_wgt, sfc_sw_down_wgt_l1,
         sfc_sw_down_mean, sfc_sw_down_mean_l1, toh_rad_wm2_fill,
         avg_rad_wm2_fill) %>%
  # Remove missings for correlation
  filter(!is.na(temp_f_rdu_fill)) %>%
  filter(!is.na(sunny_hours_l1)) %>%
  filter(!is.na(sfc_sw_down_mean_l1))

cor_matrix_d <- cor(corr_data, use="everything", method="pearson")
corrplot(cor_matrix_d, method = "circle") 
  

# Test for Parallel Pre-Trends - Hourly
pre_trends_hourly_unadj <- lm(Temperature ~ treat_25 + factor(week) + treat_25:factor(week),
                                data=did_data_hourly[did_data_hourly$week>26 & did_data_hourly$week<=36,]) 

summary(pre_trends_hourly_unadj)

# Extract coefficients & CIs
coeffs_pre_h_unadj <- coef(pre_trends_hourly_unadj)
ci_pre_h_unadj <- coefci(pre_trends_hourly_unadj, vcov = vcovCL(pre_trends_hourly_unadj, did_data_hourly[did_data_hourly$week>26 & did_data_hourly$week<=36,]$street_name)) # clustered SE

pre_h_unadj_results <- data.frame(
  Term = names(coeffs_pre_h_unadj),
  Estimate = coeffs_pre_h_unadj,
  CI_2.5 = ci_pre_h_unadj[, 1],
  CI_97.5 = ci_pre_h_unadj[, 2]
)

pre_h_unadj_results <- pre_h_unadj_results %>%
  filter(grepl("week", Term)) %>%
  separate(Term, into=c("Var", "Week"), sep="\\(week\\)") %>%
  mutate(Var = case_when(Var == "factor" ~ "Weeks",
                         Var == "treat_25:factor" ~ "Treatment*Week"),
         Week = as.numeric(Week)) 

pre_h_unadj_plot <- ggplot(pre_h_unadj_results[pre_h_unadj_results$Var == "Treatment*Week",], aes(x = Week, y = Estimate)) +
  geom_point(size = 3, color = "#004351") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "#166a7c") + # Plot the 95% CI
  labs(title = "a. Difference between Treatment and Match Comparison Group (Hourly Data)",
       x = "Treat*Week",
       y = "Estimate (Degrees F)") +
  theme_minimal() +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  scale_x_continuous(breaks = seq(26, 36, by = 1))
pre_h_unadj_plot

pre_h_unadj_plot_week <- ggplot(pre_h_unadj_results[pre_h_unadj_results$Var == "Weeks",], aes(x = Week, y = Estimate)) +
  geom_point(size = 3, color = "#004351") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "#166a7c") + # Plot the 95% CI
  labs(title = "b. Difference in Weekly Average Temperature (Hourly Data)",
       x = "Week",
       y = "Estimate (Degrees F)") +
  theme_minimal() +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  scale_x_continuous(breaks = seq(26, 36, by = 1))
pre_h_unadj_plot_week

ggarrange(pre_h_unadj_plot, pre_h_unadj_plot_week, ncol=1, nrow=2)
ggsave("../Data/Analysis/Figures/06_Pre_Trends_by_week_hourly.png", dpi=300, width=10, height=8)


# Test for Parallel Pre-Trends - Daily
pre_trends_daily_unadj <- lm(Temperature ~ treat_25 + factor(week) + treat_25:factor(week),
                              data=did_data_daily[did_data_daily$week<=36,]) 

summary(pre_trends_daily_unadj)

# Extract coefficients & CIs
coeffs_pre_d_unadj <- coef(pre_trends_daily_unadj)
ci_pre_d_unadj <- coefci(pre_trends_daily_unadj, vcov = vcovCL(pre_trends_daily_unadj, did_data_daily[did_data_daily$week<=36,]$street_name)) # clustered SE

pre_d_unadj_results <- data.frame(
  Term = names(coeffs_pre_d_unadj),
  Estimate = coeffs_pre_d_unadj,
  CI_2.5 = ci_pre_d_unadj[, 1],
  CI_97.5 = ci_pre_d_unadj[, 2]
)

pre_d_unadj_results <- pre_d_unadj_results %>%
  filter(grepl("week", Term)) %>%
  separate(Term, into=c("Var", "Week"), sep="\\(week\\)") %>%
  mutate(Var = case_when(Var == "factor" ~ "Weeks",
                         Var == "treat_25:factor" ~ "Treatment*Week"),
         Week = as.numeric(Week)) 

pre_d_unadj_plot <- ggplot(pre_d_unadj_results[pre_d_unadj_results$Var == "Treatment*Week",], aes(x = Week, y = Estimate)) +
  geom_point(size = 3, color = "#004351") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "#166a7c") + # Plot the 95% CI
  labs(title = "Pre-Treatment Trends - Daily Data - Unadjusted",
       x = "Treat*Week",
       y = "Estimate (Degrees F)") +
  theme_minimal() +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  scale_x_continuous(breaks = seq(26, 36, by = 1))
pre_d_unadj_plot

ggarrange(pre_h_unadj_plot, pre_d_unadj_plot, ncol=1, nrow=2)
ggsave("../Data/Analysis/Figures/06_Pre_Trends_by_week.png", dpi=300, width=10, height=8)

# Test for Parallel Pre-Trends - BY HOUR - Adjusted with Controls
pre_trends_byhour_adj <- lm(Temperature ~ treat_25 + factor(hour) + treat_25:factor(hour) + 
                                temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + 
                                sfc_sw_down_wgt + sfc_sw_down_wgt_l1,
                              data=did_data_hourly[did_data_hourly$week<=36,]) 

summary(pre_trends_byhour_adj)

# Extract coefficients & CIs
coeffs_pre_hour_adj <- coef(pre_trends_byhour_adj)
ci_pre_hour_adj <- coefci(pre_trends_byhour_adj, vcov = vcovCL(pre_trends_byhour_adj, did_data_hourly[did_data_hourly$week<=36,]$street_name)) # clustered SE

pre_hour_adj_results <- data.frame(
  Term = names(coeffs_pre_hour_adj),
  Estimate = coeffs_pre_hour_adj,
  CI_2.5 = ci_pre_hour_adj[, 1],
  CI_97.5 = ci_pre_hour_adj[, 2]
)

pre_hour_adj_results <- pre_hour_adj_results %>%
  filter(grepl("hour", Term)) %>%
  separate(Term, into=c("Var", "Hour"), sep="\\(hour\\)") %>%
  mutate(Var = case_when(Var == "factor" ~ "Hour",
                         Var == "treat_25:factor" ~ "Treatment*Hour"),
         Hour = as.numeric(Hour)) 

pre_hour_adj_plot <- ggplot(pre_hour_adj_results[pre_hour_adj_results$Var == "Treatment*Hour",], aes(x = Hour, y = Estimate)) +
  geom_point(size = 3, color = "#004351") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "#166a7c") + # Plot the 95% CI
  labs(title = "Pre-Treatment Trends - Hour of Day - Adjusted",
       x = "Treat*Hour",
       y = "Estimate (Degrees F)") +
  theme_minimal() +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  scale_x_continuous(breaks = seq(0, 23, by = 1))
pre_hour_adj_plot # differences in hours 8 and 9


# Test for Parallel Pre-Trends - BY HOUR - Unadjusted
pre_trends_byhour_unadj <- lm(Temperature ~ treat_25 + factor(hour) + treat_25:factor(hour),
                              data=did_data_hourly[did_data_hourly$week<=36,]) 

summary(pre_trends_byhour_unadj)

# Extract coefficients & CIs
coeffs_pre_hour_unadj <- coef(pre_trends_byhour_unadj)
ci_pre_hour_unadj <- coefci(pre_trends_byhour_unadj, vcov = vcovCL(pre_trends_byhour_unadj, did_data_hourly[did_data_hourly$week<=36,]$street_name)) # clustered SE

pre_hour_unadj_results <- data.frame(
  Term = names(coeffs_pre_hour_unadj),
  Estimate = coeffs_pre_hour_unadj,
  CI_2.5 = ci_pre_hour_unadj[, 1],
  CI_97.5 = ci_pre_hour_unadj[, 2]
)

pre_hour_unadj_results <- pre_hour_unadj_results %>%
  filter(grepl("hour", Term)) %>%
  separate(Term, into=c("Var", "Hour"), sep="\\(hour\\)") %>%
  mutate(Var = case_when(Var == "factor" ~ "Hour",
                         Var == "treat_25:factor" ~ "Treatment*Hour"),
         Hour = as.numeric(Hour)) 

pre_hour_unadj_plot <- ggplot(pre_hour_unadj_results[pre_hour_unadj_results$Var == "Treatment*Hour",], aes(x = Hour, y = Estimate)) +
  geom_point(size = 3, color = "#004351") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "#166a7c") + # Plot the 95% CI
  labs(title = "Pre-Treatment Trends - Hour of Day - Unadjusted",
       x = "Treat*Hour",
       y = "Estimate (Degrees F)") +
  theme_minimal() +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  scale_x_continuous(breaks = seq(0, 23, by = 1))
pre_hour_unadj_plot # differences in hours 8 and 9

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2. Basic DID  ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. 20 minute interval data ----

# Prepare to cluster standard errors at the STREET level
se_cluster <- function(x) {
  coeftest(x, vcov = vcovCL(x, cluster=did_data$street_name))[, "Std. Error"]
} 

# Basic DID Model
m1 <- lm(Temperature ~ treat_25 + post + treat_post,
         data=did_data) 

summary(m1)

# DID Model + Time Varying Controls
m2 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
           temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + 
           sfc_sw_down_wgt + sfc_sw_down_wgt_l1,
         data=did_data)
summary(m2)

# DID Model + Time Varying Controls + Sensor Characteristics
m3 <- lm(Temperature ~ treat_25 + post + treat_post  + factor(hour) + 
           temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + 
           sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + 
           Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
           + pct_treec,
         data=did_data)
summary(m3)
vif(m3)

rm(m1, m2, m3)
gc()

models_20m <- list(
  # Basic DID Model
  m1 <- lm(Temperature ~ treat_25 + post + treat_post,
             data=did_data),
  # DID Model + Time Varying Controls
  m2 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
             temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + sunny + 
               sfc_sw_down_wgt + sfc_sw_down_wgt_l1,
             data=did_data),
  # DID Model + Time Varying Controls + Sensor Characteristics
  m3 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
             temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + sunny + 
             sfc_sw_down_wgt + sfc_sw_down_wgt_l1 +
             Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
             pct_treec,
             data=did_data)
)

# Table for Models with Clustered Standard Errors
stargazer(models_20m, type = "text", omit="hour") # check var order

stargazer(
  models_20m, type = "html", single.row = TRUE, report = "vc*p",
  column.labels = c("Basic DID", "DID + Time Varying Controls", "DID + All Controls"), omit=c("hour"),
  # REMINDER: if you update the models, change the var labels below:
  covariate.labels = c("Treat","Post","Treat*Post", "RDU Temp. (F)", "RDU RH (%)", "RDU Tot. Precip. (in)",
                       "RDU Sunny Flag", "Surface SW Rad. Down", "Surface SW Rad. Down Lag1",
                       "Shaded", "Sidewalk", "Distance to Street (in)", "Street Width (ft)", "South-Facing",
                       "North-Facing","West-Facing", "Pct Treecover"),
  se = lapply(models_20m, se_cluster), out = "../Data/Results/06_DID_Models_20m.html"
)


# 2. Hourly data ---- 

se_cluster_h <- function(x) {
  coeftest(x, vcov = vcovCL(x, cluster=did_data_hourly$street_name))[, "Std. Error"]
} 
  # vcovCR doesn't work anymore

# Basic DID Model
m1_h <- lm(Temperature ~ treat_25 + post + treat_post,
         data=did_data_hourly) 

summary(m1_h)

# DID Model + Time Varying Controls
m2_h <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
             temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + 
           sfc_sw_down_wgt + sfc_sw_down_wgt_l1,
         data=did_data_hourly)
summary(m2_h)

# DID Model + Time Varying Controls + Sensor Characteristics
m3_h <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
             temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + 
             sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + 
             Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
             pct_treec,
         data=did_data_hourly)
summary(m3_h)
vif(m3_h) 

# removed daylight mins ~ 6 - probably bc of downward daily radiation vars?

rm(m1_h, m2_h, m3_h)
gc()

models_hourly <- list(
  # Basic DID Model
  m1_h <- lm(Temperature ~ treat_25 + post + treat_post,
           data=did_data_hourly),
  # DID Model + Time Varying Controls
  m2_h <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) +
               temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + 
             sfc_sw_down_wgt + sfc_sw_down_wgt_l1,
           data=did_data_hourly),
  # DID Model + Time Varying Controls + Sensor Characteristics
  m3_h <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) +
               temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + 
               sfc_sw_down_wgt + sfc_sw_down_wgt_l1 +
               Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
               pct_treec,
           data=did_data_hourly)
)


# Table for Models with Clustered Standard Errors
stargazer(models_hourly, type = "text", omit="hour") # check var order

stargazer(
  models_hourly, type = "html", single.row = TRUE, report = "vc*p",
  column.labels = c("Basic DID", "DID + Time Varying Controls", "DID + All Controls"), omit=c("hour"), 
  # REMINDER: if you update the models, change the var labels below:
  covariate.labels = c("Treat","Post","Treat*Post", "RDU Temp. (F)", "RDU RH (%)", "RDU Tot. Precip. (in)",
                       "RDU Sunny Flag", "Surface SW Rad. Down", "Surface SW Rad. Down Lag1",
                       "Shaded", "Sidewalk", "Distance to Street (in)", "Street Width (ft)", "South-Facing",
                       "North-Facing","West-Facing", "Pct Treecover"),
  se = lapply(models_hourly, se_cluster_h), out = "../Data/Results/06_DID_Models_Hourly.html"
)


# 2. Daily data ---- 

se_cluster_d <- function(x) {
  coeftest(x, vcov = vcovCL(x, cluster=did_data_daily$street_name))[, "Std. Error"]
} 
# vcovCR doesn't work anymore

# Basic DID Model
m1_d <- lm(Temperature ~ treat_25 + post + treat_post,
           data=did_data_daily) 

summary(m1_d)

# DID Model + Time Varying Controls
m2_d <- lm(Temperature ~ treat_25 + post + treat_post + 
             temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny_hours + 
             sfc_sw_down_wgt + sfc_sw_down_wgt_l1,
           data=did_data_daily)
summary(m2_d)

# DID Model + Time Varying Controls + Sensor Characteristics
m3_d <- lm(Temperature ~ treat_25 + post + treat_post + 
             temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny_hours + 
             sfc_sw_down_wgt + sfc_sw_down_wgt_l1 +
             Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
             pct_treec,
           data=did_data_daily)
summary(m3_d)
vif(m3_d) # daylight_mins close to 7 this time, otherwise all low - daylight_mins removed

rm(m1_d, m2_d, m3_d)
gc()

models_daily <- list(
  # Basic DID Model
  m1_d <- lm(Temperature ~ treat_25 + post + treat_post,
             data=did_data_daily),
  # DID Model + Time Varying Controls
  m2_d <- lm(Temperature ~ treat_25 + post + treat_post + 
               temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny_hours + 
               sfc_sw_down_wgt + sfc_sw_down_wgt_l1,
             data=did_data_daily),
  # DID Model + Time Varying Controls + Sensor Characteristics
  m3_d <- lm(Temperature ~ treat_25 + post + treat_post + 
               temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny_hours + 
               sfc_sw_down_wgt + sfc_sw_down_wgt_l1 +
               Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
               pct_treec,
             data=did_data_daily)
)


# Table for Models with Clustered Standard Errors
stargazer(models_daily, type = "text", omit="hour") # check var order

stargazer(
  models_daily, type = "html", single.row = TRUE, report = "vc*p",
  column.labels = c("Basic DID", "DID + Time Varying Controls", "DID + All Controls"), 
  # REMINDER: if you update the models, change the var labels below:
  covariate.labels = c("Treat","Post","Treat*Post", "RDU Temp. (F)", "RDU RH (%)", "RDU Tot. Precip. (in)",
                       "RDU Total Sunny Hours", "Surface SW Rad. Down", "Surface SW Rad. Down Lag1",
                       "Shaded", "Sidewalk", "Distance to Street (in)", "Street Width (ft)", "South-Facing",
                       "North-Facing","West-Facing", "Pct Treecover"),
  se = lapply(models_daily, se_cluster_d), out = "../Data/Results/06_DID_Models_Daily.html"
)
