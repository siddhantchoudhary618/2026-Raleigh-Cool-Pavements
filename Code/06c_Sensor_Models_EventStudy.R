################################################################################
# Program Name: 06c_Sensor_Models_EventStudy.R
# Program Purpose: Run event study models by week using hourly and daily data
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

did_data <- read_csv("Data/Analysis/05_Analysis_Data.csv") %>% # Too large for GH, stored on DDL Google Drive
  filter(!month(date) %in% c(6)) 

did_data_hourly <- read_csv("Data/Analysis/05_Analysis_Data_Hourly.csv") %>%
  filter(!month(date) %in% c(6))

did_data_daily <- read_csv("Data/Analysis/05_Analysis_Data_Daily.csv") %>%
  filter(!month(date) %in% c(6))

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2. Event Study  ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get treatment weeks - starting from 6/28/2025 as the Saturday before data starts
# this helps group the treatment days into cohorts - except for week 12 which still just has 4 sensors
did_data_hourly_es_1 <- did_data_hourly %>% # did_data_hourly %>%
  mutate(datetime = make_datetime(year = year(date),
                                  month = month(date),
                                  day = day(date),
                                  hour = hour,
                                  min = 0,
                                  sec=0,
                                  tz = "Etc/GMT+4")) %>%
  filter(randomize!=101) %>% # to get a longer pre-period
  mutate(days_since_start = as.numeric(date_dt - make_date(year=2025, month=6, day=28))) %>%
  mutate(wks_since_start = days_since_start/7) %>%
  mutate(weeks_since_start = floor(wks_since_start)) %>%
  select(-c(days_since_start, wks_since_start))

treatment_weeks <- did_data_hourly_es_1 %>%
  distinct(date, date_dt, weeks_since_start) %>%
  filter(date %in% c("2025-09-12", "2025-09-25", "2025-10-07","2025-10-09",
                     "2025-10-10", "2025-10-18", "2025-10-21","2025-10-22",
                     "2025-10-23")) %>%
  select(date_dt, weeks_since_start) %>%
  rename(treatment_date = date_dt,
         treatment_week = weeks_since_start) %>%
  mutate(reference_week = treatment_week - 1)

##### Event Study Models: Daily Data ----
es_data_daily <- did_data_daily %>%
  mutate(days_since_start = as.numeric(date_dt - make_date(year=2025, month=6, day=28))) %>%
  mutate(wks_since_start = days_since_start/7) %>%
  mutate(weeks_since_start = floor(wks_since_start)) %>%
  select(-c(days_since_start, wks_since_start)) %>%
  select(-c(time_unit,treatment_week)) %>%
  # Bring in new treatment week assignment
  left_join(treatment_weeks, by="treatment_date") %>%
  mutate(time_unit = weeks_since_start - treatment_week) %>%
  filter(time_unit>=-11 & time_unit <= 4) %>%
  filter(randomize != 101) %>%
  filter(treatment_week != weeks_since_start) %>% # exclude obs during week of treatment
  mutate(time_unit = relevel(factor(time_unit), ref=11)) # to make -1 the ref week
  

es_testing_d <- lm(Temperature ~ treat_25 + factor(time_unit) + treat_25:factor(time_unit) + 
                     rh_pct + precip_in_rdu_fill + sunny_hours + temp_f_rdu_fill + 
                     sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + pct_treec,
                 data=es_data_daily) 


# Extract coefficients & CIs
coeffs_es_d <- coef(es_testing_d)
ci_es_d <- coefci(es_testing_d, vcov = vcovCL(es_testing_d, es_data_daily$street_name)) # clustered SE

es_d_results <- data.frame(
  Term = names(coeffs_es_d),
  Estimate = coeffs_es_d,
  CI_2.5 = ci_es_d[, 1],
  CI_97.5 = ci_es_d[, 2]
)

es_d_results <- es_d_results %>%
  filter(grepl("time_unit", Term)) %>%
  separate(Term, into=c("Var", "Weeks"), sep="\\(time_unit\\)") %>%
  mutate(Var = case_when(Var == "factor" ~ "Weeks",
                         Var == "treat_25:factor" ~ "Treatment*Weeks"),
         Weeks = as.numeric(Weeks)) 

summary(es_testing_d)
vif(es_testing_d)

# Note: these coefplots cannot be combined
# Note: these coefplots also do not seem to be using clustered std errors
coefplot(es_testing_d, keep="^treat_25:")
coefplot(es_testing_d, keep="^treat_25:", vcov = vcovCL(es_testing_d, es_data_daily$street_name))
coefci(es_testing_d, keep="^treat_25:", vcov = vcovCL(es_testing_d, es_data_daily$street_name))

coefplot(es_testing_d, keep="^factor\\(time_unit\\)", vcov = vcovCL(es_testing_d, es_data_daily$street_name))


es_d_t <- ggplot(es_d_results[es_d_results$Var == "Treatment*Weeks",], aes(x = Weeks, y = Estimate)) +
  geom_point(size = 3, color = "#004351") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "#166a7c") + # Plot the 95% CI
  labs(title = "Event Study Model - Daily Data",
       x = "Treat*Weeks Before Treatment",
       y = "Estimate (Degrees F)") +
  theme_minimal() +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  geom_vline(xintercept = 0,
             color = "#EF446F",
             style="dashed",
             size = 1) + 
  scale_x_continuous(breaks = seq(-11, 4, by = 1))

es_d_w <- ggplot(es_d_results[es_d_results$Var == "Weeks",], aes(x = Weeks, y = Estimate)) +
  geom_point(size = 3, color = "#004351") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "#166a7c") + # Plot the 95% CI
  labs(title = "",
       x = "Weeks Before Treatment",
       y = "Estimate (Degrees F)") +
  theme_minimal() +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  scale_x_continuous(breaks = seq(-11, 3, by = 1))

ggarrange(es_d_t, es_d_w, ncol=1)
ggsave("Data/Analysis/Figures/Event_Study_Daily_Temp.png", dpi=300, height=8, width=10)

  
##### Event Study Models: Hourly Data ----

did_data_hourly_es <- did_data_hourly_es_1 %>%
  select(-c(treatment_week)) %>%
  left_join(treatment_weeks, by="treatment_date") %>%
  mutate(reference_week = treatment_week - 1) %>%
  # exclude week of treatment (week 0)
  filter(treatment_week != weeks_since_start) %>%
  mutate(time_unit = weeks_since_start - treatment_week)

es_data <- did_data_hourly_es %>% 
  # left_join(missing, by=c("date_dt","hour")) %>%
  filter(time_unit >= -11 & time_unit<=4) %>% # keep weeks with all sensors
  mutate(time_unit_day = date - treatment_date) %>%
  mutate(time_unit_fct = relevel(factor(time_unit), ref=11)) # to make -1 the ref week


# OVERALL
es_testing <- lm(Temperature ~ treat_25 + factor(time_unit_fct) + treat_25:factor(time_unit_fct) + 
                     factor(hour) + rh_pct + precip_in_rdu_fill + sunny + temp_f_rdu_fill + 
                     sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + pct_treec, 
                   data=es_data) 

vif(es_testing) # test for multicollinearity, we anticipate more multicollinearity between the ES week indicators and our time-varying controls

summary(es_testing)

# Extract coefficients & CIs
coeffs_es_h <- coef(es_testing)
ci_es_h <- coefci(es_testing, vcov = vcovCL(es_testing, es_data$street_name)) # clustered SE

es_h_results <- data.frame(
  Term = names(coeffs_es_h),
  Estimate = coeffs_es_h,
  CI_2.5 = ci_es_h[, 1],
  CI_97.5 = ci_es_h[, 2]
)

es_h_results <- es_h_results %>%
  filter(grepl("time_unit_fct", Term)) %>%
  separate(Term, into=c("Var", "Weeks"), sep="\\(time_unit_fct\\)") %>%
  mutate(Var = case_when(Var == "factor" ~ "Weeks",
                         Var == "treat_25:factor" ~ "Treatment*Weeks"),
         Weeks = as.numeric(Weeks)) 

summary(es_testing)
vif(es_testing)

es_h_t <- ggplot(es_h_results[es_h_results$Var == "Treatment*Weeks",], aes(x = Weeks, y = Estimate)) +
  geom_point(size = 3, color = "#004351") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "#166a7c") + # Plot the 95% CI
  labs(title = "Event Study Model - Hourly Data",
       x = "Treat*Weeks Before Treatment",
       y = "Estimate (Degrees F)") +
  theme_minimal() +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  geom_vline(xintercept = 0,
             color = "#EF446F",
             style="dashed",
             size = 1) + 
  scale_x_continuous(breaks = seq(-11, 4, by = 1))
es_h_t
ggsave("Data/Analysis/Figures/Event_Study_Hourly_Temp_TreatPostOnly.png", dpi=300, height=8, width=10)

es_h_w <- ggplot(es_h_results[es_h_results$Var == "Weeks",], aes(x = Weeks, y = Estimate)) +
  geom_point(size = 3, color = "#004351") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "#166a7c") + # Plot the 95% CI
  labs(title = "",
       x = "Weeks Before Treatment",
       y = "Estimate (Degrees F)") +
  theme_minimal() +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  scale_x_continuous(breaks = seq(-11, 4, by = 1))

ggarrange(es_h_t, es_h_w, ncol=1)
ggsave("Data/Analysis/Figures/Event_Study_Hourly_Temp.png", dpi=300, height=8, width=10)


# COMBINE DAILY AND HOURLY RESULTS
ggarrange(es_d_t, es_h_t, ncol=1)
ggsave("Data/Analysis/Figures/Event_Study_Hourly_Daily.png", dpi=300, height=8, width=10)

