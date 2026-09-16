################################################################################
# Program Name: 06b_Sensor_Models_DID_wModifiers.R
# Program Purpose: Test DID results across various time-varying and time invariant modifiers, by running separate models
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
setwd("/Users/siddhantchoudhary/Downloads/raleigh-cool-pavements-main")

did_data <- read_csv("Data/Analysis/05_Analysis_Data.csv") %>% # Too large for GH, stored on DDL Google Drive
  filter(!month(date) %in% c(6)) 

did_data_hourly <- read_csv("Data/Analysis/05_Analysis_Data_Hourly.csv") %>%
  filter(!month(date) %in% c(6))

did_data_daily <- read_csv("Data/Analysis/05_Analysis_Data_Daily.csv") %>%
  filter(!month(date) %in% c(6))

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2. Treatment Effects by Hour  ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# LOOP THROUGH EACH HOUR AND ESTIMATE IMPACT
hours_sequence <- seq(from=0, to=23)
results_df <- data.frame()

for (h in hours_sequence) {
  model_name <- paste("did_hour_",h,sep="")
  model <- lm(Temperature ~ treat_25 + post + treat_post + 
                temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + 
                sfc_sw_down_wgt + sfc_sw_down_wgt_l1 +
                Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
                pct_treec,
              data=did_data_hourly[did_data_hourly$hour==h,]) 

  # Extract coefficients & CIs
  coeffs_i <- coef(model)
  ci_i <- coefci(model, vcov = vcovCL(model, cluster=did_data_hourly[did_data_hourly$hour==h,]$street_name)) # clustered SE
  
  # Combine into a data frame row
  row_data <- data.frame(
    Hour = h,
    Term = names(coeffs_i),
    Estimate = coeffs_i,
    CI_2.5 = ci_i[, 1],
    CI_97.5 = ci_i[, 2]
  )
  
  results_df <- rbind(results_df, row_data)
} 

hourly_results <- results_df %>%
  filter(Term=="treat_post")

ggplot(hourly_results, aes(x = Hour, y = Estimate)) +
  geom_point(size = 3, color = "#004351") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "#166a7c") + # Plot the 95% CI
  labs(title = "Treat-Post Coefficient in Each Hour with 95% CI",
       x = "Hour",
       y = "Estimate (Degrees F)") +
  theme_minimal() +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  xlim(0,23) +
  scale_x_continuous(breaks = 0:23)

ggsave("Data/Analysis/Figures/Hourly_Coefficients.png", dpi=300)

# Compare Treatment Effects to Other Model Coefficients by Hour:

hourly_results_shade <- results_df %>%
  filter(Term=="ShadeYes")

ggplot(hourly_results_shade, aes(x = Hour, y = Estimate)) +
  geom_point(size = 3, color = "blue") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "red") + # Plot the 95% CI
  labs(title = "Value with 95% Confidence Intervals",
       x = "Hour",
       y = "Estimate (Degrees F)") +
  theme_minimal() +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  xlim(0,23) +
  scale_x_continuous(breaks = 0:23)

hourly_results_sunny <- results_df %>%
  filter(Term=="sunny")

ggplot(hourly_results_sunny, aes(x = Hour, y = Estimate)) +
  geom_point(size = 3, color = "blue") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "red") + # Plot the 95% CI
  labs(title = "Value with 95% Confidence Intervals",
       x = "Hour",
       y = "Estimate") +
  theme_minimal() +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  xlim(0,23) +
  scale_x_continuous(breaks = 0:23) # weird patterns here 

hourly_results_sfc <- results_df %>%
  filter(Term=="sfc_sw_down_wgt")

ggplot(hourly_results_sfc, aes(x = Hour, y = Estimate)) +
  geom_point(size = 3, color = "blue") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "red") + # Plot the 95% CI
  labs(title = "Value with 95% Confidence Intervals",
       x = "Hour",
       y = "Estimate") +
  theme_minimal() +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  xlim(0,23) +
  scale_x_continuous(breaks = 0:23)

hourly_results_sfcl1 <- results_df %>%
  filter(Term=="sfc_sw_down_wgt_l1")

ggplot(hourly_results_sfcl1, aes(x = Hour, y = Estimate)) +
  geom_point(size = 3, color = "blue") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "red") + # Plot the 95% CI
  labs(title = "Value with 95% Confidence Intervals",
       x = "Hour",
       y = "Estimate") +
  theme_minimal() +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  xlim(0,23) +
  scale_x_continuous(breaks = 0:23)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 3. DID with Modifiers  ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# NOTE: Additional modifier models can be assessed by modifying the code in each numbered section below
#       When doing so, the control variables must be adjusted to exclude that variable and use it as a modifier. 
#       May be run with hourly or daily data, depending on what is more appropriate for the modifier in question. 
#       Use Model 3 from 06a script as the base:

# m3 <- lm(Temperature ~ treat_25 + post + treat_post  + factor(hour) + 
# temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + 
#   sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + 
#   Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
#   + pct_treec,
# data=did_data_hourly)

##### 1. Sunny vs. Not Sunny ----
# Do the treatment effects vary under fair/sunny conditions on the day of and before?

mod1_m1 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
                temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + 
                sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + 
                Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction,
              data=did_data_hourly[did_data_hourly$sunny==1,])

# DID Model + Time Varying Controls
mod1_m2 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
                temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + 
                sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + 
                Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction,
              data=did_data_hourly[did_data_hourly$sunny==0,])

mod1_models <- list(
  
  # Basic DID Model x Sunny
  mod1_m1 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
                  temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + 
                  sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + 
                  Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
                  pct_treec,
           data=did_data_hourly[did_data_hourly$sunny==1,]),
  # DID Model + Time Varying Controls
  mod1_m2 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
                  temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + 
                  sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + 
                  Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
                  pct_treec,
           data=did_data_hourly[did_data_hourly$sunny==0,])
)

coefci(mod1_m1, vcov = vcovCL(mod1_m1, cluster=did_data_hourly[did_data_hourly$sunny==1,]$street_name))

mod1_m1_se <- coeftest(mod1_m1, vcov = vcovCL(mod1_m1, cluster=did_data_hourly[did_data_hourly$sunny==1,]$street_name))[, "Std. Error"]
mod1_m2_se <- coeftest(mod1_m2, vcov = vcovCL(mod1_m2, cluster=did_data_hourly[did_data_hourly$sunny==0,]$street_name))[, "Std. Error"]

# Table for Models with Clustered Standard Errors
stargazer(mod1_models, type = "text", omit="hour") # check var order

stargazer(
  mod1_models, type = "html", single.row = TRUE, report = "vc*p",
  column.labels = c("Sunny Hours", "Not Sunny Hours", "DID + All Controls"), omit=c("hour"),
  # REMINDER: if you update the models, change the var labels below:
  covariate.labels = c("Treat","Post","Treat*Post", "RDU Temp. (F)", "RDU RH (%)", "RDU Tot. Precip. (in)",
                       "Surface SW Rad. Down", "Surface SW Rad. Down Lag1",
                       "Shaded", "Sidewalk", "Distance to Street (in)", "Street Width (ft)", 
                       "South-Facing", "North-Facing","West-Facing", "Pct Treecover (%)"),
  se = list(mod1_m1_se,mod1_m2_se), out = "Data/Results/06_Modifier_Model_1_Hourly_Sunny.html"
)

# PLOT RESULTS USING LOOP:
# LOOP THROUGH EACH HOUR AND ESTIMATE IMPACT
sunny <- unique(did_data_hourly$sunny)
sunny_results_df <- data.frame()

for (s in sunny) {
  model_name <- paste("sunny",s,sep="")
  model <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) +
                temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + 
                sfc_sw_down_wgt + sfc_sw_down_wgt_l1 +
                Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
                pct_treec,
              data=did_data_hourly[did_data_hourly$sunny==s,]) 
  # hourly_models[[model_name]] <- model
  
  # Extract coefficients & CIs
  coeffs_i <- coef(model)
  ci_i <- coefci(model, vcov = vcovCL(model, cluster=did_data_hourly[did_data_hourly$sunny==s,]$street_name)) # clustered SE
  
  # Combine into a data frame row
  row_data <- data.frame(
    sunny = s,
    Term = names(coeffs_i),
    Estimate = coeffs_i,
    CI_2.5 = ci_i[, 1],
    CI_97.5 = ci_i[, 2]
  )
  
  sunny_results_df <- rbind(sunny_results_df, row_data)
} 

sunny_results <- sunny_results_df %>%
  filter(Term=="treat_post")

sunny_plot <- ggplot(sunny_results, aes(x = as.character(sunny), y = Estimate)) +
  geom_point(size = 3, color = "#004351") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "#166a7c") + # Plot the 95% CI
  scale_x_discrete(labels = c("1"="Sunny", "0"="Not Sunny")) +
  coord_flip() +
  labs(title = "Treat-Post Coefficient with 95% CI",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text = element_text(size=11)) +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  ylim(c(-1,1))


##### 2. High vs. Low Humidity ----
# Note: compare treatment effects during hours in the top quartile and bottom quartile of humidity

humidity <- did_data_hourly %>%
  distinct(date_dt, hour, rh_pct) %>%
  mutate(quantile = ntile(x = rh_pct, n=4)) %>%
  select(date_dt, quantile)
  
did_data_hourly_rh <- did_data_hourly %>%
  left_join(humidity, by="date_dt")

mod2_m1 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) +
                 temp_f_rdu_fill + precip_in_rdu_fill + sunny + 
                 sfc_sw_down_wgt + sfc_sw_down_wgt_l1 +
                 Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
                 pct_treec,
               data=did_data_hourly_rh[did_data_hourly_rh$quantile==1,])

# DID Model + Time Varying Controls
mod2_m2 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) +
                temp_f_rdu_fill + precip_in_rdu_fill + sunny + 
                sfc_sw_down_wgt + sfc_sw_down_wgt_l1 +
                Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
                pct_treec,
              data=did_data_hourly_rh[did_data_hourly_rh$quantile==4,])

mod2_models <- list(
  # Basic DID Model
  mod2_m1 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) +
                  temp_f_rdu_fill + precip_in_rdu_fill + sunny + 
                  sfc_sw_down_wgt + sfc_sw_down_wgt_l1 +
                  Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
                  pct_treec,
                data=did_data_hourly_rh[did_data_hourly_rh$quantile==1,]),
  # DID Model + Time Varying Controls
  mod2_m2 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) +
                  temp_f_rdu_fill + precip_in_rdu_fill + sunny + 
                  sfc_sw_down_wgt + sfc_sw_down_wgt_l1 +
                  Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
                  pct_treec,
                data=did_data_hourly_rh[did_data_hourly_rh$quantile==4,])
)

mod2_m1_se <- coeftest(mod2_m1, vcov = vcovCL(mod2_m1, cluster=did_data_hourly_rh[did_data_hourly_rh$quantile==1,]$street_name))[, "Std. Error"]
mod2_m2_se <- coeftest(mod2_m2, vcov = vcovCL(mod2_m2, cluster=did_data_hourly_rh[did_data_hourly_rh$quantile==4,]$street_name))[, "Std. Error"]

# Table for Models with Clustered Standard Errors
stargazer(mod2_models, type = "text", omit="hour") # check var order

stargazer(
  mod2_models, type = "html", single.row = TRUE, report = "vc*p", omit="hour",
  column.labels = c("Bottom 25% Daily RH %", "Top 25% Daily RH %"), 
  # REMINDER: if you update the models, change the var labels below:
  covariate.labels = c("Treat","Post","Treat*Post", "RDU Temp. (F)", "RDU Tot. Precip. (in)",
                       "Sunny", "Surface SW Rad. Down", "Surface SW Rad. Down Lag1",
                       "Shaded", "Sidewalk", "Distance to Street (in)", "Street Width (ft)",
                       "South-Facing", "North-Facing","West-Facing", "Pct Treecover (%)"),
  se = list(mod2_m1_se,mod2_m2_se), out = "Data/Results/06_Modifier_Model_1_Hourly_Humidity.html"
)

# PLOT RESULTS USING LOOP:
# LOOP THROUGH EACH HOUR AND ESTIMATE IMPACT
rhquant <- c(1,4) # don't use "unique" here bc we only want top and bottom quantiles
rhquant_results_df <- data.frame()

for (q in rhquant) {
  model_name <- paste("rhquant",q,sep="")
  model <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) +
                temp_f_rdu_fill + precip_in_rdu_fill + sunny + 
                sfc_sw_down_wgt + sfc_sw_down_wgt_l1 +
                Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
                pct_treec,
              data=did_data_hourly_rh[did_data_hourly_rh$quantile==q,])
  # hourly_models[[model_name]] <- model
  
  # Extract coefficients & CIs
  coeffs_i <- coef(model)
  ci_i <- coefci(model, vcov = vcovCL(model, cluster=did_data_hourly_rh[did_data_hourly_rh$quantile==q,]$street_name)) # clustered SE
  
  # Combine into a data frame row
  row_data <- data.frame(
    rhquant = q,
    Term = names(coeffs_i),
    Estimate = coeffs_i,
    CI_2.5 = ci_i[, 1],
    CI_97.5 = ci_i[, 2]
  )
  
  rhquant_results_df <- rbind(rhquant_results_df, row_data)
} 

rhquant_results <- rhquant_results_df %>%
  filter(Term=="treat_post")

rhquant_plot <- ggplot(rhquant_results, aes(x = as.character(rhquant), y = Estimate)) +
  geom_point(size = 3, color = "#004351") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "#166a7c") + # Plot the 95% CI
  scale_x_discrete(labels = c("1"="Bottom 25% RH", "4"="Top 25% RH")) +
  coord_flip() +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text = element_text(size=11)) +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  ylim(c(-1,1))

##### 3. SW Radiation + SW Radiation Lag ----
# Note: Do treatment effects vary if downward solar shortwave radiation was above average (198) TODAY and/or YESTERDAY?

mod3_m1 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
                temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + 
                Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec,
              data=did_data_hourly[(did_data_hourly$sfc_sw_down_wgt>=198) & (did_data_hourly$sfc_sw_down_wgt_l1>=198),])

summary(mod3_m1)

mod3_m2 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
                temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + 
                Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction+ pct_treec,
              data=did_data_hourly[(did_data_hourly$sfc_sw_down_wgt>=198) & (did_data_hourly$sfc_sw_down_wgt_l1<198),])

mod3_m3 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
                temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + 
                Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec,
              data=did_data_hourly[(did_data_hourly$sfc_sw_down_wgt<198) & (did_data_hourly$sfc_sw_down_wgt_l1>=198),])

mod3_m4 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
                temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + 
                Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec,
              data=did_data_hourly[(did_data_hourly$sfc_sw_down_wgt<198) & (did_data_hourly$sfc_sw_down_wgt_l1<198),])

mod3_models <- list(
  mod3_m1 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
                  temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + 
                  Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec,
                data=did_data_hourly[(did_data_hourly$sfc_sw_down_wgt>=198) & (did_data_hourly$sfc_sw_down_wgt_l1>=198),]),
  mod3_m2 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
                  temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + 
                  Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec,
                data=did_data_hourly[(did_data_hourly$sfc_sw_down_wgt>=198) & (did_data_hourly$sfc_sw_down_wgt_l1<198),]),
  
  mod3_m3 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
                  temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + 
                  Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec,
                data=did_data_hourly[(did_data_hourly$sfc_sw_down_wgt<198) & (did_data_hourly$sfc_sw_down_wgt_l1>=198),]),
  
  mod3_m4 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
                  temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + 
                  Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec,
                data=did_data_hourly[(did_data_hourly$sfc_sw_down_wgt<198) & (did_data_hourly$sfc_sw_down_wgt_l1<198),])
)

mod3_m1_se <- coeftest(mod3_m1, vcov = vcovCL(mod3_m1, cluster=did_data_hourly[(did_data_hourly$sfc_sw_down_wgt>=198) & (did_data_hourly$sfc_sw_down_wgt_l1>=198),]$street_name))[, "Std. Error"]
mod3_m2_se <- coeftest(mod3_m2, vcov = vcovCL(mod3_m2, cluster=did_data_hourly[(did_data_hourly$sfc_sw_down_wgt>=198) & (did_data_hourly$sfc_sw_down_wgt_l1<198),]$street_name))[, "Std. Error"]
mod3_m3_se <- coeftest(mod3_m3, vcov = vcovCL(mod3_m3, cluster=did_data_hourly[(did_data_hourly$sfc_sw_down_wgt<198) & (did_data_hourly$sfc_sw_down_wgt_l1>=198),]$street_name))[, "Std. Error"]
mod3_m4_se <- coeftest(mod3_m4, vcov = vcovCL(mod3_m4, cluster=did_data_hourly[(did_data_hourly$sfc_sw_down_wgt<198) & (did_data_hourly$sfc_sw_down_wgt_l1<198),]$street_name))[, "Std. Error"]

# Table for Models with Clustered Standard Errors
stargazer(mod3_models, type = "text", omit="hour") # check var order

stargazer(
  mod3_models, type = "html", single.row = TRUE, report = "vc*p",
  column.labels = c("Hi SW, Hi SW Lag 1", "Hi SW, Lo SW Lag 1", "Lo SW, Hi SW Lag 1", "Lo SW, Lo SW Lag 1"), omit=c("hour"),
  # REMINDER: if you update the models, change the var labels below:
  covariate.labels = c("Treat","Post","Treat*Post", "RDU Temp. (F)", "RDU RH (%)", "RDU Tot. Precip. (in)",
                       "RDU Sunny Flag", "Surface SW Rad. Down", "Surface SW Rad. Down Lag1",
                       "Shaded", "Sidewalk", "Distance to Street (in)", "Street Width (ft)", "South-Facing",
                       "North-Facing","West-Facing", "Pct Treecover (%)"),
  se = list(mod3_m1_se,mod3_m2_se,mod3_m3_se,mod3_m4_se), out = "Data/Results/06_Modifier_Model_2_Hourly_SWRadiation.html"
)

# PLOT RESULTS USING LOOP:
# LOOP THROUGH EACH HOUR AND ESTIMATE IMPACT
mod3_data <- did_data_hourly %>%
  mutate(mod3_group = case_when(sfc_sw_down_wgt>=198 & sfc_sw_down_wgt_l1>=198 ~ "Hi SW, Hi SW Lag 1",
                                sfc_sw_down_wgt>=198 & sfc_sw_down_wgt_l1<198 ~ "Hi SW, Lo SW Lag 1",
                                sfc_sw_down_wgt<198 & sfc_sw_down_wgt_l1>=198 ~ "Lo SW, Hi SW Lag 1",
                                sfc_sw_down_wgt<198 & sfc_sw_down_wgt_l1<198 ~ "Lo SW, Lo SW Lag 1")) %>%
  filter(!is.na(mod3_group))
         
radiation_groups <- unique(mod3_data$mod3_group)
rad_results_df <- data.frame()

for (r in radiation_groups) {
  model_name <- paste("mod3_group",r,sep="")
  model <- lm(Temperature ~ treat_25 + post + treat_post + 
                temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny +
                Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
                pct_treec,
              data=mod3_data[mod3_data$mod3_group==r,]) 
  # hourly_models[[model_name]] <- model
  
  # Extract coefficients & CIs
  coeffs_i <- coef(model)
  ci_i <- coefci(model, vcov = vcovCL(model, cluster=mod3_data[mod3_data$mod3_group==r,]$street_name)) # clustered SE
  
  # Combine into a data frame row
  row_data <- data.frame(
    radiation_group = r,
    Term = names(coeffs_i),
    Estimate = coeffs_i,
    CI_2.5 = ci_i[, 1],
    CI_97.5 = ci_i[, 2]
  )
  
  rad_results_df <- rbind(rad_results_df, row_data)
} 

rad_results <- rad_results_df %>%
  filter(Term=="treat_post")

rad_plot <- ggplot(rad_results, aes(x = radiation_group, y = Estimate)) +
  geom_point(size = 3, color = "#004351") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "#166a7c") + # Plot the 95% CI
  # scale_x_discrete(labels = c("1"="Sunny", "0"="Not Sunny")) +
  coord_flip() +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text = element_text(size=11)) +
  theme(axis.text = element_text(size=11)) +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  ylim(c(-1,1))

##### 4. Sunny Hours + Sunny Hours Lag (Try with Daily) ----
# Note: Do treatment effects vary if there were more than 9 hours of sunny conditions TODAY and/or YESTERDAY?
# Alternative/similar to 

mod4_m1 <- lm(Temperature ~ treat_25 + post + treat_post +  
                temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + 
                Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction,
              data=did_data_daily[(did_data_daily$sunny_hours>=9) & (did_data_daily$sunny_hours_l1>=9),])

summary(mod4_m1)

mod4_m2 <- lm(Temperature ~ treat_25 + post + treat_post + 
                temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + 
                Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction,
              data=did_data_daily[(did_data_daily$sunny_hours>=9) & (did_data_daily$sunny_hours_l1<9),])

mod4_m3 <- lm(Temperature ~ treat_25 + post + treat_post + 
                temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + 
                Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction,
              data=did_data_daily[(did_data_daily$sunny_hours<9) & (did_data_daily$sunny_hours_l1>=9),])

mod4_m4 <- lm(Temperature ~ treat_25 + post + treat_post + 
                temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + 
                Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction,
              data=did_data_daily[(did_data_daily$sunny_hours<9) & (did_data_daily$sunny_hours_l1<9),])

mod4_models <- list(
  mod4_m1 <- lm(Temperature ~ treat_25 + post + treat_post + 
                  temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + 
                  Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction,
                data=did_data_daily[(did_data_daily$sunny_hours>=9) & (did_data_daily$sunny_hours_l1>=9),]),
  mod4_m2 <- lm(Temperature ~ treat_25 + post + treat_post + 
                  temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + 
                  Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction,
                data=did_data_daily[(did_data_daily$sunny_hours>=9) & (did_data_daily$sunny_hours_l1<9),]),
  
  mod4_m3 <- lm(Temperature ~ treat_25 + post + treat_post + 
                  temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + 
                  Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction,
                data=did_data_daily[(did_data_daily$sunny_hours<9) & (did_data_daily$sunny_hours_l1>=9),]),
  
  mod4_m4 <- lm(Temperature ~ treat_25 + post + treat_post + 
                  temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + 
                  Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction,
                data=did_data_daily[(did_data_daily$sunny_hours<9) & (did_data_daily$sunny_hours_l1<9),])
)

mod4_m1_se <- coeftest(mod4_m1, vcov = vcovCL(mod4_m1, cluster=did_data_daily[(did_data_daily$sunny_hours>=9) & (did_data_daily$sunny_hours_l1>=9),]$street_name))[, "Std. Error"]
mod4_m2_se <- coeftest(mod4_m2, vcov = vcovCL(mod4_m2, cluster=did_data_daily[(did_data_daily$sunny_hours>=9) & (did_data_daily$sunny_hours_l1<9),]$street_name))[, "Std. Error"]
mod4_m3_se <- coeftest(mod4_m3, vcov = vcovCL(mod4_m3, cluster=did_data_daily[(did_data_daily$sunny_hours<9) & (did_data_daily$sunny_hours_l1>=9),]$street_name))[, "Std. Error"]
mod4_m4_se <- coeftest(mod4_m4, vcov = vcovCL(mod4_m4, cluster=did_data_daily[(did_data_daily$sunny_hours<9) & (did_data_daily$sunny_hours_l1<9),]$street_name))[, "Std. Error"]

# Table for Models with Clustered Standard Errors
stargazer(mod4_models, type = "text", omit="hour") # check var order

stargazer(
  mod4_models, type = "html", single.row = TRUE, report = "vc*p",
  column.labels = c("Hi SW, Hi SW Lag 1", "Hi SW, Lo SW Lag 1", "Lo SW, Hi SW Lag 1", "Lo SW, Lo SW Lag 1"), omit=c("hour"),
  # REMINDER: if you update the models, change the var labels below:
  # covariate.labels = c("Treat","Post","Treat*Post", "RDU Temp. (F)", "RDU RH (%)", "RDU Tot. Precip. (in)",
  #                      "RDU Sunny Flag", "Surface SW Rad. Down", "Surface SW Rad. Down Lag1",
  #                      "Shaded", "Sidewalk", "Distance to Street (in)", "Street Width (ft)", "South-Facing",
  #                      "North-Facing","West-Facing"),
  se = list(mod4_m1_se,mod4_m2_se,mod4_m3_se,mod4_m4_se), out = "Data/Results/06_Modifier_Model_3_Daily_SunnyHours.html")


##### 5. Shaded vs. Unshaded - Hourly ----
# Note: Do treatment effects vary between locations assessed as shady vs those assessed as non-shady? (Shade var collected during installation,  subjective)
# Note: shade vs unshaded is not hard matched in pairs!! Not like previous modifier which were all time-varying and equivalent across all pairs. Also doesn't tell us if the street is shady, only the sensor location.

mod5_m1 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
                temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + 
                sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + sunny +
                Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction,
              data=did_data_hourly[did_data_hourly$Shade=="Yes",])

# DID Model + Time Varying Controls
mod5_m2 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
                temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + 
                sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + sunny +
                Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction,
              data=did_data_hourly[did_data_hourly$Shade=="No",])

mod5_models <- list(
  # Basic DID Model
  mod5_m1 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
                  temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + 
                  sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + sunny +
                  Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
                  pct_treec,
                data=did_data_hourly[did_data_hourly$Shade=="Yes",]),
  # DID Model + Time Varying Controls
  mod5_m2 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) + 
                  temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + 
                  sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + sunny +
                  Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
                  pct_treec,
                data=did_data_hourly[did_data_hourly$Shade=="No",])
)

mod5_m1_se <- coeftest(mod5_m1, vcov = vcovCL(mod5_m1, cluster=did_data_hourly[did_data_hourly$Shade=="Yes",]$street_name))[, "Std. Error"]
mod5_m2_se <- coeftest(mod5_m2, vcov = vcovCL(mod5_m2, cluster=did_data_hourly[did_data_hourly$Shade=="No",]$street_name))[, "Std. Error"]

# Table for Models with Clustered Standard Errors
stargazer(mod5_models, type = "text", omit="hour") # check var order

stargazer(
  mod5_models, type = "html", single.row = TRUE, report = "vc*p",
  column.labels = c("Shaded", "Unshaded", "DID + All Controls"), omit=c("hour"),
  # REMINDER: if you update the models, change the var labels below:
  # covariate.labels = c("Treat","Post","Treat*Post", "RDU Temp. (F)", "RDU RH (%)", "RDU Tot. Precip. (in)",
  #                      "RDU Sunny Flag", "Surface SW Rad. Down", "Surface SW Rad. Down Lag1",
  #                      "Shaded", "Sidewalk", "Distance to Street (in)", "Street Width (ft)", "South-Facing",
  #                      "North-Facing","West-Facing"),
  se = list(mod5_m1_se,mod5_m2_se), out = "Data/Results/06_Modifier_Model_4_Hourly_Shade.html"
)


# PLOT RESULTS USING LOOP:
# LOOP THROUGH EACH HOUR AND ESTIMATE IMPACT
shade <- unique(did_data_hourly$Shade)
shade_results_df <- data.frame()

for (s in shade) {
  model_name <- paste("shade",s,sep="")
  model <- lm(Temperature ~ treat_25 + post + treat_post + 
                temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + 
                sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + sunny +
                Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
                pct_treec,
              data=did_data_hourly[did_data_hourly$Shade==s,]) 
  # hourly_models[[model_name]] <- model
  
  # Extract coefficients & CIs
  coeffs_i <- coef(model)
  ci_i <- coefci(model, vcov = vcovCL(model, cluster=did_data_hourly[did_data_hourly$Shade==s,]$street_name)) # clustered SE
  
  # Combine into a data frame row
  row_data <- data.frame(
    shade = s,
    Term = names(coeffs_i),
    Estimate = coeffs_i,
    CI_2.5 = ci_i[, 1],
    CI_97.5 = ci_i[, 2]
  )
  
  shade_results_df <- rbind(shade_results_df, row_data)
} 

shade_results <- shade_results_df %>%
  filter(Term=="treat_post")

shade_plot <- ggplot(shade_results, aes(x = as.character(shade), y = Estimate)) +
  geom_point(size = 3, color = "#004351") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "#166a7c") + # Plot the 95% CI
  scale_x_discrete(labels = c("Yes"="Shaded", "No"="Not Shaded")) +
  coord_flip() +
  labs(title = "",
       x = "",
       y = "Estimate (Degrees F)") +
  theme_minimal() +
  theme(axis.text = element_text(size=11)) +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  ylim(c(-1,1))


##### 7. Percent Treecover ----
# Note: Do treatment effects vary between sensors on streets with top 25% treecover compared to those with bottom 25% treecover (among sensor locations)
# Note: Similar to #6, this is a time-invariant variable that was not hard-matched between pairs. Must ensure sufficient variation between T and C groups in this var to estimate. 

treecover <- did_data_hourly %>%
  distinct(sensor_id, treat_25, randomize, pct_treec) %>%
  # group_by(date_dt) %>%
  # summarise(rh_pct = mean(rh_pct)) %>%
  # ungroup() %>%
  # mutate(month = month(date_dt)) %>%
  mutate(quantile = ntile(x = pct_treec, n=4)) %>%
  filter(randomize %in% c(8, 17, 27, 30, 101,
                          1, 9, 19, 31, 35)) %>%
  select(sensor_id, randomize, quantile)

did_data_hourly_treec <- did_data_hourly %>%
  left_join(treecover, by=c("sensor_id","randomize"))

mod7_m1 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) +
                temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + 
                sfc_sw_down_wgt + sfc_sw_down_wgt_l1 +
                Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction,
              data=did_data_hourly_treec[did_data_hourly_treec$quantile==1,])

# DID Model + Time Varying Controls
mod7_m2 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) +
                temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + 
                sfc_sw_down_wgt + sfc_sw_down_wgt_l1 +
                Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction,
              data=did_data_hourly_treec[did_data_hourly_treec$quantile==4,])

mod7_models <- list(
  # Basic DID Model
  mod7_m1 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) +
                  temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + 
                  sfc_sw_down_wgt + sfc_sw_down_wgt_l1 +
                  Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
                  pct_treec,
                data=did_data_hourly_treec[did_data_hourly_treec$quantile==1,]),
  # DID Model + Time Varying Controls
  mod7_m2 <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) +
                  temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + 
                  sfc_sw_down_wgt + sfc_sw_down_wgt_l1 +
                  Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction +
                  pct_treec,
                data=did_data_hourly_treec[did_data_hourly_treec$quantile==4,])
)

mod7_m1_se <- coeftest(mod7_m1, vcov = vcovCL(mod7_m1, cluster=did_data_hourly_treec[did_data_hourly_treec$quantile==1,]$street_name))[, "Std. Error"]
mod7_m2_se <- coeftest(mod7_m2, vcov = vcovCL(mod7_m2, cluster=did_data_hourly_treec[did_data_hourly_treec$quantile==4,]$street_name))[, "Std. Error"]

# Table for Models with Clustered Standard Errors
stargazer(mod7_models, type = "text", omit="hour") # check var order

stargazer(
  mod7_models, type = "html", single.row = TRUE, report = "vc*p", omit="hour",
  column.labels = c("Bottom 25% Daily RH %", "Top 25% Daily RH %"), 
  # REMINDER: if you update the models, change the var labels below:
  covariate.labels = c("Treat","Post","Treat*Post", "RDU Temp. (F)", "RDU RH (%)", "RDU Tot. Precip. (in)",
                       "Sunny", "Surface SW Rad. Down", "Surface SW Rad. Down Lag1",
                       "Shaded", "Sidewalk", "Distance to Street (in)", "Street Width (ft)",
                       "South-Facing", "North-Facing","West-Facing"),
  se = list(mod7_m1_se,mod7_m2_se), out = "Data/Results/06_Modifier_Model_7_Hourly_Treecover.html"
)

# PLOT RESULTS USING LOOP:
# LOOP THROUGH EACH HOUR AND ESTIMATE IMPACT
treecquant <- c(1,4) # don't use unique bc we only want top and bottom quantiles
treecquant_results_df <- data.frame()

for (q in treecquant) {
  model_name <- paste("treecquant",q,sep="")
  model <- lm(Temperature ~ treat_25 + post + treat_post + factor(hour) +
                temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + 
                sfc_sw_down_wgt + sfc_sw_down_wgt_l1 +
                Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction,
              data=did_data_hourly_treec[did_data_hourly_treec$quantile==q,])
  # hourly_models[[model_name]] <- model
  
  # Extract coefficients & CIs
  coeffs_i <- coef(model)
  ci_i <- coefci(model, vcov = vcovCL(model, cluster=did_data_hourly_treec[did_data_hourly_treec$quantile==q,]$street_name)) # clustered SE
  
  # Combine into a data frame row
  row_data <- data.frame(
    treecquant = q,
    Term = names(coeffs_i),
    Estimate = coeffs_i,
    CI_2.5 = ci_i[, 1],
    CI_97.5 = ci_i[, 2]
  )
  
  treecquant_results_df <- rbind(treecquant_results_df, row_data)
} 

treecquant_results <- treecquant_results_df %>%
  filter(Term=="treat_post")

treecquant_plot <- ggplot(treecquant_results, aes(x = as.character(treecquant), y = Estimate)) +
  geom_point(size = 3, color = "#004351") + # Plot the central value
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.2, color = "#166a7c") + # Plot the 95% CI
  scale_x_discrete(labels = c("1"="Bottom 25% Treecover", "4"="Top 25% Treecover")) +
  coord_flip() +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text = element_text(size=11)) +
  geom_hline(yintercept = 0, # Convert datetime to numeric
             color = "black",
             linetype = "dashed",
             size = 1) +
  ylim(c(-1,1))

##### PLOT COMBINE MODIFIER MODELS ----
# May include any modifier model results from above

ggarrange(sunny_plot, rhquant_plot, rad_plot, treecquant_plot, nrow=4, ncol=1, align="v")
ggsave("Data/Analysis/Figures/Modifier_models.png", dpi=300, height=10, width=8)
