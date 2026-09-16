################################################################################
# Program Name: 05_Create_Analysis_Dataset.R
# Program Purpose: create analysis datasets for cool pavement models
# Author: Katherine Burley Farr
# Contact: kburley@ad.unc.edu
# Affiliation: UNC Department of Public Policy, Data-Driven EnviroLab
################################################################################

#Run at start: setwd("/Users/siddhantchoudhary/Downloads/raleigh-cool-pavements-main/Code")

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
library(weathermetrics)
}

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 1. Sensor Data ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls())

# Filepath - DDL GDrive where the clean data files live
# NOTE: Clean Data is using Eastern Daylight Time! This makes more sense because daylight time is active during the summer
clean_csvs <- list.files("/Users/siddhantchoudhary/Downloads/raleigh-cool-pavements-main/Data/2025 Sensor Data Collection/CleanData", 
                         pattern="^Sensor_Data_Week.*\\.csv$",
                         full.name = T, recursive=T)

# clean_csvs <- clean_csvs[-1] 

combined_data_orig <- clean_csvs %>%
  map_dfr(~ read.csv(.x) %>% 
            mutate(filename = basename(.x))) %>%
  distinct(sensor_id, datetime, Temperature, Relative.Humidity) 

combined_data <- combined_data_orig %>%
  group_by(sensor_id, datetime) %>%
  summarise(Temperature = mean(Temperature),
            Relative.Humidity = mean(Relative.Humidity)) %>% # handles a few dups
  ungroup() %>%
  mutate(hour = as.numeric(hour(datetime)),
         date_dt = date(datetime)) %>%
  mutate(week = week(date_dt))
  
rm(combined_data_orig)

# Clean Street Characteristics Data to Merge 
street_chars <- read_csv("../Data/Analysis/03_Sensor_Locations_Characteristics_All.csv") %>%
  rename(sensor_id = SensorID) %>%
  mutate(sensor_id = as.character(sensor_id)) %>%
  mutate(randomize = case_when(GMaps_ID == "ASHBURTON_KAPLAN DR_WICKHAM RD" ~ 10,
                               TRUE ~ randomize)) # this one was at the intersection, but its actually Ashburton-Wickham-Newcastle matched to Oberlin

street_chars_filt <- street_chars %>% 
  select(sensor_id, GMaps_ID, randomize, treat_25, Shade, Sidewalk, St2Pole_in, SensDirect, COR_WIDTH, yr_repave,
         UHI_Qtile2, pct_treec) %>%
  # 2025 treatment dates identified via "2025 Rejuvenation Progress" excel file shared by the city
  # 44 sensors treated, 18 not yet treated, pushed to 2026
  mutate(treatment_date = case_when(randomize %in% c(2,3,19,35,36) ~ ymd("2025-09-12",tz="Etc/GMT+4"),
                                    randomize %in% c(1, 10) ~ ymd("2025-09-25",tz="Etc/GMT+4"),
                                    randomize %in% c(28,15,17,21) ~ ymd("2025-10-07",tz="Etc/GMT+4"),
                                    randomize %in% c(27,101) ~ ymd("2025-10-09",tz="Etc/GMT+4"),
                                    randomize %in% c(23) ~ ymd("2025-10-10",tz="Etc/GMT+4"),
                                    randomize %in% c(8,20,25,30) ~ ymd("2025-10-18",tz="Etc/GMT+4"),
                                    randomize %in% c(29) ~ ymd("2025-10-21",tz="Etc/GMT+4"),
                                    randomize %in% c(4) ~ ymd("2025-10-22",tz="Etc/GMT+4"),
                                    randomize %in% c(31, 9) ~ ymd("2025-10-23",tz="Etc/GMT+4"))) %>%
  mutate(treatment_week = week(treatment_date)) %>%
  # Treatment "waves" assigned based on treatments happening in the same week
  mutate(treatment_wave = case_when(randomize %in% c(2,3,19,35,36) ~ 1,
                                    randomize %in% c(1, 10) ~ 2,
                                    randomize %in% c(28,15,17,21,27,101,23) ~ 3,
                                    randomize %in% c(8,20,25,30) ~ 4,
                                    randomize %in% c(29,4,31,9) ~ 5)) %>%
  filter(!is.na(treatment_date)) %>%
  separate_wider_delim(cols=SensDirect, names=c("degrees", "direction"), delim=" ", cols_remove = F) %>%
  # Based on sensor orientation, assign to the closest cardinal direction
  mutate(sensor_direction = case_when(direction %in% c("N","E","S","W") ~ direction,
                                      direction == "NE" & degrees <= 45 ~ "N",
                                      direction == "NE" & degrees > 45 ~ "E",
                                      direction == "SE" & degrees > 135 ~ "S", # none <=135
                                      direction == "SW" & degrees <=225 ~ "S",
                                      direction == "SW" & degrees > 225 ~ "W",
                                      direction == "NW" & degrees <= 315 ~ "W")) %>% # none > 315
  # Get street for clustering
  separate_wider_delim(cols=GMaps_ID, names=c("street_name","Cross1","Cross2"), delim="_", cols_remove=F) %>%
  select(-c(SensDirect, degrees, direction, Cross1, Cross2))

# Get points for LST data extraction
sensor_points <- st_read("../Data/Analysis/03_Sensor_Locations_Characteristics_All.shp") %>%
  filter(SensorID %in% c(street_chars_filt$sensor_id)) %>%
  rename(sensor_id = SensorID) %>%
  select(sensor_id, lat_y, lon_x, geometry)
  
# st_write(sensor_points, "LST_Analysis/Data/Orig/Sensor_Points.shp")

sensor_data <- combined_data %>%
  filter(sensor_id %in% unique(street_chars_filt$sensor_id)) %>%
  mutate(sensor_id = as.character(sensor_id)) %>%
  left_join(street_chars_filt, by="sensor_id") %>%
  mutate(datetime_orig = datetime) %>%
  mutate(datetime = as_datetime(datetime_orig, tz="Etc/GMT+4")) # eastern daylight time (EDT)
  
# FLAG observations where data is missing on at least one sensor in a pair
# if both T & C sensors are missing for a datetime observation, their data is not included here
flag_missing_data <- sensor_data %>%
  mutate(missing_type = case_when(treat_25 == 1 ~ "treat_data",
                                  treat_25 == 0 ~ "control_data")) %>%
  distinct(sensor_id, randomize, datetime, Temperature, missing_type) %>%
  pivot_wider(id_cols=c("randomize","datetime"), names_from = "missing_type", values_from="Temperature") %>%
  mutate(pair_data_missing = case_when(is.na(treat_data) | is.na(control_data) ~ 1,
                                       TRUE ~ 0)) %>%
  select(randomize, datetime, pair_data_missing)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2. Additional Control Vars ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# CERES Solar Radiation Data 
# Source: https://ceres-tool.larc.nasa.gov/ord-tool/jsp/FLASH_TISASelection.jsp
# Average of the two lat/lon locations from the CERES data - roughly in the Raleigh area
ceres <- read_csv("../Data/Original/Solar_Radiation/CERES_FLASH_TISA_Version1A_Subset_20250701-20251205.csv") %>%
  select(time, lon, lat, sfc_sw_down_all_daily) %>%
  mutate(weight = case_when(lat == 35.5 ~ 0.7,
                            lat == 36.5 ~ 0.3)) %>%
  mutate(sfc_sw_down_all_daily_wgt = sfc_sw_down_all_daily*weight) %>%
  group_by(time, lon) %>%
  summarise(sfc_sw_down_wgt_orig = sum(sfc_sw_down_all_daily_wgt),
            sfc_sw_down_mean_orig = mean(sfc_sw_down_all_daily)) %>%
  ungroup() %>%
  group_by(lon) %>%
  mutate(prev_obs_wgt = dplyr::lag(sfc_sw_down_wgt_orig, n=1),
         next_obs_wgt = dplyr::lead(sfc_sw_down_wgt_orig, n=1),
         prev_obs_mean = dplyr::lag(sfc_sw_down_mean_orig, n=1),
         next_obs_mean = dplyr::lead(sfc_sw_down_mean_orig, n=1)) %>%
  ungroup() %>%
  mutate(sfc_sw_down_wgt = case_when(!is.na(sfc_sw_down_wgt_orig) ~ sfc_sw_down_wgt_orig,
                                               is.na(sfc_sw_down_wgt_orig) ~ (prev_obs_wgt + next_obs_wgt)/2),
         sfc_sw_down_mean = case_when(!is.na(sfc_sw_down_mean_orig) ~ sfc_sw_down_mean_orig,
                                                is.na(sfc_sw_down_mean_orig) ~ (prev_obs_wgt + next_obs_mean)/2)) %>%
  select(time, sfc_sw_down_wgt, sfc_sw_down_mean) %>%
  rename(date_dt = time)

# Hourly Weather Conditions at RDU Airport (KRDU) (NCSU Cardinal)
# Source: https://products.climate.ncsu.edu/cardinal/scout/
# "Times are in Local Standard Time (LST) unless otherwise noted"
rdu_hourly_ncsu <- read_xlsx("../Data/Original/Weather/ZD7DM7W7_1.xlsx", skip=11) %>%
  rename(datetime_str = `Date/Time (Eastern)`,
         temp_f_rdu_fill_rdu = `Top-of-the-Hour Air Temperature (F)`,
         dewpoint_f_rdu = `Top-of-the-Hour Dew Point Temperature (F)`,
         precip_in_rdu = `Total Precipitation (in)`,
         cloud_coverage = `Cloud Coverage & Height`) %>%
  select(-`Present Weather`) %>%
  # Create datetime variables
  mutate(datetime_est = as.POSIXct(datetime_str, tz = "Etc/GMT+5")) %>%
  mutate(datetime = with_tz(datetime_est, tz="Etc/GMT+4")) %>% # get main datetime var in EDT
  # initial cleaning
  mutate(temp_f_rdu_fill_rdu = as.numeric(temp_f_rdu_fill_rdu),
         dewpoint_f_rdu = as.numeric(dewpoint_f_rdu),
         precip_in_rdu = as.numeric(precip_in_rdu)) %>%
  mutate(across(c("cloud_coverage"), ~na_if(., "MV"))) %>%
  mutate(cloud_type = substr(cloud_coverage, start = 1, stop = 3)) %>%
  # Impute missing values
  mutate(lag_temp = dplyr::lag(temp_f_rdu_fill_rdu, n=1),
         lead_temp = dplyr::lead(temp_f_rdu_fill_rdu, n=1),
         lag_dp = dplyr::lag(dewpoint_f_rdu, n=1),
         lead_dp = dplyr::lead(dewpoint_f_rdu, n=1),
         lag_precip = dplyr::lag(precip_in_rdu, n=1),
         lead_precip = dplyr::lead(precip_in_rdu, n=1)) %>%
  mutate(temp_f_rdu_fill = case_when(!is.na(temp_f_rdu_fill_rdu) ~ temp_f_rdu_fill_rdu,
                                     is.na(temp_f_rdu_fill_rdu) ~ (lag_temp + lead_temp)/2),
         dewpoint_f_rdu_fill = case_when(!is.na(dewpoint_f_rdu) ~ dewpoint_f_rdu,
                                      is.na(dewpoint_f_rdu) ~ (lag_dp + lead_dp)/2),
         precip_in_rdu_fill = case_when(!is.na(precip_in_rdu) ~ precip_in_rdu,
                                        is.na(precip_in_rdu) ~ (lag_precip + lag_precip)/2)) %>%
  # If still missing (bc of consective missing) OR str var, fill down
  tidyr::fill(temp_f_rdu_fill,dewpoint_f_rdu_fill,precip_in_rdu_fill,cloud_type, .direction="down") %>%
  select(-c(lag_temp,lead_temp,lag_dp,lead_dp,lag_precip,lead_precip)) %>%
  mutate(sunny = case_when(cloud_type %in% c("CLR","FEW","SCT") ~ 1, # less than 50% cloud coverage
                           cloud_type %in% c("BKN","OVC") ~ 0)) %>% # over 50% cloud coverage
  mutate(date_dt = date(datetime))

# Hourly Solar Radiation from Lake Wheeler Rd Field Lab (LAKE) Station in Raleigh (NCSU Cardinal)
# Source: https://products.climate.ncsu.edu/cardinal/scout/
hourly_rad_ncsu <- read_xlsx("../Data/Original/Weather/WF2FH3Q2_1.xlsx", skip=11) %>%
  rename(toh_rad_wm2_orig = `Top-of-the-Hour Solar Radiation (W/m2)`,
         avg_rad_wm2_orig = `Average Solar Radiation (W/m2)`) %>%
  mutate(toh_rad_wm2_fill_up = case_when(toh_rad_wm2_orig %in% c("QCF","MV") ~ NA,
                                       TRUE ~ as.numeric(toh_rad_wm2_orig)),
         avg_rad_wm2_fill_up = case_when(avg_rad_wm2_orig %in% c("QCF","MV") ~ NA,
                                       TRUE ~ as.numeric(avg_rad_wm2_orig))) %>%
  # interpolate missing with avg of previous and next observations
  mutate(toh_rad_wm2_fill_down = toh_rad_wm2_fill_up,
         avg_rad_wm2_fill_down = avg_rad_wm2_fill_up) %>%
  tidyr::fill(toh_rad_wm2_fill_up, avg_rad_wm2_fill_up, .direction = "up") %>%
  tidyr::fill(toh_rad_wm2_fill_down, avg_rad_wm2_fill_down, .direction = "down") %>%
  mutate(toh_rad_wm2_fill = (toh_rad_wm2_fill_up + toh_rad_wm2_fill_down)/2,
         avg_rad_wm2_fill = (avg_rad_wm2_fill_up + avg_rad_wm2_fill_down)/2) %>%
  mutate(datetime_est = as.POSIXct(`Date/Time (Eastern)`, tz = "Etc/GMT+5")) %>%
  mutate(datetime = with_tz(datetime_est, tz="Etc/GMT+4")) %>%
  select(datetime, toh_rad_wm2_fill, avg_rad_wm2_fill)
  

# Daily Sunrise/Sunset Data for RDU 
# Source: https://aa.usno.navy.mil/data/RS_OneYear, manually converted to Excel
# In EST - convert to EDT! 
daily_sunlight <- read_xlsx("../Data/Original/Solar_Radiation/Daily_Sunrise_Sunset.xlsx", col_types="text") %>%
  mutate(rise_h = str_sub(Rise, end = -3),
         rise_m = str_sub(Rise, start = -2, end = -1),
         set_h = str_sub(Set, end = -3),
         set_m = str_sub(Set, start = -2, end = -1)) %>%
  mutate(date_dt = make_date(year=2025, month=as.numeric(Month), day=as.numeric(Day)),
         dt_rise_est = make_datetime(year = 2025, month=as.numeric(Month), day=as.numeric(Day),
                                 hour=as.numeric(rise_h), min=as.numeric(rise_m), sec=0, tz = "Etc/GMT+5"),
         dt_set_est = make_datetime(year = 2025, month=as.numeric(Month), day=as.numeric(Day),
                                hour=as.numeric(set_h), min=as.numeric(set_m), sec=0, tz = "Etc/GMT+5")) %>%
  # Convert sunset and sunrise datetimes to EDT to align with sensor data
  mutate(dt_rise = with_tz(dt_rise_est, tz="Etc/GMT+4"),
         dt_set = with_tz(dt_set_est, tz="Etc/GMT+4")) %>%
  mutate(daylight_mins = as.numeric(difftime(dt_set, dt_rise, units = "mins")),
         daylight_mins_check = as.numeric(difftime(dt_set_est, dt_rise_est, units = "mins"))) %>% # OK
  select(date_dt, dt_rise, dt_set, daylight_mins)

# Combine the weather/sunlight control data:
rdu_weather_hourly <- rdu_hourly_ncsu %>% 
  left_join(hourly_rad_ncsu, by="datetime") %>%
  left_join(daily_sunlight, by="date_dt") %>%
  # only include full hours of daylight - more conservative
  mutate(daytime_hour = case_when(hour(datetime) > hour(dt_rise) & hour(datetime) < hour(dt_set) ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(hour = hour(datetime)) %>%
  select(-c(datetime, datetime_str, datetime_est, temp_f_rdu_fill_rdu, dewpoint_f_rdu, precip_in_rdu, cloud_coverage)) %>%
  mutate(rh_pct = dewpoint.to.humidity(t = temp_f_rdu_fill, dp = dewpoint_f_rdu_fill, temperature.metric = "fahrenheit"))
  # only using hour because this is hourly data merging to 20min sensor data that also has datetime
  
rdu_solar_rad_daily <- rdu_weather_hourly %>%
  filter(daytime_hour==1) %>%
  group_by(date_dt, daylight_mins) %>%
  summarise(sunny_hours = sum(sunny)) %>%
  ungroup() %>%
  left_join(ceres, by="date_dt") %>%
  # Create lagged values!
  mutate(daylight_mins_l1 = lag(daylight_mins, n=1),
         sunny_hours_l1 = lag(sunny_hours, n=1),
         sfc_sw_down_wgt_l1 = lag(sfc_sw_down_wgt, n=1),
         sfc_sw_down_mean_l1 = lag(sfc_sw_down_mean, n=1)) 

rm(ceres, daily_sunlight, rdu_hourly_ncsu)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 3. Create 20-min, hourly, and daily DID datasets ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Full data - 20 min intervals
did_data <- sensor_data %>%
  left_join(rdu_weather_hourly, by=c("date_dt","hour")) %>%
  left_join(rdu_solar_rad_daily, by="date_dt") %>%
  left_join(flag_missing_data, by=c("datetime", "randomize")) %>%
  select(-daylight_mins.x) %>%
  rename(daylight_mins = daylight_mins.y) %>%
  # Create treatpost indicator
  mutate(post = case_when(datetime < make_datetime(year = year(treatment_date),
                                                   month = month(treatment_date),
                                                   day = day(treatment_date),
                                                   hour = 0,
                                                   min = 0,
                                                   sec = 0,
                                                   tz = "Etc/GMT+4") ~ 0,
                          datetime >= make_datetime(year = year(treatment_date),
                                                    month = month(treatment_date),
                                                    day = day(treatment_date)+2, # exclude day of treatment and day after
                                                    hour = 0,
                                                    min = 0,
                                                    sec = 0,
                                                    tz = "Etc/GMT+4") ~ 1,
                          TRUE ~ NA)) %>%
  filter(!is.na(post)) %>%
  mutate(treat_post = treat_25*post) %>%
  # Remove first and last weeks 
  filter(week>=26 & week<=46) %>%
  # DROP observations from sensor where its match was missing observations
  # This will ensure that the hourly and daily summaries are equivalent across pairs
  filter(pair_data_missing == 0) %>%
  mutate(date = as.Date(date_dt),
         treatment_date = as.Date(treatment_date)) %>%
  mutate(time_unit = week - treatment_week) 

write_csv(did_data, "../Data/Analysis/05_Analysis_Input.csv") # Too large for GitHub, stored on DDL Gdrive

# Hourly 
did_data_hourly <- did_data %>%
  # remove datetime vars - 20 min
  group_by(sensor_id, hour, date_dt, week, street_name, GMaps_ID, randomize, treat_25,
           Shade, Sidewalk, St2Pole_in, COR_WIDTH, yr_repave, treatment_date,
           treatment_week, treatment_wave, sensor_direction, UHI_Qtile2,
           pct_treec, cloud_type, sunny, daylight_mins, toh_rad_wm2_fill,
           avg_rad_wm2_fill, sunny_hours, sfc_sw_down_wgt, sfc_sw_down_mean, daylight_mins_l1,
           sunny_hours_l1, sfc_sw_down_wgt_l1, sfc_sw_down_mean_l1,
           post, treat_post, date, time_unit) %>%
  # dt_rise, dt_set, rise_h, set_h, - removed bc causing problems in the summarise + are not used
  # Take mean or sum to get hourly values
  summarise(Temperature = mean(Temperature, na.rm=T),
            Relative.Humidity = mean(Relative.Humidity, na.rm=TRUE),
            temp_f_rdu_fill = mean(temp_f_rdu_fill, na.rm=T),
            dewpoint_f_rdu_fill = mean(dewpoint_f_rdu_fill, na.rm=T),
            rh_pct = mean(rh_pct, na.rm=T),
            precip_in_rdu_fill = sum(precip_in_rdu_fill, na.rm=T)) %>%
  ungroup() %>%
  # Handle the NaN values introduced in the summarise...
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))

write_csv(did_data_hourly, "../Data/Analysis/05_Analysis_Data_Hourly.csv")


# Daily
did_data_daily <- did_data %>%
  # remove hourly indicators
  group_by(sensor_id, date_dt, week, street_name, GMaps_ID, randomize, treat_25,
           Shade, Sidewalk, St2Pole_in, COR_WIDTH, yr_repave, UHI_Qtile2,
           pct_treec, treatment_date, treatment_week, treatment_wave, sensor_direction, 
           daylight_mins, sunny_hours, sfc_sw_down_wgt, sfc_sw_down_mean, 
           daylight_mins_l1, sunny_hours_l1, sfc_sw_down_wgt_l1, sfc_sw_down_mean_l1,
           post, treat_post, date, time_unit) %>%  
  # dt_rise, dt_set, rise_h, set_h, - removed bc causing problems in the summarise + are not used
  # Take mean or sum to get daily values
  summarise(Temperature = mean(Temperature, na.rm=TRUE),
            Relative.Humidity = mean(Relative.Humidity, na.rm=TRUE),
            temp_f_rdu_fill = mean(temp_f_rdu_fill, na.rm=T),
            dewpoint_f_rdu_fill = mean(dewpoint_f_rdu_fill, na.rm=T),
            rh_pct = mean(rh_pct, na.rm=T),
            precip_in_rdu_fill = sum(precip_in_rdu_fill, na.rm=T),
            # these are prob not useful, but want to compare to sfc vars
            avg_rad_wm2_fill = sum(avg_rad_wm2_fill, na.rm=T),
            toh_rad_wm2_fill = sum(toh_rad_wm2_fill, na.rm=T)) %>%
  ungroup() %>%
  # Handle the NaN values introduced in the summarise...
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))

write_csv(did_data_daily, "../Data/Analysis/05_Analysis_Data_Daily.csv")

  
