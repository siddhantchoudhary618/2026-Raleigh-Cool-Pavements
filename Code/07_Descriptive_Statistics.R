################################################################################
# Program Name: 07_Descriptive_Statistics.R
# Program Purpose: Compare treatment streets to eligible vs all streets
# Author: Katherine Burley Farr
# Contact: kburley@ad.unc.edu
# Affiliation: UNC Department of Public Policy, Data-Driven EnviroLab
################################################################################

rm(list=ls())

library(tidyverse)
library(pdftools)
library(stringr)
library(sf)
library(readxl)
library(writexl)
library(lubridate)
library(gtsummary)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 1. Bring in data  ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Street Data ----

streets_orig <- st_read("../Data/Analysis/01_Streets_for_Matching.shp") %>%
  mutate(treat_2020 = case_when(prev_treat==1 & treat_year=="2020" ~ 1,
                                TRUE ~ 0),
         treat_2022 = case_when(prev_treat==1 & treat_year %in% c("2022", "2022/2024") ~ 1,
                                TRUE ~ 0),
         treat_2024 = case_when(prev_treat==1 & treat_year %in% c("2024", "2022/2024") ~ 1,
                                TRUE ~ 0),
         treat_2025 = case_when(treat_25==1 ~ 1,
                                TRUE ~ 0)) %>%
  mutate(eligible_2020 = case_when(yr_repave == 2017 & treat_2020 == 0 ~ 1,
                                   TRUE ~ 0),
         eligible_2022 = case_when(yr_repave == 2019 & treat_2022 == 0 ~ 1,
                                   TRUE ~ 0),
         eligible_2024 = case_when(yr_repave %in% c(2018, 2021) & treat_2024 == 0 ~ 1,
                                   TRUE ~ 0),
         eligible_2025 = case_when(yr_repave %in% c(2019, 2022) & treat_2025 == 0 ~ 1,
                                   TRUE ~ 0)) %>%
  mutate(treated = case_when(treat_2020 == 1 | treat_2022==1 | treat_2024==1 | treat_2025 == 1 ~ 1,
                             TRUE ~ 0),
         untreated = case_when(treat_2020==0 & treat_2022==0 & treat_2024==0 & treat_2025==0 ~ 1,
                               TRUE ~ 0),
         eligible = case_when(eligible_2020 == 1 | eligible_2022==1 | eligible_2024==1 | eligible_2025 == 1 ~ 1,
                              TRUE ~ 0)) %>%
  mutate(all_others = case_when(treat_2020==0 & treat_2022==0 & treat_2024==0 & treat_2025==0 & 
                                  eligible_2020==0 & eligible_2022==0 & eligible_2024==0 & eligible_2025==0 ~ 1,
                                TRUE ~ 0))

# Average street segment area?
streets_area <- streets_orig %>%
  select(GlobalID, COR_WIDTH, COR_LENGTH) %>%
  mutate(COR_WIDTH_m = COR_WIDTH/3.281,
         COR_LENGTH_m = COR_LENGTH/3.281) %>%
  mutate(segment_area_m = COR_WIDTH_m*COR_LENGTH_m)

summary(streets_area$segment_area_m)

# Total length in each treatment year?
streets_length <- streets_orig %>%
  st_drop_geometry() %>%
  mutate(earliest_treat_year = case_when(treat_2025 == 1 ~ "2025",
                                         treat_2024 == 1 ~ "2024",
                                         treat_2022 == 1 ~ "2022",
                                         treat_2020 == 1 ~ "2020",
                                         TRUE ~ "Untreated")) %>%
  group_by(earliest_treat_year) %>%
  summarise(tot_length = sum(COR_LENGTH)) %>%
  ungroup()

# Export Version for Extracting Pre-Period Annual Avg. LST
streets_limited_vars <- streets_orig %>%
  select(GlobalID, treat_25, prev_treat, yr_repave, pct_treec)

# st_write(streets_limited_vars, "../Data/Analysis/07_All_Streets_Limited_Vars.shp")

treat_by_repave_yr <- streets_orig %>%
  st_drop_geometry() %>%
  select(GlobalID, yr_repave, treat_2020, treat_2022, treat_2024, treat_2025, untreated, pct_treec) %>%
  pivot_longer(cols=c(treat_2020, treat_2022, treat_2024, treat_2025, untreated), names_to = "treat_year", values_to = "n") %>%
  group_by(yr_repave, treat_year) %>%
  summarise(tot_treat = sum(n)) %>%
  pivot_wider(names_from = treat_year, values_from = tot_treat)

#### Pre-Treatment LST Data ----

lst_2019 <- read_csv("G:/My Drive/Cool_Pavements_LST/Annual_LST_2019_All_Streets.csv") %>%
  select(GlobalID, mean) %>%
  rename(median_lst_2019 = mean)

lst_2021 <- read_csv("G:/My Drive/Cool_Pavements_LST/Annual_LST_2021_All_Streets.csv") %>%
  select(GlobalID, mean) %>%
  rename(median_lst_2021 = mean)

lst_2023 <- read_csv("G:/My Drive/Cool_Pavements_LST/Annual_LST_2023_All_Streets.csv") %>%
  select(GlobalID, mean) %>%
  rename(median_lst_2023 = mean)

lst_2024 <- read_csv("G:/My Drive/Cool_Pavements_LST/Annual_LST_2024_All_Streets.csv") %>%
  select(GlobalID, mean) %>%
  rename(median_lst_2024 = mean)

pre_treat_lst <- lst_2019 %>%
  left_join(lst_2021, by="GlobalID") %>%
  left_join(lst_2023, by="GlobalID") %>%
  left_join(lst_2024, by="GlobalID") 

rm(lst_2019, lst_2021, lst_2023, lst_2024) 

#### Census Data ----

census2020 <- read_csv("../Data/Orig/Census/nhgis0033_csv/nhgis0033_csv/nhgis0033_ds258_2020_tract.csv") %>%
  rename(total_population = U7H001) %>%
  # Ethnicity
  mutate(pct_hispanic = U7N009/total_population) %>%
  # Race
  mutate(pct_white = (U7N003 + U7N010)/total_population,
         pct_black = (U7N004 + U7N011)/total_population,
         pct_native = (U7N005 + U7N012)/total_population,
         pct_asian = (U7N006 + U7N013)/total_population,
         pct_pacific = (U7N007 + U7N014)/total_population,
         pct_other = (U7N008+U7N015)/total_population) %>%
  # Age Groups, under 5, 5-18, 65+
  mutate(pct_under5 = (U7S003 + U7S027)/total_population,
         pct_5to17 = (U7S004 + U7S005 + U7S006 + U7S028 + U7S029 + U7S030)/total_population,
         pct_65pl = (U7S020 + U7S021 + U7S022 + U7S023 + U7S024 + U7S025 +
                       U7S044 + U7S045 + U7S046 + U7S047 + U7S048 + U7S049)/total_population) %>%
  # Renter Occupied Housing Units
  rename(tot_occ_housing_units = U9Y001) %>%
  mutate(pct_renter_occ = U9Y004/tot_occ_housing_units) %>%
  select(GISJOIN, GEOID, total_population, pct_hispanic, pct_white, pct_black, pct_native,
         pct_asian, pct_pacific, pct_other, pct_under5, pct_5to17, pct_65pl, pct_renter_occ) %>%
  filter(total_population>10)

acs_data <- read_csv("../Data/Orig/Census/nhgis0033_csv/nhgis0033_csv/nhgis0033_ds267_20235_tract.csv") %>%
  # Education - ASP3E001 is total pop 25+
  mutate(pct_with_bach_pl = (ASP3E022+ASP3E023+ASP3E024+ASP3E025)/ASP3E001,
         pct_with_doc = ASP3E025/ASP3E001) %>%
  # Poverty
  mutate(pct_below_pov = ASQNE002/ASQNE001) %>%
  # Median Income and Housing Year Built
  rename(median_income = ASQPE001,
         median_yr_built = ASUKE001) %>%
  select(GISJOIN, pct_with_bach_pl, pct_with_doc, pct_below_pov, median_income, median_yr_built)

ct_demographics <- census2020 %>%
  left_join(acs_data, by="GISJOIN") %>%
  rename(GEOID_orig = GEOID) %>%
  separate(GEOID_orig, into=c("Extra","GEOID"), sep="US", remove=FALSE) %>%
  select(-c(GISJOIN, GEOID_orig, Extra))

# COMBINE info
streets_to_censustracts <- read_csv("../Data/Orig/Census/Streets_To_CensusTracts.csv") %>%
  select(GlobalID, GEOID) %>%
  mutate(GEOID = as.character(GEOID))

streets_desc_stats <- streets_orig %>%
  select(GlobalID, treated, eligible, treat_2020, treat_2022, treat_2024, treat_2025,
         eligible_2020, eligible_2022, eligible_2024, eligible_2025,
         untreated, all_others, pct_treec) %>%
  # Bring in Pre-Treatment LST
  left_join(pre_treat_lst, by="GlobalID") %>%
  # Bring in Census Demographics
  left_join(streets_to_censustracts, by="GlobalID") %>%
  left_join(ct_demographics, by="GEOID") %>%
  st_drop_geometry() # %>%
  # mutate(group = as.factor(case_when(all_others == 1 ~ "All Others",
  #                          treat_2020 == 1 ~ "Treat 2020",
  #                          eligible_2020 == 1 ~ "Eligible 2020",
  #                          treat_2022 == 1 ~ "Treat 2022",
  #                          eligible_2022 == 1 ~ "Eligible 2022",
  #                          treat_2024 == 1 ~ "Treat 2024",
  #                          eligible_2024 == 1 ~ "Eligible 2024",
  #                          treat_2025 == 1 ~ "Treat 2025",
  #                          eligible_2025 == 1 ~ "Eligible 2025",
  #                          TRUE ~ NA))) %>%
  # # Will actually have to do this separately because some groups overlap
  # mutate(group_fct = factor(group, levels = c("All Others","Eligible 2020",
  #                                            "Treat 2020", "Eligible 2022",
  #                                            "Treat 2022", "Eligible 2024",
  #                                            "Treat 2024", "Eligible 2025",
  #                                            "Treat 2025"))) %>%
  # mutate(across(starts_with("pct"), ~round(.x*100, digits=2)))

#### Estimate Individual Tables ----

# Comparison Tables
streets_all_others <- streets_desc_stats %>%
  filter(all_others==1)

table.all_others <- 
  tbl_summary(
    streets_all_others,
    include = c(median_lst_2019, median_lst_2021, median_lst_2023, median_lst_2024,
                pct_treec, pct_white, pct_black, pct_native, pct_asian, 
                pct_other, pct_hispanic, pct_under5, pct_5to17, pct_65pl,
                pct_with_bach_pl, pct_below_pov, median_income, 
                pct_renter_occ, median_yr_built),
    statistic = list(all_continuous() ~ "{mean}"),
    missing = "no", # don't list missing data separately
  ) |> 
  modify_header(
    label ~ "**Variable**",
    all_stat_cols() ~ "All Others (N = {N})") |>  
  bold_labels()

streets_treated <- streets_desc_stats %>%
  filter(treated==1)

table.treated <- 
  tbl_summary(
    streets_treated,
    include = c(median_lst_2019, median_lst_2021, median_lst_2023, median_lst_2024,
                pct_treec, pct_white, pct_black, pct_native, pct_asian, 
                pct_other, pct_hispanic, pct_under5, pct_5to17, pct_65pl,
                pct_with_bach_pl, pct_below_pov, median_income, 
                pct_renter_occ, median_yr_built),
    statistic = list(all_continuous() ~ "{mean}"),
    missing = "no", # don't list missing data separately
  ) |> 
  modify_header(
    label ~ "**Variable**",
    all_stat_cols() ~ "Treated (N = {N})") |>  
  bold_labels()

streets_eligible <- streets_desc_stats %>%
  filter(eligible==1)

table.eligible <- 
  tbl_summary(
    streets_eligible,
    include = c(median_lst_2019, median_lst_2021, median_lst_2023, median_lst_2024,
                pct_treec, pct_white, pct_black, pct_native, pct_asian, 
                pct_other, pct_hispanic, pct_under5, pct_5to17, pct_65pl,
                pct_with_bach_pl, pct_below_pov, median_income, 
                pct_renter_occ, median_yr_built),
    statistic = list(all_continuous() ~ "{mean}"),
    missing = "no", # don't list missing data separately
  ) |> 
  modify_header(
    label ~ "**Variable**",
    all_stat_cols() ~ "Eligible (N = {N})") |>  
  bold_labels()

# 2020 
streets_eligible_2020 = streets_desc_stats %>%
  filter(eligible_2020==1)

table.eligible_2020 <- 
  tbl_summary(
    streets_eligible_2020,
    include = c(median_lst_2019, 
                pct_treec, pct_white, pct_black, pct_native, pct_asian,
                pct_other, pct_hispanic, pct_under5, pct_5to17, pct_65pl,
                pct_with_bach_pl, pct_below_pov, median_income, 
                pct_renter_occ, median_yr_built),
    statistic = list(all_continuous() ~ "{mean}"),
    missing = "no", # don't list missing data separately
  ) |> 
  modify_header(
    label ~ "**Variable**",
    all_stat_cols() ~ "Eligible 2020 (N = {N})") |>  
  bold_labels()

streets_treat_2020 = streets_desc_stats %>%
  filter(treat_2020==1)

table.treat_2020 <- 
  tbl_summary(
    streets_treat_2020,
    include = c(median_lst_2019, 
                pct_treec, pct_white, pct_black, pct_native, pct_asian,  
                pct_other, pct_hispanic, pct_under5, pct_5to17, pct_65pl,
                pct_with_bach_pl, pct_below_pov, median_income, 
                pct_renter_occ, median_yr_built),
    statistic = list(all_continuous() ~ "{mean}"),
    missing = "no", # don't list missing data separately
  ) |> 
  modify_header(
    label ~ "**Variable**",
    all_stat_cols() ~ "Treat 2020 (N = {N})") |>  
  bold_labels()

# 2022 
streets_eligible_2022 = streets_desc_stats %>%
  filter(eligible_2022==1)

table.eligible_2022 <- 
  tbl_summary(
    streets_eligible_2022,
    include = c(median_lst_2021,
                pct_treec, pct_white, pct_black, pct_native, pct_asian, 
                pct_other, pct_hispanic, pct_under5, pct_5to17, pct_65pl,
                pct_with_bach_pl, pct_below_pov, median_income, 
                pct_renter_occ, median_yr_built),
    statistic = list(all_continuous() ~ "{mean}"),
    missing = "no", # don't list missing data separately
  ) |> 
  modify_header(
    label ~ "**Variable**",
    all_stat_cols() ~ "Eligible 2022 (N = {N})") |>  
  bold_labels()

streets_treat_2022 = streets_desc_stats %>%
  filter(treat_2022==1)

table.treat_2022 <- 
  tbl_summary(
    streets_treat_2022,
    include = c(median_lst_2021,
                pct_treec, pct_white, pct_black, pct_native, pct_asian, 
                pct_other, pct_hispanic, pct_under5, pct_5to17, pct_65pl,
                pct_with_bach_pl, pct_below_pov, median_income, 
                pct_renter_occ, median_yr_built),
    statistic = list(all_continuous() ~ "{mean}"),
    missing = "no", # don't list missing data separately
  ) |> 
  modify_header(
    label ~ "**Variable**",
    all_stat_cols() ~ "Treat 2022 (N = {N})") |>  
  bold_labels()

# 2024
streets_eligible_2024 = streets_desc_stats %>%
  filter(eligible_2024==1)

table.eligible_2024 <- 
  tbl_summary(
    streets_eligible_2024,
    include = c(median_lst_2023,
                pct_treec, pct_white, pct_black, pct_native, pct_asian, 
                pct_other, pct_hispanic, pct_under5, pct_5to17, pct_65pl,
                pct_with_bach_pl, pct_below_pov, median_income, 
                pct_renter_occ, median_yr_built),
    statistic = list(all_continuous() ~ "{mean}"),
    missing = "no", # don't list missing data separately
  ) |> 
  modify_header(
    label ~ "**Variable**",
    all_stat_cols() ~ "Eligible 2024 (N = {N})") |>  
  bold_labels()

streets_treat_2024 = streets_desc_stats %>%
  filter(treat_2024==1)

table.treat_2024 <- 
  tbl_summary(
    streets_treat_2024,
    include = c(median_lst_2023,
                pct_treec, pct_white, pct_black, pct_native, pct_asian, 
                pct_other, pct_hispanic, pct_under5, pct_5to17, pct_65pl,
                pct_with_bach_pl, pct_below_pov, median_income, 
                pct_renter_occ, median_yr_built),
    statistic = list(all_continuous() ~ "{mean}"),
    missing = "no", # don't list missing data separately
  ) |> 
  modify_header(
    label ~ "**Variable**",
    all_stat_cols() ~ "Treat 2024 (N = {N})") |>  
  bold_labels()

# 2025
streets_eligible_2025 = streets_desc_stats %>%
  filter(eligible_2025==1)

table.eligible_2025 <- 
  tbl_summary(
    streets_eligible_2025,
    include = c(median_lst_2024,
                pct_treec, pct_white, pct_black, pct_native, pct_asian, 
                pct_other, pct_hispanic, pct_under5, pct_5to17, pct_65pl,
                pct_with_bach_pl, pct_below_pov, median_income, 
                pct_renter_occ, median_yr_built),
    statistic = list(all_continuous() ~ "{mean}"),
    missing = "no", # don't list missing data separately
  ) |> 
  modify_header(
    label ~ "**Variable**",
    all_stat_cols() ~ "Eligible 2025 (N = {N})") |>  
  bold_labels() 

streets_treat_2025 = streets_desc_stats %>%
  filter(treat_2025==1)

table.treat_2025 <- 
  tbl_summary(
    streets_treat_2025,
    include = c(median_lst_2024,
                pct_treec, pct_white, pct_black, pct_native, pct_asian, 
                pct_other, pct_hispanic, pct_under5, pct_5to17, pct_65pl,
                pct_with_bach_pl, pct_below_pov, median_income, 
                pct_renter_occ, median_yr_built),
    statistic = list(all_continuous() ~ "{mean}"),
    missing = "no", # don't list missing data separately
  ) |> 
  modify_header(
    label ~ "**Variable**",
    all_stat_cols() ~ "Treat 2025 (N = {N})") |>  
  bold_labels() 

#### Combine Data ----

# MERGE - can't combine them in the tbl_summary format.... annoying!

tbl_allothers <- table.all_others$table_body %>% select(variable, stat_0) %>% rename(`All Others (N = 12,050)` = stat_0)
tbl_treated <- table.treated$table_body %>% select(variable, stat_0) %>% rename(`Treated (N = 567)` = stat_0)
tbl_eligible <- table.eligible$table_body %>% select(variable, stat_0) %>% rename(`Eligible (N = 468)` = stat_0)


tbl_eligible20 <- table.eligible_2020$table_body %>% select(variable, stat_0)  %>% rename(`Eligible 2020 (N = 433)` = stat_0)
tbl_treat20 <- table.treat_2020$table_body %>% select(variable, stat_0)  %>% rename(`Treat 2020 (N = 126)` = stat_0)
tbl_eligible22 <- table.eligible_2022$table_body %>% select(variable, stat_0)  %>% rename(`Eligible 2022 (N = 52)` = stat_0)
tbl_treat22 <- table.treat_2022$table_body %>% select(variable, stat_0)  %>% rename(`Treat 2022 (N = 113)` = stat_0)
tbl_eligible24 <- table.eligible_2024$table_body %>% select(variable, stat_0)  %>% rename(`Eligible 2024 (N = 203)` = stat_0)
tbl_treat24 <- table.treat_2024$table_body %>% select(variable, stat_0)  %>% rename(`Treat 2024 (N = 162)` = stat_0)
tbl_eligible25 <- table.eligible_2025$table_body %>% select(variable, stat_0)  %>% rename(`Eligible 2025 (N = 55)` = stat_0)
tbl_treat25 <- table.treat_2025$table_body %>% select(variable, stat_0)  %>% rename(`Treat 2025 (N = 231)` = stat_0)

merge_summary <- tbl_allothers %>%
  left_join(tbl_eligible, by="variable") %>%
  left_join(tbl_treated, by="variable") 

write.csv(merge_summary, "../Data/Results/07_Demographic_Desc_Stats_Summary.csv", row.names=F)
  
merge <- tbl_allothers %>%
  left_join(tbl_eligible20, by="variable") %>%
  left_join(tbl_treat20, by="variable") %>%
  left_join(tbl_eligible22, by="variable") %>%
  left_join(tbl_treat22, by="variable") %>%
  left_join(tbl_eligible24, by="variable") %>%
  left_join(tbl_treat24, by="variable") %>%
  left_join(tbl_eligible25, by="variable") %>%
  left_join(tbl_treat25, by="variable")

rm(list = ls(pattern = "^table"))
rm(list = ls(pattern = "^tbl"))

write.csv(merge, "../Data/Results/07_Demographic_Desc_Stats.csv", row.names=F)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2. Treatment Schedule Table ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sensors <- read_csv("../Data/Analysis/03_Sensor_Locations_Characteristics_All.csv")

sensors_table <- sensors %>%
  mutate(randomize = case_when(GMaps_ID == "ASHBURTON_KAPLAN DR_WICKHAM RD" ~ 10,
                               TRUE ~ randomize)) %>%
  select(randomize, treat_25, StreetName, SensorID) %>%
  mutate(treatment_date = case_when(randomize %in% c(2,3,19,35,36) ~ ymd("2025-09-12",tz="Etc/GMT+4"),
                                    randomize %in% c(1, 10) ~ ymd("2025-09-25",tz="Etc/GMT+4"),
                                    randomize %in% c(28,15,17,21) ~ ymd("2025-10-07",tz="Etc/GMT+4"),
                                    randomize %in% c(27,101) ~ ymd("2025-10-09",tz="Etc/GMT+4"),
                                    randomize %in% c(23) ~ ymd("2025-10-10",tz="Etc/GMT+4"),
                                    randomize %in% c(8,20,25,30) ~ ymd("2025-10-18",tz="Etc/GMT+4"),
                                    randomize %in% c(29) ~ ymd("2025-10-21",tz="Etc/GMT+4"),
                                    randomize %in% c(4) ~ ymd("2025-10-22",tz="Etc/GMT+4"),
                                    randomize %in% c(31, 9) ~ ymd("2025-10-23",tz="Etc/GMT+4"))) %>%
  mutate(treat_date = case_when(!is.na(treatment_date) ~ as.character(treatment_date),
                                TRUE ~ "Postponed")) %>%
  arrange(treatment_date) %>%
  select(-treatment_date) %>%
  pivot_wider(id_cols=c("randomize","treat_date"), names_from = c(treat_25), values_from=c("StreetName","SensorID")) %>%
  select(randomize, SensorID_1, StreetName_1, SensorID_0, StreetName_0, treat_date)

write.csv(sensors_table, "../Data/Analysis/07_Treatment_Schedule_Table.csv", row.names=F)
