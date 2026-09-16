################################################################################
# Program Name: 03_Combine_Streets_and_Sensor_Characteristics.R
# Program Purpose: Combine sensor level data + variables with street segment data

# Author: Katherine Burley Farr
# Contact: kburley@ad.unc.edu
# Affiliation: UNC Department of Public Policy, Data-Driven EnviroLab
################################################################################

rm(list=ls())

library(tidyverse)
library(pdftools)
library(stringr)
library(sf)
library(broom)
library(gtsummary)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 1. Bring in Data ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# EXPORTS FROM ARCGIS ONLINE (KBF RALEIGH KESTREL SENSORS - Feature Class)
sensor_chars_final <- read_csv("../Data/Analysis/KBF_Raleigh_Kestrel_Sensors_CSV_Final/Sensors_0.csv")
sensor_chars_final_shp <- st_read("../Data/Analysis/KBF_Raleigh_Kestrel_Sensors_SHP_Final/Sensors.shp")


street_segments <- read_csv("../Data/Analysis/02_Matched_Randomized_Street_Segments.csv")
replacement_match <- read_csv("../Data/Analysis/03_Stolen_Sensor_Replacement_Matches.csv") %>%
  mutate(LCZ_filter = as.character(LCZ_filter)) %>%
  filter(COR_STREET == "YELVERTON") %>% # replacement for Montcastle, match for Smith Basin
  mutate(randomize = 4)

street_segments_all <- street_segments %>%
  bind_rows(replacement_match)

sensor_latlon <- sensor_chars_final %>% # sensor_chars_orig
  select(SensorID, x, y) %>%
  mutate(SensorID = as.character(SensorID)) %>%
  rename(lon_x = x, 
         lat_y = y)

combined <- sensor_chars_final_shp %>%
  left_join(street_segments_all, by="GMaps_ID") %>%
  left_join(sensor_latlon, by="SensorID") %>%

  rename(GlobalID = GlobalID.x) %>%
  st_zm()

st_write(combined, "../Data/Analysis/03_Sensor_Locations_Characteristics_All.shp")
combined_csv <- combined %>% st_drop_geometry()
write.csv(combined_csv, "../Data/Analysis/03_Sensor_Locations_Characteristics_All.csv", row.names=FALSE)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 1. Street Segment and Sensor Table ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

combined_csv <- read_csv("../Data/Analysis/03_Sensor_Locations_Characteristics_All.csv")

table_data <- combined_csv %>%
  filter(randomize != 101) %>%
  # Based on sensor orientation, assign to the closest cardinal direction
  separate_wider_delim(cols=SensDirect, names=c("degrees", "direction"), delim=" ", cols_remove = F) %>%
  mutate(sensor_direction = case_when(direction %in% c("N","E","S","W") ~ direction,
                                      direction == "NE" & degrees <= 45 ~ "N",
                                      direction == "NE" & degrees > 45 ~ "E",
                                      direction == "SE" & degrees > 135 ~ "S", # none <=135
                                      direction == "SW" & degrees <=225 ~ "S",
                                      direction == "SW" & degrees > 225 ~ "W",
                                      direction == "NW" & degrees <= 315 ~ "W")) # none > 315

table.sensors <- 
  tbl_summary(
    table_data,
    include = c(COR_WIDTH, LANE_COUNT, UHI_Qtile2, pct_treec, hw_af_t_f, hw_af_hi_f,
                hw_am_hi_f, hw_am_t_f, hw_pm_hi_f, hw_pm_t_f, sidewalks, LCZ_desc, repave_grp,
                St2Pole_in, Shade, Sidewalk, sensor_direction),
    statistic = list(all_continuous() ~ "{mean}, ({sd})"),
    by = treat_25, # split table by group
    missing = "no" # don't list missing data separately
  ) |> 
  add_n() |> # add column with total number of non-missing observations
  add_p(pvalue_fun = ~style_sigfig(., digits = 2)) |> # test for a difference between groups
  modify_header(label = "**Variable**") |> # update the column header
  bold_labels()

table.sensors
table.sensors |>
  as_gt() |> gt::gtsave("../Data/Results/03_Sensor_Segment_Group_Equivalence.docx")
