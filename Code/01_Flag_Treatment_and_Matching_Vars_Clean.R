################################################################################
# Program Name: 01_Flag_Treatment_and_Matching_Vars.R
# Program Purpose: Convert 2025 paving treatment list to csv format

# Author: Katherine Burley Farr
# Contact: kburley@ad.unc.edu
# Affiliation: UNC Department of Public Policy, Data-Driven EnviroLab
################################################################################

rm(list=ls())

library(tidyverse)
library(pdftools)
library(stringr)
library(sf)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 1. Identify Current and Previous Treated Streets  ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

city_streets_csv <- read_csv("../Data/Original/Raleigh/City_of_Raleigh_Maintained_Streets.csv") %>%
  select(GlobalID, `Street Surface`) %>%
  rename(surface = `Street Surface`)

city_streets_sf <- st_read("../Data/Original/Raleigh/City_of_Raleigh_Maintained_Streets/City_of_Raleigh_Maintained_Streets.shp") %>% # too large for GH, get from GDrive
  mutate(street_name_merge = paste(COR_STREET,toupper(ST_TYPE_ST))) %>%
  select(c(street_name_merge, COR_BEGIN_, COR_END_DE, COR_STREET, ST_TYPE_ST, 
           COR_LENGTH, COR_ST_TYP, COR_WIDTH, DIRECTION_, LANE_COUNT, 
           LEAF_ID, LAST_YEA_2, GlobalID, geometry)) %>%
  rename(yr_repave = LAST_YEA_2) %>%
  left_join(city_streets_csv, by="GlobalID")

verified_streets <- read_csv("../Data/Original/Raleigh/FY25_Treatment_Streets_Verified.csv") %>% # includes some additional collected data 
  select(-c(utility_poles, light_poles)) 

previous_treated_streets <- st_read("../Data/Original/Raleigh/Rejuvenation_With_TiO2_(2020-2024)/RejuvenationWithTiO2.shp") %>%
  mutate(street_segment = paste(StreetName, BeginDesc, EndDesc,sep="/")) %>%
  # Fix a few street names to match with COR_STREET
  mutate(COR_STREET = case_when(StreetName == "KAPLAN DR" ~ "KAPLAN",
                                StreetName == "LAURNET PL" ~ "LAURNET",
                                StreetName == "ARBOR GRANDE WY" ~ "ARBOR GRANDE",
                                StreetName == "BILYEU ST" ~ "BILYEU",
                                StreetName == "INSPIRE DR" ~ "INSPIRE",
                                TRUE ~ StreetName))

# TREATMENT STREETS 2025

# to merge on to city streets
street_chars <- verified_streets %>%
  distinct(street_id, street_name_merge, sidewalks)

# Filter to segments with matching street names
filtered_streets <- city_streets_sf %>%
  filter(street_name_merge %in% unique(verified_streets$street_name_merge)) %>% # 93 street names!
  
  # FILTER SEGMENTS NOT INCLUDED IN LIST, BASED ON MANUAL CHECKS!
  mutate(keep = case_when(paste(COR_STREET,COR_BEGIN_,COR_END_DE,sep="/") %in% 
                            c("CHATHAM/STEVENS RD/MILBURNIE RD", "CHATHAM/DERBY DR/STEVENS RD",
                              "CHATHAM/ALBEMARLE AV/DERBY DR","CHATHAM/BERTIE DR/ALBEMARLE AV",
                              "CRABTREE/BEG PVMT #1341/WESTCHESTER RD", "CRABTREE/WESTCHESTER RD/N. KING CHARLES RD",
                              "CRABTREE/N. KING CHARLES RD/END PVMT #1505", "CRABTREE/RALEIGH BLVD/TIMBER DR",
                              "CRABTREE/TIMBER DR/CAPITAL BLVD", "RIDGE BROOK/BLUFFS VIEW DR/END MAINT",
                              "LILYMOUNT/PEBBLE RIDGE DR/BROWN OWL DR", "LILYMOUNT/CDS/PEBBLE RIDGE DR",
                              "MAYRIDGE/MAYBROOK CROSSING DR/PHASE LINE #1706", "RAVENWOOD/MELBOURNE RD/END PVMT #1024",
                              "MELBOURNE/KAPLAN DR/I-440 BRIDGE", "MELBOURNE/I-440 BRIDGE/AUKLAND DR",
                              "KAPLAN/KENT RD/MERWIN RD", "KAPLAN/MERWIN RD/WESTBRIDGE CT", 
                              "KAPLAN/WESTBRIDGE CT/SHERBURG CT", "KAPLAN/SHERBURG CT/DELMONT DR",
                              "KAPLAN/DELMONT DR/LORIMER RD", "KAPLAN/LORIMER RD/ASHBURTON RD",
                              "KAPLAN/ASHBURTON RD/MELBOURNE RD", "KAPLAN/MELBOURNE RD/PINEVIEW DR",
                              "KAPLAN/PINEVIEW DR/RAVEL ST", "KAPLAN/RAVEL ST/SWALLOW DR",
                              "KAPLAN/SWALLOW DR/ATHENS DR", "KAPLAN/ATHENS DR/THEA LN",
                              "MARCOM/GORMAN ST/STOVALL DR", "HAMPTON/KIPAWA ST/CDS",
                              "RATCHFORD/MEADOW WOOD RBT/END PVMT / COR MAINT",
                              "HILL/E. LANE ST/OAKWOOD AV", "HILL/PENDER ST/E. LANE ST",
                              "HILL/E. JONES ST/PENDER ST", "HILL/BOYER ST/E. JONES ST",
                              "HILL/NEW BERN AVE/BOYER ST", "HILL/BEG C&G #2310/FIELDS BROADLANDS DR",
                              "HILL/FIELDS OF BROADLANDS DR/PRVT PROP", "PARRISH/E. LENOIR ST/BEAUTY AV",
                              "PARRISH/BEAUTY AV/JOE LOUIS AV", "MERRYWOOD/E. LENOIR ST/BEAUTY AV",
                              "MERRYWOOD/BEAUTY AV/JOE LOUIS AV", "MERRYWOOD/JOE LOUIS AV/MALTA AV",
                              "MERRYWOOD/MALTA AV/ROCK QUARRY RD", "MACKINAC ISLAND/MARSHLANE WY/MONCACY DR",
                              "MACKINAC ISLAND/KESSLERS CROSS DR/MARSHLANE WY", 
                              "MACKINAC ISLAND/PADUCAH DR/KESSLERS CROSS DR",
                              "MACKINAC ISLAND/GUARD HILL DR/PADUCAH DR", 
                              "MACKINAC ISLAND/QUITMAN TR/GUARD HILL DR",
                              "MACKINAC ISLAND/BISLAND DR/QUITMAN TR","MACKINAC ISLAND/PADUCAH DR/BISLAND DR",
                              "MACKINAC ISLAND/BARRINGTON DR/PADUCAH DR",
                              "TRIANGLE TOWN/OLD WAKE FOREST RD/FUTURE E. SIDE ST",
                              "TRIANGLE TOWN/FUTURE E. SIDE ST/TOWN DR", "TRIANGLE TOWN/TOWN DR/BEGIN NCDOT C/A") ~ 0,
                          (COR_STREET == "OAKWOOD" & !COR_BEGIN_ %in% c("N. TARBORO ST","ST AUGUSTINE AV")) ~ 0,
                          (COR_STREET == "MILBURNIE" & !COR_BEGIN_ %in% c("HILL ST","DELANY DR","BOOKER DR")) ~ 0,
                          (COR_STREET == "NORTH HILLS" & !COR_BEGIN_ %in% c("LEAD MINE RD", "HILLOCK DR", 
                                                                            "OLD VILLAGE RD", "NORTHBROOK DR",
                                                                            "KIMBERLY DR","THAYER DR")) ~ 0,
                          (COR_STREET == "TANBARK" & !COR_BEGIN_ %in% c("HUNTING RIDGE RD","BUCKHEAD DR","GRIST MILL RD")) ~ 0,
                          (COR_STREET == "ONEAL" & !COR_BEGIN_ %in% c("STRAWBERRY MEADOWS ST","PRIDE WY","LEESVILLE HIGH GYM")) ~ 0,
                          (COR_STREET == "SUMNER" & !COR_BEGIN_ == "CAPITAL BLVD") ~ 0,
                          TRUE ~ 1))

# Create Final File for Export - filter out extra street segments, flag for potential install locations
filtered_final <- filtered_streets %>%
  filter(keep == 1) %>%
  select(-keep) %>%
  
  # Bring in other collected data
  left_join(street_chars, by="street_name_merge") %>%
  
  # Prepare for merge
  rename(st_nm_mrg = street_name_merge) %>% # ,
  # duke_poles = utility_poles,
  # lite_poles = light_poles) %>%
  st_zm(drop=T, what="ZM") # for some reason we have '3D Line String' which is causing issues

# PREVIOUSLY TREATED STREETS (2020-2024)
# Note: cartier from oberlin to gordon appears mislabeled - excluding both gordon and cartier
# Note: chapanoke - sf does not line up perfectly with street segments - including segment to hammond rd.

prev_treated_chars <- previous_treated_streets %>%
  distinct(COR_STREET, Year) %>%
  group_by(COR_STREET) %>%
  mutate(treat_year = paste0(Year, collapse="/")) %>%
  ungroup() %>%
  distinct(COR_STREET, treat_year) 

prev_treated <- city_streets_sf %>%
  filter(COR_STREET %in% unique(previous_treated_streets$COR_STREET)) %>%
  select(c(street_name_merge, COR_BEGIN_, COR_END_DE, COR_STREET, ST_TYPE_ST, 
           COR_LENGTH, COR_ST_TYP, COR_WIDTH, DIRECTION_, LANE_COUNT, 
           LEAF_ID, GlobalID, geometry)) %>% # 173 street names!
  mutate(keep = case_when((COR_STREET == "ARNOLD PALMER" & COR_BEGIN_ %in% c("CENTERWOOD DR","ROUND BROOK CR","MIZNER LN",
                                                                             "SPORTING CLUB DR","WINGED THISTLE CT", 
                                                                             "CLUB HILL DR", "CLUB HILL DR","TAWNY CHASE DR",
                                                                             "WHITE EAGLE CT","LUMLEY RD LOOP")) ~ 0,
                          (COR_STREET == "MARVINO" & COR_BEGIN_ %in% c("BAREFOOT INDUSTRIAL RD","EBENEZER CHURCH RD",
                                                                       "SOMMERWELL ST","BILLINGSWORTH WY",
                                                                       "GLENWOOD AVE")) ~ 0,
                          (COR_STREET == "COUNTRY" & ST_TYPE_ST == "Ct") ~ 0,
                          (COR_STREET == "COUNTRY" & COR_BEGIN_ %in% c("PRIDE WY", "PINE TIMBER DR", "LEESVILLE RD")) ~ 0,
                          (COR_STREET == "GROVE BARTON" & COR_BEGIN_ %in% c("DOIE COPE RD","LYNN RD")) ~ 0,
                          (COR_STREET == "ROMEALIA" & COR_BEGIN_ == "STONEHENGE PARK DR") ~ 0,
                          (COR_STREET == "THE LAKES" & COR_END_DE == "LAKE HILL DR") ~ 0,
                          (COR_STREET == "OCTOBER" & COR_BEGIN_ == "NEALSTONE WY" ~ 0),
                          (COR_STREET == "STONEY WOODS" & COR_BEGIN_ == "STONEYRIDGE DR") ~ 0,
                          (COR_STREET == "WAKE BLUFF" & COR_BEGIN_ %in% c("WIDE RIVER DR","THOUGHTFUL SPOT WY","HIDDEN VALE DR")) ~ 0,
                          (COR_STREET == "CARLTON" & ST_TYPE_ST == "Dr") ~ 0,
                          (COR_STREET == "LINVILLE RIDGE" & COR_BEGIN_ %in% c("SAPPHIRE VALLEY DR N.","SAPPHIRE VALLEY DR S.",
                                                                              "EAGLE TRACE DR")) ~ 0,
                          (COR_STREET == "BEACON VILLAGE" & COR_BEGIN_ %in% c("BEACON HEIGHTS DR","BEACON VALLEY DR")) ~ 0,
                          (COR_STREET == "MACKINAC ISLAND" & COR_BEGIN_ %in% c("MARSHLANE WY","KESSLERS CROSS DR",
                                                                               "PADUCAH DR","GUARD HILL DR","QUITMAN TR",
                                                                               "BISLAND DR","BARRINGTON DR")) ~ 0,
                          (COR_STREET == "GENEROSITY" & COR_BEGIN_ == "JONES SAUSAGE RD") ~ 0,
                          (COR_STREET == "BILYEU" & COR_BEGIN_ %in% c("KIRBY ST E.", "KIRBY ST W.")) ~ 0,
                          # Keep specific streets
                          (COR_STREET == "OLD HUNDRED" & COR_END_DE != "STONEHENGE FARM LN") ~ 0,
                          (COR_STREET == "WILDERNESS" & COR_BEGIN_ != "STONEHENGE FARM LN E.") ~ 0,
                          (COR_STREET == "STONEHENGE PARK" & COR_BEGIN_ != "STONEHENGE FARM LN") ~ 0,
                          (COR_STREET == "COXINDALE" & !COR_BEGIN_ %in% c("LITCHFORD RD", "AVERELL CT")) ~ 0,
                          (COR_STREET == "DAWNSHIRE" & COR_BEGIN_ != "ROSWELL RD") ~ 0,
                          (COR_STREET == "FALLS RIVER" & !COR_BEGIN_ %in% c("FALLSRIVER/DUNN S. CR","CATARA DR",
                                                                            "FALLSRIVER/BRIGHTHAVEN W.","FILIGREE CT",
                                                                            "RIVERBANK DR","GRASSY CREEK PL","SAGEHURST PL",
                                                                            "FALLSRIVER/BRIGHTHAVEN E.","BRIGHTHAVEN DR",
                                                                            "MORGAUSE DR","CONNALLY LN")) ~ 0,
                          (COR_STREET == "SPRUCE TREE" & COR_BEGIN_ != "FALLS OF NEUSE RD") ~ 0,
                          (COR_STREET == "TOWN CENTER" & !COR_BEGIN_ %in% c("OLD WAKE FOREST RD", "SECU ENT")) ~ 0,
                          (COR_STREET == "BRENTWOOD" & !COR_BEGIN_ %in% c("I-440 RAMPS","LAKE WOODARD DR",
                                                                          "WESTINGHOUSE BV","RALEIGH BLVD",
                                                                          "STONY BROOK DR")) ~ 0,
                          (COR_STREET == "VIRTUOUS" & COR_BEGIN_ !="BEG PVMT") ~ 0,
                          (COR_STREET == "QUARRY RIDGE" & !COR_BEGIN_ %in% c("QUARRY SPRINGS RD","TRYON RIDGE DR")) ~ 0,
                          (COR_STREET == "SNOWBERRY" & !COR_BEGIN_ %in% c("PASCHALL CT","BENTONS RIDGE DR")) ~ 0,
                          (COR_STREET == "AUTHORITY" & COR_BEGIN_ != "END PVMT RR") ~ 0,
                          (COR_STREET == "CHAPANOKE" & !COR_BEGIN_ %in% c("S. WILMINGTON SERV RD","HAMMOND RD")) ~ 0,
                          (COR_STREET == "KAPLAN" & !COR_BEGIN_ %in% c("HUNTING HORN LN","BURGESS CT",
                                                                       "HUNTERS CLUB DR","TREXLER CT","GORMAN ST")) ~ 0,
                          (COR_STREET == "BEACON LAKE" & !COR_BEGIN_ %in% c("MEMO CT","CDS")) ~ 0,
                          TRUE ~ 1)) %>%
  filter(keep == 1) %>%
  select(-keep) %>%
  left_join(prev_treated_chars, by="COR_STREET") %>%
  st_zm(drop=T, what="ZM") # for some reason we have '3D Line String' which is causing issues

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2. Combine with treatment data from this year and previous  ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create Master Dataset of all city maintained streets
treat_streets_tomerge <- filtered_final %>% 
  st_drop_geometry() %>%
  select(GlobalID, street_id, sidewalks) # prev_treat, potential, duke_poles, lite_poles, any_poles - add these later! 

prev_treat_tomerge <- prev_treated %>%
  st_drop_geometry() %>%
  select(GlobalID, treat_year)

rm(prev_treated, prev_treated_chars, previous_treated_streets, 
   filtered_streets, filtered_final, street_chars, verified_streets)

# BRING IN THE INFORMATION ON TREATED + PREVIOUSLY TREATED STREETS!
all_streets_flagged <- city_streets_sf %>%
  left_join(treat_streets_tomerge, by="GlobalID") %>% # 231 to be treated segments
  left_join(prev_treat_tomerge, by="GlobalID") %>% # 398 previously treated segments
  
  # flag segments previously treated
  mutate(prev_treat = ifelse(!is.na(treat_year),1,0)) %>% 
  
  # flag segments to be treated this year
  mutate(treat_25 = ifelse(!is.na(street_id),1,0)) %>%
  
  # flag subset of those feasible for sensor installs
  mutate(potential_treat = ifelse((treat_25==1 & prev_treat == 0),1,0)) %>% # 169 feasible treatment street segments 
  mutate(potential_comp = ifelse((treat_25==0 & prev_treat==0 & yr_repave %in% c(2018,2019,2020,2021,2022,2023)),1,0)) %>% # 518 potential comparison
  
  st_zm(drop=T, what="ZM") %>% # to enable export
  select(-street_name_merge) %>%
  mutate(sqyd_est = ((COR_LENGTH/3)*(COR_WIDTH/3))) %>%
  select(GlobalID, COR_STREET, COR_BEGIN_, COR_END_DE, everything())

rm(prev_treat_tomerge, treat_streets_tomerge)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2. Combine with manually collected pole data ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Bring in SEGMENT level data on poles
##Where the hell do I get these .csv files
comp_street_poles <- read_csv("../Data/Original/Raleigh/Comparison_Streets_Pole_Collection_ORIG_Complete.csv") %>%
  select(GlobalID, duke_poles, lite_poles, sidewalks, how_many_poles) # already have sidewalks in the main df so will need to coalesce these 

##Where the hell do I get these .csv files
treat_street_poles <- read_csv("../Data/Original/Raleigh/Treatment_Streets_Pole_Collection_ORIG_Complete.csv") %>%
  filter(!is.na(how_many_poles)) %>% # remove the one segment on Mackinac Island, wakefield pines segment is OK! 
  select(-`...9`) %>%
  select(GlobalID, duke_poles, lite_poles, how_many_poles)

# wakefield pines is in comp_street_poles, but is a potential treat segment - that's why numbers are 1 off - this won't matter in the full df

pole_data <- comp_street_poles %>%
  bind_rows(treat_street_poles)

# COMBINE
all_streets_poles <- all_streets_flagged %>%
  left_join(pole_data, by="GlobalID") %>%
  mutate(sidewalks = coalesce(sidewalks.x, sidewalks.y)) %>%
  select(-c(sidewalks.x, sidewalks.y)) %>%
  mutate(any_poles = how_many_poles!=0) # 23% of potential treat + potential comp poles
# 123 potential treat segments w/ accessible poles
# 530 potential comp segments w/ accessible poles

# View(all_streets_poles[all_streets_poles$potential_treat==1 | all_streets_poles$potential_comp==1,]) # make sure no pole data is missing
rm(all_streets_flagged, pole_data, comp_street_poles, treat_street_poles)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 3. Bring in other processed data for ALL streets ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# HEATWATCH DATA
# Processing: Avg. value w/in 15m buffer
heatwatch_af_hi <- read_csv("../Data/Original/Raleigh/Calculated_Street_Vars/All_Streets_AF_HI_F.csv") %>%
  select(GlobalID, MEAN) %>%
  rename(hw_af_hi_f = MEAN)

heatwatch_af_t <- read_csv("../Data/Original/Raleigh/Calculated_Street_Vars/All_Streets_AF_T_F.csv") %>%
  select(GlobalID, MEAN) %>%
  rename(hw_af_t_f = MEAN)

heatwatch_am_hi <- read_csv("../Data/Original/Raleigh/Calculated_Street_Vars/All_Streets_AM_HI_F.csv") %>%
  select(GlobalID, MEAN) %>%
  rename(hw_am_hi_f = MEAN)

heatwatch_am_t <- read_csv("../Data/Original/Raleigh/Calculated_Street_Vars/All_Streets_AM_T_F.csv") %>%
  select(GlobalID, MEAN) %>%
  rename(hw_am_t_f = MEAN)

heatwatch_pm_hi <- read_csv("../Data/Original/Raleigh/Calculated_Street_Vars/All_Streets_PM_HI_F.csv") %>%
  select(GlobalID, MEAN) %>%
  rename(hw_pm_hi_f = MEAN)

heatwatch_pm_t <- read_csv("../Data/Original/Raleigh/Calculated_Street_Vars/All_Streets_PM_T_F.csv") %>%
  select(GlobalID, MEAN) %>%
  rename(hw_pm_t_f = MEAN)

# Local Climate Zones - https://essd.copernicus.org/articles/14/3835/2022/, data from GEE
# Processing: Nearest point using 35m buffer for each street segment
lcz <- read_csv("../Data/Original/Raleigh/Calculated_Street_Vars/All_Streets_LCZ.csv") %>%
  select(GlobalID, grid_code) %>%
  mutate(LCZ_filter = case_when(grid_code == 11 ~ "A",
                                grid_code == 12 ~ "B",
                                grid_code == 13 ~ "C",
                                grid_code == 14 ~ "D",
                                grid_code == 15 ~ "E",
                                grid_code == 16 ~ "F",
                                grid_code == 17 ~ "G",
                                TRUE ~ as.character(grid_code)),
         LCZ_desc = case_when(grid_code == 1 ~ "Compact highrise",
                              grid_code == 2 ~ "Compact midrise",
                              grid_code == 3 ~ "Compact lowrise",
                              grid_code == 4 ~ "Open highrise",
                              grid_code == 5 ~ "Open midrise",
                              grid_code == 6 ~ "Open lowrise",
                              grid_code == 7 ~ "Lightweight lowrise",
                              grid_code == 8 ~ "Large lowrise",
                              grid_code == 9 ~ "Sparsely built",
                              grid_code == 10 ~ "Heavy industry",
                              grid_code == 11 ~ "Dense Trees (LCZ A)",
                              grid_code == 12 ~ "Scattered Trees (LCZ B)",
                              grid_code == 13 ~ "Bush, scrub (LCZ C)",
                              grid_code == 14 ~ "Low plants (LCZ D)",
                              grid_code == 15 ~ "Bare rock or paved (LCZ E)",
                              grid_code == 16 ~ "Bare soil or sand (LCZ F)",
                              grid_code == 17 ~ "Water (LCZ G)"))

# Canopy Cover
# Processing: % canopy coverage w/i 15m buffer of street segment
treecover <- st_read("../Data/Orig/Raleigh/Calculated_Street_Vars/AllStreets_Treecover.shp") %>% # too large for GH, get from GDrive
  st_drop_geometry() %>%
  select(All_Str_10, mean_treec) %>%
  rename(GlobalID = All_Str_10,
         pct_treec = mean_treec)

# UHI Quantile Assignment
# Processing: 15m street segment buffer - spatial join - largest overlap
uhi_quantile <- read_csv("../Data/Orig/Raleigh/Calculated_Street_Vars/All_Streets_UHIQuantile_Assigned.csv") %>%
  select(GlobalID, UHI_Qtile)

all_streets_allvars <- all_streets_poles %>%
  left_join(lcz, by="GlobalID") %>%
  left_join(uhi_quantile, by="GlobalID") %>%
  left_join(treecover, by="GlobalID") %>%
  left_join(heatwatch_af_hi, by="GlobalID") %>%
  left_join(heatwatch_af_t, by="GlobalID") %>%
  left_join(heatwatch_am_hi, by="GlobalID") %>%
  left_join(heatwatch_am_t, by="GlobalID") %>%
  left_join(heatwatch_pm_hi, by="GlobalID") %>%
  left_join(heatwatch_pm_t, by="GlobalID") %>%
  # rename a few with too long of names for shp
  rename(pot_treat = potential_treat,
         pot_comp = potential_comp,
         pole_est = how_many_poles)

# View(all_streets_allvars[all_streets_allvars$potential_treat==1 | all_streets_allvars$potential_comp==1,])
# a few missing UHI quantile and heatwatch data.

rm(heatwatch_af_hi, heatwatch_af_t, heatwatch_am_hi, heatwatch_am_t,
   heatwatch_pm_hi, heatwatch_pm_t, lcz, uhi_quantile, treecover)

st_write(all_streets_allvars, "../Data/Analysis/01_All_Streets_All_Vars.shp", append=FALSE)

# DATA FOR MATCHING!
matching_subset <- all_streets_allvars %>%
  filter(pot_treat==1 | pot_comp==1)

# 28 missing UHI quantile
# 10 missing heatwatch data

st_write(matching_subset, "../Data/Analysis/01_Streets_for_Matching.shp", append=FALSE)
