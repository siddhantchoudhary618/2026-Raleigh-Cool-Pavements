################################################################################
# Program Name: 02_Match_and_Randomize_StreetSegments.R
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
library(MatchIt)
library(broom)
library(gtsummary)
library(cobalt)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 1. Bring in Data ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

matching_subset_orig <- st_read("../Data/Analysis/01_Streets_for_Matching.shp")

matching_subset_poles <- matching_subset_orig %>%
  filter(any_poles == TRUE) %>% # 124 treat, 406 comparison 
  # a few fall outside the qtile assignment - manually assign to nearest quantile
  mutate(UHI_Qtile_temp = case_when(is.na(UHI_Qtile) & COR_STREET=="BOND" ~ 9,
                                    is.na(UHI_Qtile) & COR_STREET=="BREELAND" ~ 1,
                                    is.na(UHI_Qtile) & COR_STREET=="BRIDGEMAN" ~ 1,
                                    is.na(UHI_Qtile) & COR_STREET=="FARMINGTON GROVE" ~ 2,
                                    is.na(UHI_Qtile) & COR_STREET=="FOREST PINES" ~ 5,
                                    is.na(UHI_Qtile) & COR_STREET=="FOUNTAIN PARK" ~ 3,
                                    is.na(UHI_Qtile) & COR_STREET=="GABE" ~ 3,
                                    is.na(UHI_Qtile) & COR_STREET=="NORTH EXETER" ~ 1,
                                    is.na(UHI_Qtile) & COR_STREET=="ROCK BARN" ~ 3,
                                    is.na(UHI_Qtile) & COR_STREET=="SMITH BASIN" ~ 2,
                                    is.na(UHI_Qtile) & COR_STREET=="SPINDLEWOOD" ~ 1,
                                    is.na(UHI_Qtile) & COR_STREET=="SUMMERDALE" ~ 9,
                                    is.na(UHI_Qtile) & COR_STREET=="THISTLEDOWN" ~ 2,
                                    is.na(UHI_Qtile) & COR_STREET=="ONEAL" ~ 3,
                                    is.na(UHI_Qtile) & COR_STREET=="WINSTON DIAMOND" ~ 6,
                                    TRUE ~ UHI_Qtile)) %>% 
  filter(!is.na(hw_af_hi_f)) %>% # TEMPORARY - although may just exclude these, they are very far out
  mutate(repave_grp = case_when(yr_repave %in% c(2018,2019,2020) ~ "older",
                                yr_repave %in% c(2021,2022,2023) ~ "newer")) # for exact matching

matching_subset_repave <- matching_subset_poles %>%
  filter(!is.na(repave_grp)) # a handful of segments that were not flagged as repaved in 2019 or 2022

# Check initial imbalance:
m.out0 <- matchit(pot_treat ~ COR_WIDTH + LANE_COUNT + UHI_Qtile_temp + 
                    pct_treec + hw_af_t_f + hw_am_t_f, # 
                  data = matching_subset_repave, method=NULL, distance="glm")
summary(m.out0) # honestly not even that bad. 

# Initial Matching - No Exact Matching
m.out1 <- matchit(pot_treat ~ COR_WIDTH + UHI_Qtile_temp + pct_treec + yr_repave, 
                  data = matching_subset_repave, method="nearest", distance="glm") 
summary(m.out1)
plot(summary(m.out1))

m.out1.matches <- match.data(m.out1, distance="prop.score") %>% st_drop_geometry()

plot(m.out1, type = "histogram")
plot(m.out1, type = "jitter", interactive = FALSE)

plot(m.out1, type="density", interactive=FALSE, 
     which.xs = ~pct_treec + hw_af_t_f + hw_am_t_f)

plot(m.out1,type="density", interactive=FALSE,
     which.xs = ~COR_WIDTH + LANE_COUNT + as.factor(UHI_Qtile_temp))

# Check years - do units get matches more than 1 yr difference?
check_yrs <- m.out1.matches %>%
  select(subclass, pot_treat, yr_repave) %>%
  pivot_wider(names_from=pot_treat, values_from=yr_repave) %>%
  mutate(diff = `1`-`0`)

# Add Exact Matching by Repave Year - PS
m.out2 <- matchit(pot_treat ~ UHI_Qtile_temp + pct_treec + COR_WIDTH, 
                  data = matching_subset_repave, method="nearest", distance="glm", exact = "repave_grp")
summary(m.out2)
plot(summary(m.out2))

m.out2.matches <- match.data(m.out2, distance="prop.score") %>% st_drop_geometry()

plot(m.out2, type = "histogram")
plot(m.out2, type = "jitter", interactive = FALSE)

plot(m.out2, type="density", interactive=FALSE, 
     which.xs = ~pct_treec + hw_af_t_f + hw_am_t_f)

plot(m.out2,type="density", interactive=FALSE,
     which.xs = ~COR_WIDTH + LANE_COUNT + as.factor(UHI_Qtile_temp))

# Exact Matching w/i Repave Year + Distance Matching
m.out3 <- matchit(pot_treat ~ UHI_Qtile_temp + pct_treec + COR_WIDTH, 
                  data = matching_subset_repave, method="nearest", distance="mahalanobis", exact = "repave_grp")
m.out3.matches <- match.data(m.out3) %>% st_drop_geometry()

summary(m.out3)
plot(summary(m.out3))

plot(m.out3,type="density", interactive=FALSE,
     which.xs = ~pct_treec + COR_WIDTH + as.factor(UHI_Qtile_temp))

# LOVE PLOT
love.plot(pot_treat ~ pct_treec + UHI_Qtile_temp + COR_WIDTH + LANE_COUNT + hw_af_t_f + hw_am_t_f, 
          data = matching_subset_repave, weights = list("Prop. Score" = m.out2, "Distance" = m.out3), 
          abs = T, thresholds = 0.1,
          var.names = c(pct_treec = "% Treecover",
                        UHI_Qtile_temp= "UHI Qtile",
                        COR_WIDTH = "Street Segment Width",
                        LANE_COUNT = "Street Segment Lanes",
                        hw_af_t_f = "H.W. Afternoon Temp (F)",
                        hw_am_t_f = "H.W. Morning Temp (F)"))

# COMPARE EQUIVALENCE
table0 <-
  tbl_summary(
    matching_subset_repave %>% st_drop_geometry(),
    include = c(COR_WIDTH, LANE_COUNT, UHI_Qtile_temp, pct_treec, hw_af_t_f, hw_af_hi_f,
                hw_am_hi_f, hw_am_t_f, hw_pm_hi_f, hw_pm_t_f, sidewalks, LCZ_desc, repave_grp),
    statistic = list(all_continuous() ~ "{mean}, ({sd})"),
    by = pot_treat, # split table by group
    missing = "no" # don't list missing data separately
  ) |> 
  add_n() |> # add column with total number of non-missing observations
  add_p(pvalue_fun = ~style_sigfig(., digits = 2)) |> # test for a difference between groups
  modify_header(label = "**Variable**") |> # update the column header
  bold_labels() 

table0 |>
  as_gt() |> gt::gtsave("../Data/Results/02_group_equivalence_prematch.docx")

table1 <-
  tbl_summary(
    m.out1.matches,
    include = c(COR_WIDTH, LANE_COUNT, UHI_Qtile_temp, pct_treec, hw_af_t_f, hw_af_hi_f,
                hw_am_hi_f, hw_am_t_f, hw_pm_hi_f, hw_pm_t_f, sidewalks, LCZ_desc, repave_grp),
    statistic = list(all_continuous() ~ "{mean}, ({sd})"),
    by = pot_treat, # split table by group
    missing = "no" # don't list missing data separately
  ) |> 
  add_n() |> # add column with total number of non-missing observations
  add_p(pvalue_fun = ~style_sigfig(., digits = 2)) |> # test for a difference between groups
  modify_header(label = "**Variable**") |> # update the column header
  bold_labels()

table1 |>
  as_gt() |> gt::gtsave("../Data/Results/02_group_equivalence_match1.docx")

table2 <-
  tbl_summary(
    m.out2.matches,
    include = c(COR_WIDTH, LANE_COUNT, UHI_Qtile_temp, pct_treec, hw_af_t_f, hw_af_hi_f,
                hw_am_hi_f, hw_am_t_f, hw_pm_hi_f, hw_pm_t_f, sidewalks, LCZ_desc, repave_grp),
    statistic = list(all_continuous() ~ "{mean}, ({sd})"),
    by = pot_treat, # split table by group
    missing = "no" # don't list missing data separately
  ) |> 
  add_n() |> # add column with total number of non-missing observations
  add_p(pvalue_fun = ~style_sigfig(., digits = 2)) |> # test for a difference between groups
  modify_header(label = "**Variable**") |> # update the column header
  bold_labels()

table2 |>
  as_gt() |> gt::gtsave("../Data/Results/02_group_equivalence_match2.docx")

table3 <-
  tbl_summary(
    m.out3.matches,
    include = c(COR_WIDTH, LANE_COUNT, UHI_Qtile_temp, pct_treec, hw_af_t_f, hw_af_hi_f,
                hw_am_hi_f, hw_am_t_f, hw_pm_hi_f, hw_pm_t_f, sidewalks, LCZ_desc, repave_grp),
    statistic = list(all_continuous() ~ "{mean}, ({sd})"),
    by = pot_treat, # split table by group
    missing = "no" # don't list missing data separately
  ) |> 
  add_n() |> # add column with total number of non-missing observations
  add_p(pvalue_fun = ~style_sigfig(., digits = 2)) |> # test for a difference between groups
  modify_header(label = "**Variable**") |> # update the column header
  bold_labels()

table3 |>
  as_gt() |> gt::gtsave("../Data/Results/02_group_equivalence_match3.docx")

# RANDOMIZE
set.seed(66)

randomize_pairs <- m.out3.matches %>% 
  select(GlobalID, COR_STREET, COR_BEGIN_, COR_END_DE, pot_treat, subclass) %>% # , prop.score
  pivot_wider(names_from = pot_treat, values_from = c(GlobalID, COR_STREET, COR_BEGIN_, COR_END_DE)) %>% # , prop.score
  arrange(subclass) %>%
  mutate(randomize = sample(1:max(as.numeric(m.out3.matches$subclass)),max(as.numeric(m.out3.matches$subclass)), replace=F))

random_selection <- randomize_pairs %>%
  filter(randomize <= 37 | randomize==99) %>% # want the first 30, but excluded 7 after physical checks, excluded 1 after install
  # EXCLUDE STREETS AFTER MANUAL CHECKS - DOWN TO EXACTLY 30 PAIRS!
  filter(!COR_STREET_1 %in% c("WILD DUNES", "INDIANWOOD", "LAUREL VALLEY", "HIGHWOODS","HAMPTON")) %>%
  filter(!COR_STREET_0 %in% c("LAKE", "HEATHGATE", "FULTON")) %>%
  select(subclass, randomize, GlobalID_1, COR_STREET_1, COR_BEGIN__1, COR_END_DE_1, GlobalID_0, COR_STREET_0, COR_BEGIN__0, COR_END_DE_0) %>%
  arrange(randomize)

# Export to print and take on installs
write.csv(random_selection, "../Data/Analysis/02_Matched_Random_Selection_Pairs.csv", row.names=FALSE)

# TO EXCLUDE:
#   
# - WILD DUNES_LONG POINT CT_GRAND CYPRESS CT (no accessible poles). Match: BROOKHURST_HAVERHILL CT_PURDUE ST
# - INDIANWOOD_W. CDS_ROYAL MELBOURNE DR (too far in yard). Match: DARBY_HADLEY RD_BATES ST (also in yard)
# - LAUREL VALLEY_EAGLE TRACE DR_FOREST HIGHLAND DR S. (too far in yard). Match: BAILEY_GARNER RD_WALLER PL
# - HIGHWOODS_CAPITAL BLVD_GLENRIDGE DR (poles only on median, unclear if they are duke, difficult access). Match: ATLANTIC_NEW HOPE CHURCH RD_SINGLETON IND. DR
# - HAMPTON_BARSET PL_KIPAWA ST (poles too far into yards). Match: ROTHGEB_WEBB ST_WALDEN PL
# - LAKE_DIX ST_LAKE DR W. (no poles). Match: SLADE HILL_ROYAL OAKS DR_WEYBRIDGE DR
# - HEATHGATE_BROOKWAY CT_STAGWOOD LN (one pole in bushes). Match: FARMINGTON GROVE_FALLS RIVER AV_HALLBERG LN


m.out3.matches.random30 <- m.out3.matches %>% 
  filter(subclass %in% unique(random_selection$subclass))

table(m.out3.matches.random30$pole_est) 

table.random <- 
  tbl_summary(
    m.out3.matches.random30,
    include = c(COR_WIDTH, LANE_COUNT, UHI_Qtile_temp, pct_treec, hw_af_t_f, hw_af_hi_f,
                hw_am_hi_f, hw_am_t_f, hw_pm_hi_f, hw_pm_t_f, sidewalks, LCZ_desc, repave_grp),
    statistic = list(all_continuous() ~ "{mean}, ({sd})"),
    by = pot_treat, # split table by group
    missing = "no" # don't list missing data separately
  ) |> 
  add_n() |> # add column with total number of non-missing observations
  add_p(pvalue_fun = ~style_sigfig(., digits = 2)) |> # test for a difference between groups
  modify_header(label = "**Variable**") |> # update the column header
  bold_labels()

table.random
table.random |>
  as_gt() |> gt::gtsave("../Data/Results/02_group_equivalence_match3_randomsample_FINAL.docx")

# FINAL DATASET:
random_tomerge <- randomize_pairs %>%
  select(subclass, GlobalID_0, GlobalID_1, randomize) %>%
  pivot_longer(c(GlobalID_0, GlobalID_1), names_to="var", values_to = "GlobalID") %>%
  select(-var) %>%
  mutate(install.location = GlobalID %in% unique(m.out3.matches.random30$GlobalID)) 
    # 30 pairs, accounts for manual exclusions

final_matches <- match.data(m.out3) %>%
  left_join(random_tomerge, by=c("subclass", "GlobalID")) %>%
  rename(UHI_Qtile2 = UHI_Qtile_temp,
         select_loc = install.location) %>%
  mutate(GMaps_ID = paste(COR_STREET, COR_BEGIN_,COR_END_DE, sep="_"))

st_write(final_matches, "../Data/Analysis/02_Matched_Randomized_Street_Segments.shp", append=FALSE)

final_matches_df <- final_matches %>%
  st_drop_geometry()

write.csv(final_matches_df, "../Data/Analysis/02_Matched_Randomized_Street_Segments.csv", row.names = F)

# EXPORT LIST FOR RALEIGH
# matches_forraleigh <- final_matches_df %>%
#   filter(select_loc == TRUE) %>%
#   select(GlobalID, COR_STREET, COR_BEGIN_, COR_END_DE, yr_repave, pot_treat) %>%
#   rename(treatment_2025 = pot_treat) %>%
#   arrange(COR_STREET)
# 
# write.csv(matches_forraleigh, "../Data/Analysis/02_Matched_Randomized_Street_Segments_Raleigh.csv", row.names = F)


###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2. Generate Match Alternatives for REMOVED SENSORS ONLY ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

matches_orig <- read_csv("../Data/Analysis/02_Matched_Randomized_Street_Segments.csv") %>%
  filter(select_loc==TRUE & treat_25 == 0)

# 1st. Generate 2nd match, without replacement

# Remove the 30 original matches from the options 
matching_subset_2ndmatch <- matching_subset_repave %>% 
  filter(!GlobalID %in% unique(matches_orig$GlobalID))
  
m.out3.2ndmatch <- matchit(pot_treat ~ UHI_Qtile_temp + pct_treec + COR_WIDTH, # LANE_COUNT + hw_af_t_f + hw_am_t_f,
                    data = matching_subset_2ndmatch, method="nearest", distance="mahalanobis", exact = "repave_grp")

m.out3.2ndmatches <- match.data(m.out3.2ndmatch) %>% st_drop_geometry()

# Export 2nd match replacements:
# Montcastle - removed by city during Week 1 due to a neighbor complaint. Use 2nd match for Smith Basin (the treatment segment).

export_2ndmatches <- m.out3.2ndmatches %>%
  rename(UHI_Qtile2 = UHI_Qtile_temp) %>%
  mutate(GMaps_ID = paste(COR_STREET, COR_BEGIN_,COR_END_DE, sep="_"),
         select_loc = TRUE, 
         randomize = NA) %>%
  filter(subclass == 71)

write.csv(export_2ndmatches, "../Data/Analysis/03_Sensor_Replacement_Matches.csv", row.names=FALSE)

# 2nd. Match with replacement - weird results from this. can't see the actual matches. 
# m.out3.mwr <-matchit(pot_treat ~ UHI_Qtile_temp + pct_treec + COR_WIDTH, # LANE_COUNT + hw_af_t_f + hw_am_t_f,
#                      data = matching_subset_repave, method="nearest", distance="mahalanobis", exact = "repave_grp",
#                      replace=TRUE)
# 
# m.out3.mwrmatches <- match.data(m.out3.mwr) %>% st_drop_geometry()

