################################################################################
# Program Name: 06c_Sensor_Models_EventStudy.py
# Program Purpose: Run event study models by week using hourly and daily data
# Translated from: 06c_Sensor_Models_EventStudy.R
# Original Author: Katherine Burley Farr (kburley@ad.unc.edu)
# Affiliation: UNC Department of Public Policy, Data-Driven EnviroLab
################################################################################

import warnings
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.formula.api as smf
from statsmodels.stats.outliers_influence import variance_inflation_factor

# ------------------------------------------------------------------------------
# 0. PATH SETUP (per PATH RULE)
# ------------------------------------------------------------------------------
# PROJECT_ROOT assumes this script lives in a subfolder (e.g., /Code or /Scripts)
# one level below the project root, matching the R script's relative paths
# ("Data/Analysis/...") which are relative to PROJECT_ROOT.
PROJECT_ROOT = Path("/Users/siddhantchoudhary/Downloads/raleigh-cool-pavements-main")
DATA_DIR = PROJECT_ROOT / "Data"
ANALYSIS_DIR = DATA_DIR / "Analysis"  # READ-ONLY: original R output location
FIGURES_DIR_ORIG = ANALYSIS_DIR / "Figures"  # READ-ONLY reference; R wrote here originally

# All Python-generated outputs (CSVs, plots,  tables, models) go here instead.
PYTHON_OUTPUT_DIR = DATA_DIR / "Analysis" / "Python_Analysis_06c"
PYTHON_FIGURES_DIR = PYTHON_OUTPUT_DIR / "Figures"
PYTHON_OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
PYTHON_FIGURES_DIR.mkdir(parents=True, exist_ok=True)

# Input file paths (READ ONLY - original locations, unchanged)
DID_DATA_PATH = ANALYSIS_DIR / "05_Analysis_Data.csv"
DID_DATA_HOURLY_PATH = ANALYSIS_DIR / "05_Analysis_Data_Hourly.csv"
DID_DATA_DAILY_PATH = ANALYSIS_DIR / "05_Analysis_Data_Daily.csv"

# Output tracking for final summary
output_files_summary = []  # list of (path, row_count) tuples


def load_and_report_csv(path: Path, required_cols=None):
    """Load a CSV, print columns, and validate required columns exist."""
    if not path.exists():
        raise FileNotFoundError(f"Required input file not found: {path}")
    df = pd.read_csv(path)
    print(f"\n--- Loaded: {path} ---")
    print(f"Shape: {df.shape}")
    print(f"Columns: {df.columns.tolist()}")
    if required_cols is not None:
        missing = set(required_cols) - set(df.columns)
        if missing:
            raise ValueError(
                f"Missing required columns in {path.name}: {sorted(missing)}\n"
                f"Available: {df.columns.tolist()}"
            )
    return df


################################################################################
# 1. Bring in Data
################################################################################

# R: did_data <- read_csv("Data/Analysis/05_Analysis_Data.csv") %>%
#      filter(!month(date) %in% c(6))
# NOTE: did_data (20-min granularity) is loaded in the R script but NEVER
# actually used downstream in this file (only did_data_hourly and
# did_data_daily are used in event study models). Preserving the load exactly
# to match original script behavior / side effects (e.g., memory footprint,
# variable availability), per STATISTICAL REPLICATION rule.
did_data_required = ["date"]
did_data = load_and_report_csv(DID_DATA_PATH, required_cols=did_data_required)
did_data["date"] = pd.to_datetime(did_data["date"])
did_data = did_data[did_data["date"].dt.month != 6].copy()
print(f"did_data after excluding June: shape={did_data.shape}")

# R: did_data_hourly <- read_csv("Data/Analysis/05_Analysis_Data_Hourly.csv") %>%
#      filter(!month(date) %in% c(6))
did_data_hourly_required = ["date", "date_dt", "hour", "randomize", "street_name",
                             "Temperature", "treat_25", "treatment_date",
                             "treatment_week", "rh_pct", "precip_in_rdu_fill",
                             "sunny", "temp_f_rdu_fill", "sfc_sw_down_wgt",
                             "sfc_sw_down_wgt_l1", "pct_treec"]
did_data_hourly = load_and_report_csv(DID_DATA_HOURLY_PATH, required_cols=did_data_hourly_required)
did_data_hourly["date"] = pd.to_datetime(did_data_hourly["date"])
did_data_hourly = did_data_hourly[did_data_hourly["date"].dt.month != 6].copy()
print(f"did_data_hourly after excluding June: shape={did_data_hourly.shape}")

# R: did_data_daily <- read_csv("Data/Analysis/05_Analysis_Data_Daily.csv") %>%
#      filter(!month(date) %in% c(6))
did_data_daily_required = ["date", "date_dt", "randomize", "street_name",
                            "Temperature", "treat_25", "treatment_date",
                            "treatment_week", "time_unit", "rh_pct",
                            "precip_in_rdu_fill", "sunny_hours", "temp_f_rdu_fill",
                            "sfc_sw_down_wgt", "sfc_sw_down_wgt_l1", "pct_treec"]
did_data_daily = load_and_report_csv(DID_DATA_DAILY_PATH, required_cols=did_data_daily_required)
did_data_daily["date"] = pd.to_datetime(did_data_daily["date"])
did_data_daily = did_data_daily[did_data_daily["date"].dt.month != 6].copy()
print(f"did_data_daily after excluding June: shape={did_data_daily.shape}")


################################################################################
# 2. Event Study
################################################################################

# ------------------------------------------------------------------------------
# Get treatment weeks - starting from 6/28/2025 as the Saturday before data
# starts. This helps group the treatment days into cohorts - except for
# week 12 which still just has 4 sensors.
# ------------------------------------------------------------------------------

# R:
# did_data_hourly_es_1 <- did_data_hourly %>%
#   mutate(datetime = make_datetime(year = year(date), month = month(date),
#                                    day = day(date), hour = hour, min = 0,
#                                    sec = 0, tz = "Etc/GMT+4")) %>%
#   filter(randomize != 101) %>%
#   mutate(days_since_start = as.numeric(date_dt - make_date(2025,6,28))) %>%
#   mutate(wks_since_start = days_since_start/7) %>%
#   mutate(weeks_since_start = floor(wks_since_start)) %>%
#   select(-c(days_since_start, wks_since_start))

did_data_hourly_es_1 = did_data_hourly.copy()

# Construct 'datetime' from date components + hour, localized to Etc/GMT+4
# (POSIX "Etc/GMT+4" is a fixed-offset zone equal to UTC-4, i.e. EDT-like,
# with NO DST transitions - matching R's fixed-offset tz behavior exactly.)
did_data_hourly_es_1["date"] = pd.to_datetime(did_data_hourly_es_1["date"])
_dt_naive = pd.to_datetime(dict(
    year=did_data_hourly_es_1["date"].dt.year,
    month=did_data_hourly_es_1["date"].dt.month,
    day=did_data_hourly_es_1["date"].dt.day,
    hour=did_data_hourly_es_1["hour"],
    minute=0,
    second=0,
))
did_data_hourly_es_1["datetime"] = _dt_naive.dt.tz_localize("Etc/GMT+4")

did_data_hourly_es_1 = did_data_hourly_es_1[did_data_hourly_es_1["randomize"] != 101].copy()

did_data_hourly_es_1["date_dt"] = pd.to_datetime(did_data_hourly_es_1["date_dt"])
_treatment_epoch = pd.Timestamp(year=2025, month=6, day=28)
did_data_hourly_es_1["days_since_start"] = (
    did_data_hourly_es_1["date_dt"] - _treatment_epoch
).dt.days.astype(float)
did_data_hourly_es_1["wks_since_start"] = did_data_hourly_es_1["days_since_start"] / 7
did_data_hourly_es_1["weeks_since_start"] = np.floor(did_data_hourly_es_1["wks_since_start"])
did_data_hourly_es_1 = did_data_hourly_es_1.drop(columns=["days_since_start", "wks_since_start"])

print(f"did_data_hourly_es_1 shape after randomize!=101 filter and week calc: {did_data_hourly_es_1.shape}")

# R:
# treatment_weeks <- did_data_hourly_es_1 %>%
#   distinct(date, date_dt, weeks_since_start) %>%
#   filter(date %in% c("2025-09-12", "2025-09-25", "2025-10-07","2025-10-09",
#                       "2025-10-10", "2025-10-18", "2025-10-21","2025-10-22",
#                       "2025-10-23")) %>%
#   select(date_dt, weeks_since_start) %>%
#   rename(treatment_date = date_dt, treatment_week = weeks_since_start) %>%
#   mutate(reference_week = treatment_week - 1)

_distinct_dates = did_data_hourly_es_1.drop_duplicates(subset=["date", "date_dt", "weeks_since_start"])

_treatment_date_strs = [
    "2025-09-12", "2025-09-25", "2025-10-07", "2025-10-09",
    "2025-10-10", "2025-10-18", "2025-10-21", "2025-10-22",
    "2025-10-23",
]
_treatment_date_set = pd.to_datetime(_treatment_date_strs)

treatment_weeks = _distinct_dates[_distinct_dates["date"].isin(_treatment_date_set)].copy()
treatment_weeks = treatment_weeks[["date_dt", "weeks_since_start"]]
treatment_weeks = treatment_weeks.rename(columns={
    "date_dt": "treatment_date",
    "weeks_since_start": "treatment_week",
})
treatment_weeks["reference_week"] = treatment_weeks["treatment_week"] - 1

print(f"treatment_weeks shape: {treatment_weeks.shape}")
print(treatment_weeks.head())


################################################################################
# Event Study Models: Daily Data
################################################################################

# R:
# es_data_daily <- did_data_daily %>%
#   mutate(days_since_start = as.numeric(date_dt - make_date(2025,6,28))) %>%
#   mutate(wks_since_start = days_since_start/7) %>%
#   mutate(weeks_since_start = floor(wks_since_start)) %>%
#   select(-c(days_since_start, wks_since_start)) %>%
#   select(-c(time_unit, treatment_week)) %>%
#   left_join(treatment_weeks, by="treatment_date") %>%
#   mutate(time_unit = weeks_since_start - treatment_week) %>%
#   filter(time_unit>=-11 & time_unit <= 4) %>%
#   filter(randomize != 101) %>%
#   filter(treatment_week != weeks_since_start) %>%
#   mutate(time_unit = relevel(factor(time_unit), ref=11)) # NOTE: bug in R, see below

es_data_daily = did_data_daily.copy()
es_data_daily["date_dt"] = pd.to_datetime(es_data_daily["date_dt"])
es_data_daily["days_since_start"] = (
    es_data_daily["date_dt"] - _treatment_epoch
).dt.days.astype(float)
es_data_daily["wks_since_start"] = es_data_daily["days_since_start"] / 7
es_data_daily["weeks_since_start"] = np.floor(es_data_daily["wks_since_start"])
es_data_daily = es_data_daily.drop(columns=["days_since_start", "wks_since_start"])

# Drop original time_unit and treatment_week to re-derive from new treatment_weeks table
es_data_daily = es_data_daily.drop(columns=["time_unit", "treatment_week"])

# left_join(treatment_weeks, by="treatment_date")
es_data_daily["treatment_date"] = pd.to_datetime(es_data_daily["treatment_date"])
_pre_merge_n = len(es_data_daily)
es_data_daily = es_data_daily.merge(treatment_weeks, on="treatment_date", how="left")
_post_merge_n = len(es_data_daily)
_unmatched = es_data_daily["treatment_week"].isna().sum()
print(f"es_data_daily merge with treatment_weeks: rows before={_pre_merge_n}, after={_post_merge_n}, "
      f"unmatched treatment_date (NaN treatment_week)={_unmatched}")

es_data_daily["time_unit"] = es_data_daily["weeks_since_start"] - es_data_daily["treatment_week"]

es_data_daily = es_data_daily[
    (es_data_daily["time_unit"] >= -11) & (es_data_daily["time_unit"] <= 4)
].copy()
es_data_daily = es_data_daily[es_data_daily["randomize"] != 101].copy()
# exclude obs during week of treatment
es_data_daily = es_data_daily[
    es_data_daily["treatment_week"] != es_data_daily["weeks_since_start"]
].copy()

# R: mutate(time_unit = relevel(factor(time_unit), ref=11))
# TODO (preserve R bug exactly): The stated intent per the inline comment
# ("to make -1 the ref week") is to set the reference level to time_unit == -1.
# However, ref=11 in R's relevel() is interpreted as the *11th level* (a
# positional index into the sorted factor levels), NOT the value -1. Since
# time_unit ranges from -11 to 4 (16 distinct integer values sorted
# ascending: -11,-10,...,-1,0,1,2,3,4), the 11th level corresponds to
# time_unit == -1 in THIS case (index 11, 1-based, among 16 sorted levels:
# position 11 = -11 + 10 = -1). So here ref=11 coincidentally equals the
# -1 category. Preserving this exact positional-index approach; DO NOT
# "fix" it to always dynamically find -1, since it must byte-for-byte match
# original R output. If the underlying data / week range changes, this will
# silently break in the same way it would in R.
_time_unit_levels_daily = sorted(es_data_daily["time_unit"].dropna().unique().tolist())
if len(_time_unit_levels_daily) >= 11:
    _ref_level_daily = _time_unit_levels_daily[10]  # 11th level, 0-indexed as [10]
else:
    raise ValueError(
        f"es_data_daily: fewer than 11 distinct time_unit levels found "
        f"({len(_time_unit_levels_daily)}); cannot replicate R's relevel(ref=11)."
    )
print(f"es_data_daily time_unit levels: {_time_unit_levels_daily}")
print(f"es_data_daily reference level (R relevel ref=11, positional): {_ref_level_daily}")

es_data_daily["time_unit"] = pd.Categorical(
    es_data_daily["time_unit"], categories=_time_unit_levels_daily
)
# Reorder categories so the reference level comes first (statsmodels/patsy
# C() treatment coding uses the FIRST category as reference by default,
# analogous to R's relevel() moving ref to front).
_reordered_daily = [_ref_level_daily] + [
    lvl for lvl in _time_unit_levels_daily if lvl != _ref_level_daily
]
es_data_daily["time_unit"] = es_data_daily["time_unit"].cat.reorder_categories(_reordered_daily)

print(f"es_data_daily final shape: {es_data_daily.shape}")

# R:
# es_testing_d <- lm(Temperature ~ treat_25 + factor(time_unit) +
#                     treat_25:factor(time_unit) + rh_pct + precip_in_rdu_fill +
#                     sunny_hours + temp_f_rdu_fill + sfc_sw_down_wgt +
#                     sfc_sw_down_wgt_l1 + pct_treec, data=es_data_daily)

_required_daily_model_cols = [
    "Temperature", "treat_25", "time_unit", "rh_pct", "precip_in_rdu_fill",
    "sunny_hours", "temp_f_rdu_fill", "sfc_sw_down_wgt", "sfc_sw_down_wgt_l1",
    "pct_treec", "street_name",
]
_missing = set(_required_daily_model_cols) - set(es_data_daily.columns)
if _missing:
    raise ValueError(
        f"Missing columns required for es_testing_d model: {sorted(_missing)}\n"
        f"Available: {es_data_daily.columns.tolist()}"
    )

es_testing_d_formula = (
    "Temperature ~ treat_25 + C(time_unit) + treat_25:C(time_unit) + "
    "rh_pct + precip_in_rdu_fill + sunny_hours + temp_f_rdu_fill + "
    "sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + pct_treec"
)
# Match R behavior: remove rows used by model before clustered SE
model_vars_daily = [
    "Temperature",
    "treat_25",
    "time_unit",
    "rh_pct",
    "precip_in_rdu_fill",
    "sunny_hours",
    "temp_f_rdu_fill",
    "sfc_sw_down_wgt",
    "sfc_sw_down_wgt_l1",
    "pct_treec",
    "street_name"
]

es_data_daily_model = es_data_daily.dropna(
    subset=model_vars_daily
).copy()

print(
    "Rows before model NA removal:",
    len(es_data_daily)
)
print(
    "Rows after model NA removal:",
    len(es_data_daily_model)
)

es_testing_d = smf.ols(
    formula=es_testing_d_formula,
    data=es_data_daily_model
).fit(
    cov_type="cluster",
    cov_kwds={
        "groups": es_data_daily_model["street_name"]
    }
)

print("\n--- es_testing_d model summary (clustered SE by street_name) ---")
print(es_testing_d.summary())

# VIF check (R: vif(es_testing_d))
# NOTE: R's vif() on an lm object with a factor interaction typically computes
# GVIF for term-groups. Here we approximate with per-coefficient VIF on the
# model's design matrix, which is the closest direct statsmodels equivalent.
try:
    _exog_d = es_testing_d.model.exog
    _exog_names_d = es_testing_d.model.exog_names
    vif_data_d = pd.DataFrame({
        "Variable": _exog_names_d,
        "VIF": [variance_inflation_factor(_exog_d, i) for i in range(_exog_d.shape[1])],
    })
    print("\n--- VIF (es_testing_d) ---")
    print(vif_data_d)
except Exception as e:
    print(f"VIF calculation failed for es_testing_d: {e}")
    vif_data_d = pd.DataFrame()

# Extract coefficients & CIs (R: coef(), coefci() with vcovCL clustered SE
# already baked into the statsmodels fit via cov_type='cluster' above)
coeffs_es_d = es_testing_d.params
ci_es_d = es_testing_d.conf_int(alpha=0.05)  # columns [0]=2.5%, [1]=97.5%

es_d_results = pd.DataFrame({
    "Term": coeffs_es_d.index,
    "Estimate": coeffs_es_d.values,
    "CI_2.5": ci_es_d[0].values,
    "CI_97.5": ci_es_d[1].values,
})

# R: filter(grepl("time_unit", Term)) %>%
#    separate(Term, into=c("Var","Weeks"), sep="\\(time_unit\\)") %>%
#    mutate(Var = case_when(Var=="factor"~"Weeks", Var=="treat_25:factor"~"Treatment*Weeks"),
#           Weeks = as.numeric(Weeks))
es_d_results = es_d_results[es_d_results["Term"].str.contains("time_unit", na=False)].copy()

# statsmodels/patsy term names look like: C(time_unit)[T.-10] or
# treat_25:C(time_unit)[T.-10]. Split analogous to R's separate() on the
# literal string "(time_unit)".
_split_d = es_d_results["Term"].str.split(r"\(time_unit\)", n=1, expand=True, regex=True)
es_d_results["Var"] = _split_d[0]
es_d_results["Weeks"] = _split_d[1]

# Weeks currently looks like "[T.-10]" -> strip to numeric
es_d_results["Weeks"] = (
    es_d_results["Weeks"]
    .str.replace(r"\[T\.", "", regex=True)
    .str.replace(r"\]", "", regex=True)
)

_var_choices_d = [
    es_d_results["Var"] == "C",
    es_d_results["Var"] == "treat_25:C",
]
_var_labels_d = ["Weeks", "Treatment*Weeks"]
es_d_results["Var"] = np.select(_var_choices_d, _var_labels_d, default=None)

es_d_results["Weeks"] = pd.to_numeric(es_d_results["Weeks"], errors="coerce")

print("\n--- es_d_results (time_unit terms) ---")
print(es_d_results)

_es_d_out_path = PYTHON_OUTPUT_DIR / "es_d_results.csv"
print(f"Writing: {_es_d_out_path}")
if _es_d_out_path.exists():
    warnings.warn(f"Output file already exists and will be overwritten check skipped per pipeline "
                   f"convention: {_es_d_out_path}")
es_d_results.to_csv(_es_d_out_path, index=False)
output_files_summary.append((_es_d_out_path, len(es_d_results)))

# R: coefplot(es_testing_d, keep="^treat_25:") -- native lm coefplot, not
# clustered SE (per R comment: "these coefplots also do not seem to be using
# clustered std errors"). We replicate with matplotlib below using the model
# results already computed (clustered SEs from es_testing_d, which R's
# separate coefplot lines partially override with vcovCL args). We follow the
# clustered-SE version (line 126/129 in R) since that is the one used in the
# final ggplot figures (es_d_t / es_d_w).

################################################################################
# Daily Event Study Plots
################################################################################

_es_d_t_data = es_d_results[es_d_results["Var"] == "Treatment*Weeks"].sort_values("Weeks")
_es_d_w_data = es_d_results[es_d_results["Var"] == "Weeks"].sort_values("Weeks")

fig, axes = plt.subplots(2, 1, figsize=(10, 8))

# es_d_t: Treatment*Weeks panel
ax = axes[0]
ax.errorbar(
    _es_d_t_data["Weeks"], _es_d_t_data["Estimate"],
    yerr=[
        _es_d_t_data["Estimate"] - _es_d_t_data["CI_2.5"],
        _es_d_t_data["CI_97.5"] - _es_d_t_data["Estimate"],
    ],
    fmt="o", color="#004351", ecolor="#166a7c", elinewidth=1.5, capsize=3, markersize=7,
)
ax.set_title("Event Study Model - Daily Data")
ax.set_xlabel("Treat*Weeks Before Treatment")
ax.set_ylabel("Estimate (Degrees F)")
ax.axhline(y=0, color="black", linestyle="--", linewidth=1)
ax.axvline(x=0, color="#EF446F", linestyle="--", linewidth=1)
ax.set_xticks(np.arange(-11, 5, 1))

# es_d_w: Weeks panel
ax = axes[1]
ax.errorbar(
    _es_d_w_data["Weeks"], _es_d_w_data["Estimate"],
    yerr=[
        _es_d_w_data["Estimate"] - _es_d_w_data["CI_2.5"],
        _es_d_w_data["CI_97.5"] - _es_d_w_data["Estimate"],
    ],
    fmt="o", color="#004351", ecolor="#166a7c", elinewidth=1.5, capsize=3, markersize=7,
)
ax.set_title("")
ax.set_xlabel("Weeks Before Treatment")
ax.set_ylabel("Estimate (Degrees F)")
ax.axhline(y=0, color="black", linestyle="--", linewidth=1)
ax.set_xticks(np.arange(-11, 4, 1))

plt.tight_layout()

_fig1_path = PYTHON_FIGURES_DIR / "Event_Study_Daily_Temp.png"
print(f"Writing figure: {_fig1_path}")
fig.savefig(_fig1_path, dpi=300)
plt.close(fig)
output_files_summary.append((_fig1_path, len(_es_d_t_data) + len(_es_d_w_data)))

# Keep references for later combined plot (COMBINE DAILY AND HOURLY RESULTS)
es_d_t_data_for_combo = _es_d_t_data


################################################################################
# Event Study Models: Hourly Data
################################################################################

# R:
# did_data_hourly_es <- did_data_hourly_es_1 %>%
#   select(-c(treatment_week)) %>%
#   left_join(treatment_weeks, by="treatment_date") %>%
#   mutate(reference_week = treatment_week - 1) %>%
#   filter(treatment_week != weeks_since_start) %>%
#   mutate(time_unit = weeks_since_start - treatment_week)

did_data_hourly_es = did_data_hourly_es_1.copy()

if "treatment_week" not in did_data_hourly_es.columns:
    raise ValueError(
        "Column 'treatment_week' expected in did_data_hourly_es_1 before drop; not found. "
        f"Available: {did_data_hourly_es.columns.tolist()}"
    )
did_data_hourly_es = did_data_hourly_es.drop(columns=["treatment_week"])

did_data_hourly_es["treatment_date"] = pd.to_datetime(did_data_hourly_es["treatment_date"])
_pre_merge_n2 = len(did_data_hourly_es)
did_data_hourly_es = did_data_hourly_es.merge(treatment_weeks, on="treatment_date", how="left")
_post_merge_n2 = len(did_data_hourly_es)
_unmatched2 = did_data_hourly_es["treatment_week"].isna().sum()
print(f"did_data_hourly_es merge with treatment_weeks: rows before={_pre_merge_n2}, "
      f"after={_post_merge_n2}, unmatched treatment_date (NaN treatment_week)={_unmatched2}")

# NOTE: 'reference_week' is overwritten here (already existed on treatment_weeks
# from the join, R re-mutates it identically: treatment_week - 1).
did_data_hourly_es["reference_week"] = did_data_hourly_es["treatment_week"] - 1

# exclude week of treatment (week 0)
did_data_hourly_es = did_data_hourly_es[
    did_data_hourly_es["treatment_week"] != did_data_hourly_es["weeks_since_start"]
].copy()

did_data_hourly_es["time_unit"] = (
    did_data_hourly_es["weeks_since_start"] - did_data_hourly_es["treatment_week"]
)

print(f"did_data_hourly_es shape: {did_data_hourly_es.shape}")

# R:
# es_data <- did_data_hourly_es %>%
#   filter(time_unit >= -11 & time_unit <= 4) %>%
#   mutate(time_unit_day = date - treatment_date) %>%
#   mutate(time_unit_fct = relevel(factor(time_unit), ref=11))

es_data = did_data_hourly_es[
    (did_data_hourly_es["time_unit"] >= -11) & (did_data_hourly_es["time_unit"] <= 4)
].copy()

es_data["date"] = pd.to_datetime(es_data["date"])
es_data["treatment_date"] = pd.to_datetime(es_data["treatment_date"])
es_data["time_unit_day"] = (es_data["date"] - es_data["treatment_date"]).dt.days

# R: relevel(factor(time_unit), ref=11) -- same positional-index caveat as
# above (see TODO on es_data_daily). Preserve exactly: 11th sorted level.
_time_unit_levels_hourly = sorted(es_data["time_unit"].dropna().unique().tolist())
if len(_time_unit_levels_hourly) >= 11:
    _ref_level_hourly = _time_unit_levels_hourly[10]
else:
    raise ValueError(
        f"es_data: fewer than 11 distinct time_unit levels found "
        f"({len(_time_unit_levels_hourly)}); cannot replicate R's relevel(ref=11)."
    )
print(f"es_data time_unit levels: {_time_unit_levels_hourly}")
print(f"es_data reference level (R relevel ref=11, positional): {_ref_level_hourly}")

es_data["time_unit_fct"] = pd.Categorical(
    es_data["time_unit"], categories=_time_unit_levels_hourly
)
_reordered_hourly = [_ref_level_hourly] + [
    lvl for lvl in _time_unit_levels_hourly if lvl != _ref_level_hourly
]
es_data["time_unit_fct"] = es_data["time_unit_fct"].cat.reorder_categories(_reordered_hourly)

print(f"es_data final shape: {es_data.shape}")

# R:
# es_testing <- lm(Temperature ~ treat_25 + factor(time_unit_fct) +
#                   treat_25:factor(time_unit_fct) + factor(hour) + rh_pct +
#                   precip_in_rdu_fill + sunny + temp_f_rdu_fill +
#                   sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + pct_treec, data=es_data)

_required_hourly_model_cols = [
    "Temperature", "treat_25", "time_unit_fct", "hour", "rh_pct",
    "precip_in_rdu_fill", "sunny", "temp_f_rdu_fill", "sfc_sw_down_wgt",
    "sfc_sw_down_wgt_l1", "pct_treec", "street_name",
]
_missing2 = set(_required_hourly_model_cols) - set(es_data.columns)
if _missing2:
    raise ValueError(
        f"Missing columns required for es_testing model: {sorted(_missing2)}\n"
        f"Available: {es_data.columns.tolist()}"
    )

es_testing_formula = (
    "Temperature ~ treat_25 + C(time_unit_fct) + treat_25:C(time_unit_fct) + "
    "C(hour) + rh_pct + precip_in_rdu_fill + sunny + temp_f_rdu_fill + "
    "sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + pct_treec"
)
# Match R behavior: remove rows used by model before clustered SE
model_vars_hourly = [
    "Temperature",
    "treat_25",
    "time_unit_fct",
    "hour",
    "rh_pct",
    "precip_in_rdu_fill",
    "sunny",
    "temp_f_rdu_fill",
    "sfc_sw_down_wgt",
    "sfc_sw_down_wgt_l1",
    "pct_treec",
    "street_name"
]

es_data_model = es_data.dropna(subset=model_vars_hourly).copy()

print(
    "Hourly rows before NA removal:",
    len(es_data)
)

print(
    "Hourly rows after NA removal:",
    len(es_data_model)
)

es_testing = smf.ols(
    formula=es_testing_formula,
    data=es_data_model
).fit(
    cov_type="cluster",
    cov_kwds={
        "groups": es_data_model["street_name"]
    }
)

# VIF (R: vif(es_testing) called before summary in source order)
try:
    _exog_h = es_testing.model.exog
    _exog_names_h = es_testing.model.exog_names
    vif_data_h = pd.DataFrame({
        "Variable": _exog_names_h,
        "VIF": [variance_inflation_factor(_exog_h, i) for i in range(_exog_h.shape[1])],
    })
    print("\n--- VIF (es_testing) ---")
    print(vif_data_h)
except Exception as e:
    print(f"VIF calculation failed for es_testing: {e}")
    vif_data_h = pd.DataFrame()

print("\n--- es_testing model summary (clustered SE by street_name) ---")
print(es_testing.summary())

# Extract coefficients & CIs
coeffs_es_h = es_testing.params
ci_es_h = es_testing.conf_int(alpha=0.05)

es_h_results = pd.DataFrame({
    "Term": coeffs_es_h.index,
    "Estimate": coeffs_es_h.values,
    "CI_2.5": ci_es_h[0].values,
    "CI_97.5": ci_es_h[1].values,
})

# R: filter(grepl("time_unit_fct", Term)) %>%
#    separate(Term, into=c("Var","Weeks"), sep="\\(time_unit_fct\\)") %>%
#    mutate(Var = case_when(Var=="factor"~"Weeks", Var=="treat_25:factor"~"Treatment*Weeks"),
#           Weeks = as.numeric(Weeks))
es_h_results = es_h_results[es_h_results["Term"].str.contains("time_unit_fct", na=False)].copy()

_split_h = es_h_results["Term"].str.split(r"\(time_unit_fct\)", n=1, expand=True, regex=True)
es_h_results["Var"] = _split_h[0]
es_h_results["Weeks"] = _split_h[1]

es_h_results["Weeks"] = (
    es_h_results["Weeks"]
    .str.replace(r"\[T\.", "", regex=True)
    .str.replace(r"\]", "", regex=True)
)

_var_choices_h = [
    es_h_results["Var"] == "C",
    es_h_results["Var"] == "treat_25:C",
]
_var_labels_h = ["Weeks", "Treatment*Weeks"]
es_h_results["Var"] = np.select(_var_choices_h, _var_labels_h, default=None)

es_h_results["Weeks"] = pd.to_numeric(es_h_results["Weeks"], errors="coerce")

print("\n--- es_h_results (time_unit_fct terms) ---")
print(es_h_results)

_es_h_out_path = PYTHON_OUTPUT_DIR / "es_h_results.csv"
print(f"Writing: {_es_h_out_path}")
if _es_h_out_path.exists():
    warnings.warn(f"Output file already exists and will be overwritten check skipped per pipeline "
                   f"convention: {_es_h_out_path}")
es_h_results.to_csv(_es_h_out_path, index=False)
output_files_summary.append((_es_h_out_path, len(es_h_results)))


################################################################################
# Hourly Event Study Plots
################################################################################

_es_h_t_data = es_h_results[es_h_results["Var"] == "Treatment*Weeks"].sort_values("Weeks")

fig, ax = plt.subplots(figsize=(10, 8))
ax.errorbar(
    _es_h_t_data["Weeks"], _es_h_t_data["Estimate"],
    yerr=[
        _es_h_t_data["Estimate"] - _es_h_t_data["CI_2.5"],
        _es_h_t_data["CI_97.5"] - _es_h_t_data["Estimate"],
    ],
    fmt="o", color="#004351", ecolor="#166a7c", elinewidth=1.5, capsize=3, markersize=7,
)
ax.set_title("Event Study Model - Hourly Data")
ax.set_xlabel("Treat*Weeks Before Treatment")
ax.set_ylabel("Estimate (Degrees F)")
ax.axhline(y=0, color="black", linestyle="--", linewidth=1)
ax.axvline(x=0, color="#EF446F", linestyle="--", linewidth=1)
ax.set_xticks(np.arange(-11, 5, 1))
plt.tight_layout()

_fig2_path = PYTHON_FIGURES_DIR / "Event_Study_Hourly_Temp_TreatPostOnly.png"
print(f"Writing figure: {_fig2_path}")
fig.savefig(_fig2_path, dpi=300)
plt.close(fig)
output_files_summary.append((_fig2_path, len(_es_h_t_data)))

es_h_t_data_for_combo = _es_h_t_data

_es_h_w_data = es_h_results[es_h_results["Var"] == "Weeks"].sort_values("Weeks")

fig, axes = plt.subplots(2, 1, figsize=(10, 8))

ax = axes[0]
ax.errorbar(
    _es_h_t_data["Weeks"], _es_h_t_data["Estimate"],
    yerr=[
        _es_h_t_data["Estimate"] - _es_h_t_data["CI_2.5"],
        _es_h_t_data["CI_97.5"] - _es_h_t_data["Estimate"],
    ],
    fmt="o", color="#004351", ecolor="#166a7c", elinewidth=1.5, capsize=3, markersize=7,
)
ax.set_title("Event Study Model - Hourly Data")
ax.set_xlabel("Treat*Weeks Before Treatment")
ax.set_ylabel("Estimate (Degrees F)")
ax.axhline(y=0, color="black", linestyle="--", linewidth=1)
ax.axvline(x=0, color="#EF446F", linestyle="--", linewidth=1)
ax.set_xticks(np.arange(-11, 5, 1))

ax = axes[1]
ax.errorbar(
    _es_h_w_data["Weeks"], _es_h_w_data["Estimate"],
    yerr=[
        _es_h_w_data["Estimate"] - _es_h_w_data["CI_2.5"],
        _es_h_w_data["CI_97.5"] - _es_h_w_data["Estimate"],
    ],
    fmt="o", color="#004351", ecolor="#166a7c", elinewidth=1.5, capsize=3, markersize=7,
)
ax.set_title("")
ax.set_xlabel("Weeks Before Treatment")
ax.set_ylabel("Estimate (Degrees F)")
ax.axhline(y=0, color="black", linestyle="--", linewidth=1)
ax.set_xticks(np.arange(-11, 5, 1))

plt.tight_layout()

_fig3_path = PYTHON_FIGURES_DIR / "Event_Study_Hourly_Temp.png"
print(f"Writing figure: {_fig3_path}")
fig.savefig(_fig3_path, dpi=300)
plt.close(fig)
output_files_summary.append((_fig3_path, len(_es_h_t_data) + len(_es_h_w_data)))


################################################################################
# COMBINE DAILY AND HOURLY RESULTS
################################################################################

# R: ggarrange(es_d_t, es_h_t, ncol=1); ggsave("Event_Study_Hourly_Daily.png")
fig, axes = plt.subplots(2, 1, figsize=(10, 8))

ax = axes[0]
ax.errorbar(
    es_d_t_data_for_combo["Weeks"], es_d_t_data_for_combo["Estimate"],
    yerr=[
        es_d_t_data_for_combo["Estimate"] - es_d_t_data_for_combo["CI_2.5"],
        es_d_t_data_for_combo["CI_97.5"] - es_d_t_data_for_combo["Estimate"],
    ],
    fmt="o", color="#004351", ecolor="#166a7c", elinewidth=1.5, capsize=3, markersize=7,
)
ax.set_title("Event Study Model - Daily Data")
ax.set_xlabel("Treat*Weeks Before Treatment")
ax.set_ylabel("Estimate (Degrees F)")
ax.axhline(y=0, color="black", linestyle="--", linewidth=1)
ax.axvline(x=0, color="#EF446F", linestyle="--", linewidth=1)
ax.set_xticks(np.arange(-11, 5, 1))

ax = axes[1]
ax.errorbar(
    es_h_t_data_for_combo["Weeks"], es_h_t_data_for_combo["Estimate"],
    yerr=[
        es_h_t_data_for_combo["Estimate"] - es_h_t_data_for_combo["CI_2.5"],
        es_h_t_data_for_combo["CI_97.5"] - es_h_t_data_for_combo["Estimate"],
    ],
    fmt="o", color="#004351", ecolor="#166a7c", elinewidth=1.5, capsize=3, markersize=7,
)
ax.set_title("Event Study Model - Hourly Data")
ax.set_xlabel("Treat*Weeks Before Treatment")
ax.set_ylabel("Estimate (Degrees F)")
ax.axhline(y=0, color="black", linestyle="--", linewidth=1)
ax.axvline(x=0, color="#EF446F", linestyle="--", linewidth=1)
ax.set_xticks(np.arange(-11, 5, 1))

plt.tight_layout()

_fig4_path = PYTHON_FIGURES_DIR / "Event_Study_Hourly_Daily.png"
print(f"Writing figure: {_fig4_path}")
fig.savefig(_fig4_path, dpi=300)
plt.close(fig)
output_files_summary.append((_fig4_path, len(es_d_t_data_for_combo) + len(es_h_t_data_for_combo)))


################################################################################
# VALIDATION / FINAL SUMMARY
################################################################################

print("\n" + "=" * 80)
print("VALIDATION SUMMARY")
print("=" * 80)

print("\n--- es_data_daily ---")
print(f"Shape: {es_data_daily.shape}")
print(f"Columns: {es_data_daily.columns.tolist()}")
print(es_data_daily.head())
print(f"Unique street_name count: {es_data_daily['street_name'].nunique()}")
print(f"Date range (date_dt): {es_data_daily['date_dt'].min()} to {es_data_daily['date_dt'].max()}")
print(f"time_unit value counts:\n{es_data_daily['time_unit'].value_counts().sort_index()}")

print("\n--- es_data (hourly) ---")
print(f"Shape: {es_data.shape}")
print(f"Columns: {es_data.columns.tolist()}")
print(es_data.head())
print(f"Unique street_name count: {es_data['street_name'].nunique()}")
print(f"Date range (date): {es_data['date'].min()} to {es_data['date'].max()}")
print(f"time_unit_fct value counts:\n{es_data['time_unit_fct'].value_counts().sort_index()}")

print("\n--- es_d_results ---")
print(f"Shape: {es_d_results.shape}")
print(es_d_results.describe(include="all"))

print("\n--- es_h_results ---")
print(f"Shape: {es_h_results.shape}")
print(es_h_results.describe(include="all"))

print("\n--- Model Summary Statistics ---")
print(f"es_testing_d: R-squared={es_testing_d.rsquared:.4f}, N={int(es_testing_d.nobs)}")
print(f"es_testing:   R-squared={es_testing.rsquared:.4f}, N={int(es_testing.nobs)}")

print("\n" + "=" * 80)
print("OUTPUT FILES SUMMARY")
print("=" * 80)
for _path, _n in output_files_summary:
    print(f"{_path}  |  rows/points={_n}")

################################################################################
# ASSUMPTIONS / NOTES
################################################################################
# 1. PROJECT_ROOT is assumed to be two directory levels above this script
#    file (i.e., this script lives in a subfolder such as /Code or /Scripts
#    directly under PROJECT_ROOT, matching the R script's relative path
#    "Data/Analysis/..." from PROJECT_ROOT). Adjust if actual repo layout
#    differs.
# 2. "Etc/GMT+4" is a fixed-offset (no-DST) timezone equal to UTC-4,
#    replicated via tz_localize (not tz_convert, since data is naive local
#    time being labeled, not converted from another zone) to match R's
#    make_datetime(tz=...) behavior exactly.
# 3. R's relevel(factor(time_unit), ref=11) uses ref as a POSITIONAL index
#    (11th sorted factor level), not the literal value -1, despite the
#    inline comment saying "to make -1 the ref week". This positional
#    approach is preserved exactly (see TODO comments above) rather than
#    "corrected" to always select value == -1, per STATISTICAL REPLICATION
#    rule. Given the observed time_unit range (-11 to 4, 16 levels), the
#    11th level does resolve to -1, but this is coincidental to the data
#    range and not guaranteed by the code logic itself.
# 4. did_data (from 05_Analysis_Data.csv, 20-min granularity) is loaded and
#    filtered exactly as in the R script but is not used further downstream
#    in this file -- preserved as-is as dead code / original side effect.
# 5. R's vif() on an lm with factor+interaction terms computes GVIF via
#    car::vif(), which differs from per-column VIF via
#    variance_inflation_factor() on the raw design matrix used here. This is
#    the closest available equivalent within the approved dependency list
#    (statsmodels) and is presented as a diagnostic aid, not a byte-for-byte
#    replication of car::vif() output.
# 6. R's coefplot() calls (lines 125-129 in source) are diagnostic/interactive
#    plotting calls not assigned to variables or saved to any output file in
#    the R script; they were not replicated as separate output artifacts.
#    Only the ggsave()-persisted ggplot figures (es_d_t/es_d_w combined,
#    es_h_t alone, es_h_t/es_h_w combined, and the daily+hourly combo) are
#    replicated as saved PNGs here.
# 7. Original R figure output paths ("Data/Analysis/Figures/...") are
#    READ-ONLY / left untouched; all Python-generated figures are written to
#    PYTHON_FIGURES_DIR under Python_Analysis instead, per PATH RULE.
# 8. Column existence for "sunny_hours" (daily model) vs "sunny" (hourly
#    model) is asserted separately per the original R formulas, which use
#    different variable names between the daily and hourly models exactly as
#    written in the source.
################################################################################