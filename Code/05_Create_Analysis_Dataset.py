################################################################################
# Program Name: 05_Create_Analysis_Dataset.py
# Program Purpose: create analysis datasets for cool pavement models
# Translated from: 05_Create_Analysis_Dataset.R (Katherine Burley Farr, UNC DDL)
################################################################################

from pathlib import Path

import numpy as np
import pandas as pd
import geopandas as gpd

# ---------------------------------------------------------------------------
# 0. Paths & helpers
# ---------------------------------------------------------------------------

# TODO: Original R script hardcodes an absolute, machine-specific path for the
# sensor CSV directory:
#   "/Users/siddhantchoudhary/Downloads/raleigh-cool-pavements-main/Data/2025 Sensor Data Collection/CleanData"
# PROJECT_ROOT below assumes this script lives in a "Code" folder sibling to
# "Data", matching the R script's relative paths ("../Data/..."). Verify this
# resolves correctly in the target environment.
PROJECT_ROOT = Path("/Users/siddhantchoudhary/Downloads/raleigh-cool-pavements-main")
DATA_DIR = PROJECT_ROOT / "Data"
ANALYSIS_DIR = DATA_DIR / "Analysis"
PYTHON_ANALYSIS_DIR = ANALYSIS_DIR / "Python_Analysis_05"
PYTHON_ANALYSIS_DIR.mkdir(exist_ok=True)
ORIGINAL_DIR = DATA_DIR / "Original"
SENSOR_DATA_DIR = DATA_DIR / "2026 Sensor Data Collection" / "CleanData"


def require_columns(df: pd.DataFrame, required, context: str) -> None:
    """Raise a descriptive error if any required column is absent."""
    missing = set(required) - set(df.columns)
    if missing:
        raise ValueError(
            f"Missing required columns in {context}: {sorted(missing)}\n"
            f"Available: {df.columns.tolist()}"
        )


def log_and_merge(
    left: pd.DataFrame,
    right: pd.DataFrame,
    on,
    label: str,
    how: str = "left",
    suffixes=("_x", "_y"),
) -> pd.DataFrame:
    """left_join with row-count / unmatched-key logging to prevent silent
    row duplication or unexpected drops."""
    print(f"[MERGE:{label}] left rows={len(left)} right rows={len(right)} on={on} how={how}")
    merged = left.merge(right, on=on, how=how, indicator=True, suffixes=suffixes)
    unmatched_left = int((merged["_merge"] == "left_only").sum())
    print(f"[MERGE:{label}] result rows={len(merged)} unmatched_left_only={unmatched_left}")
    return merged.drop(columns="_merge")


def lubridate_week(dt_series: pd.Series) -> pd.Series:
    """
    Replicates R lubridate::week(): "no. of complete seven day periods that
    have occurred between the date and January 1st, plus one."
    week = floor((day_of_year - 1) / 7) + 1
    NOTE: this is NOT ISO week (isoweek), which uses Monday-anchored weeks.
    """
    dt_series = pd.to_datetime(dt_series)
    day_of_year = dt_series.dt.dayofyear
    return ((day_of_year - 1) // 7 + 1).astype("Int64")


def dewpoint_to_humidity_fahrenheit(temp_f: pd.Series, dewpoint_f: pd.Series) -> pd.Series:
    """
    Replicates weathermetrics::dewpoint.to.humidity(temperature.metric="fahrenheit"),
    which converts F -> C then applies the Magnus-formula RH approximation:
        RH = 100 * exp(17.625*Td / (243.04+Td)) / exp(17.625*T / (243.04+T))
    """
    t_c = (temp_f - 32) * 5.0 / 9.0
    dp_c = (dewpoint_f - 32) * 5.0 / 9.0
    return 100 * (
        np.exp((17.625 * dp_c) / (243.04 + dp_c))
        / np.exp((17.625 * t_c) / (243.04 + t_c))
    )


###############################################################################
# 1. Sensor Data
###############################################################################

# NOTE: R pattern "^Sensor_Data_Week.*\\.csv$" is an anchored regex on the
# basename. pathlib has no arbitrary-regex glob, so "Sensor_Data_Week*.csv" is
# used as the closest equivalent. Flag if source filenames deviate from this.
clean_csvs = sorted(SENSOR_DATA_DIR.rglob("Sensor_Data_Week*.csv"))
if not clean_csvs:
    raise ValueError(
        f"No sensor CSV files found in {SENSOR_DATA_DIR} matching 'Sensor_Data_Week*.csv'"
    )

frames = []
for f in clean_csvs:
    df_part = pd.read_csv(f)
    print(f"[LOAD] {f.name} columns: {df_part.columns.tolist()}")

    # R's read.csv() converts illegal column-name characters (e.g. spaces) to
    # dots via make.names(), so a source header of "Relative Humidity" becomes
    # "Relative.Humidity" in R. pandas does not do this automatically.
    if "Relative.Humidity" not in df_part.columns:
        if "Relative Humidity" in df_part.columns:
            df_part = df_part.rename(columns={"Relative Humidity": "Relative.Humidity"})
        else:
            raise ValueError(
                f"Expected 'Relative.Humidity' (or 'Relative Humidity') not found in {f}. "
                f"Available: {df_part.columns.tolist()}"
            )

    require_columns(
        df_part, ["sensor_id", "datetime", "Temperature", "Relative.Humidity"], str(f)
    )
    df_part["Temperature"] = pd.to_numeric(df_part["Temperature"], errors="coerce")
    df_part["Relative.Humidity"] = pd.to_numeric(df_part["Relative.Humidity"], errors="coerce")
    df_part["filename"] = f.name
    frames.append(df_part)

combined_data_orig = pd.concat(frames, ignore_index=True)
# dplyr::distinct(sensor_id, datetime, Temperature, Relative.Humidity) with
# default keep_all=FALSE keeps ONLY the listed columns in the output.
combined_data_orig = (
    combined_data_orig[["sensor_id", "datetime", "Temperature", "Relative.Humidity"]]
    .drop_duplicates()
)

combined_data = (
    combined_data_orig.groupby(["sensor_id", "datetime"], as_index=False)
    .agg({"Temperature": "mean", "Relative.Humidity": "mean"})  # handles a few dups
)
combined_data["datetime"] = pd.to_datetime(combined_data["datetime"])
combined_data["hour"] = combined_data["datetime"].dt.hour.astype(float)
combined_data["date_dt"] = pd.to_datetime(combined_data["datetime"].dt.date)
combined_data["week"] = lubridate_week(combined_data["date_dt"])

del combined_data_orig

# --- Street Characteristics Data to Merge --------------------------------
street_chars_path = ANALYSIS_DIR / "03_Sensor_Locations_Characteristics_All.csv"
street_chars = pd.read_csv(street_chars_path)
print(f"[LOAD] {street_chars_path.name} columns: {street_chars.columns.tolist()}")

require_columns(
    street_chars,
    [
        "SensorID", "GMaps_ID", "randomize", "treat_25", "Shade", "Sidewalk",
        "St2Pole_in", "SensDirect", "COR_WIDTH", "yr_repave", "UHI_Qtile2", "pct_treec",
    ],
    street_chars_path.name,
)

street_chars = street_chars.rename(columns={"SensorID": "sensor_id"})
street_chars["sensor_id"] = street_chars["sensor_id"].astype(str)

# this one was at the intersection, but its actually Ashburton-Wickham-Newcastle matched to Oberlin
street_chars["randomize"] = np.where(
    street_chars["GMaps_ID"] == "ASHBURTON_KAPLAN DR_WICKHAM RD",
    10,
    street_chars["randomize"],
)

select_cols = [
    "sensor_id", "GMaps_ID", "randomize", "treat_25", "Shade", "Sidewalk",
    "St2Pole_in", "SensDirect", "COR_WIDTH", "yr_repave", "UHI_Qtile2", "pct_treec",
]
street_chars_filt = street_chars[select_cols].copy()

# 2025 treatment dates identified via "2025 Rejuvenation Progress" excel file
# shared by the city. 44 sensors treated, 18 not yet treated, pushed to 2026.
treatment_date_map = {
    2: "2025-09-12", 3: "2025-09-12", 19: "2025-09-12", 35: "2025-09-12", 36: "2025-09-12",
    1: "2025-09-25", 10: "2025-09-25",
    28: "2025-10-07", 15: "2025-10-07", 17: "2025-10-07", 21: "2025-10-07",
    27: "2025-10-09", 101: "2025-10-09",
    23: "2025-10-10",
    8: "2025-10-18", 20: "2025-10-18", 25: "2025-10-18", 30: "2025-10-18",
    29: "2025-10-21",
    4: "2025-10-22",
    31: "2025-10-23", 9: "2025-10-23",
}
street_chars_filt["treatment_date"] = street_chars_filt["randomize"].map(treatment_date_map)
street_chars_filt["treatment_date"] = (
    pd.to_datetime(street_chars_filt["treatment_date"]).dt.tz_localize("Etc/GMT+4")
)
street_chars_filt["treatment_week"] = lubridate_week(street_chars_filt["treatment_date"])

# Treatment "waves" assigned based on treatments happening in the same week
treatment_wave_map = {
    2: 1, 3: 1, 19: 1, 35: 1, 36: 1,
    1: 2, 10: 2,
    28: 3, 15: 3, 17: 3, 21: 3, 27: 3, 101: 3, 23: 3,
    8: 4, 20: 4, 25: 4, 30: 4,
    29: 5, 4: 5, 31: 5, 9: 5,
}
street_chars_filt["treatment_wave"] = street_chars_filt["randomize"].map(treatment_wave_map)

before = len(street_chars_filt)
street_chars_filt = street_chars_filt[street_chars_filt["treatment_date"].notna()].copy()
print(f"[FILTER] street_chars_filt treatment_date not NA: {before} -> {len(street_chars_filt)}")

# separate_wider_delim(SensDirect, into=c("degrees","direction"), delim=" ")
split_sd = street_chars_filt["SensDirect"].str.split(" ", n=1, expand=True)
if split_sd.shape[1] != 2:
    raise ValueError("SensDirect did not split into exactly 2 parts (degrees, direction) on ' '")
street_chars_filt["degrees"] = pd.to_numeric(split_sd[0])
street_chars_filt["direction"] = split_sd[1]

# Based on sensor orientation, assign to the closest cardinal direction
conditions = [
    street_chars_filt["direction"].isin(["N", "E", "S", "W"]),
    (street_chars_filt["direction"] == "NE") & (street_chars_filt["degrees"] <= 45),
    (street_chars_filt["direction"] == "NE") & (street_chars_filt["degrees"] > 45),
    (street_chars_filt["direction"] == "SE") & (street_chars_filt["degrees"] > 135),  # none <=135
    (street_chars_filt["direction"] == "SW") & (street_chars_filt["degrees"] <= 225),
    (street_chars_filt["direction"] == "SW") & (street_chars_filt["degrees"] > 225),
    (street_chars_filt["direction"] == "NW") & (street_chars_filt["degrees"] <= 315),  # none > 315
]
choices = [street_chars_filt["direction"], "N", "E", "S", "S", "W", "W"]
street_chars_filt["sensor_direction"] = np.select(conditions, choices, default=None)

# separate_wider_delim(GMaps_ID, into=c("street_name","Cross1","Cross2"), delim="_")
split_gmaps = street_chars_filt["GMaps_ID"].str.split("_", n=2, expand=True)
if split_gmaps.shape[1] != 3:
    raise ValueError("GMaps_ID did not split into exactly 3 parts (street_name, Cross1, Cross2) on '_'")
street_chars_filt["street_name"] = split_gmaps[0]
# Cross1/Cross2 (split_gmaps[1], split_gmaps[2]) are dropped immediately below,
# matching the R select(-c(..., Cross1, Cross2)) -- never materialized here.

street_chars_filt = street_chars_filt.drop(columns=["SensDirect", "degrees", "direction"])

# --- Sensor Points (spatial, for LST extraction) --------------------------
shp_path = ANALYSIS_DIR / "03_Sensor_Locations_Characteristics_All.shp"
sensor_points_raw = gpd.read_file(shp_path)
print(f"[LOAD] {shp_path.name} columns: {sensor_points_raw.columns.tolist()}")
require_columns(sensor_points_raw, ["SensorID", "lat_y", "lon_x", "geometry"], shp_path.name)

valid_sensor_ids = street_chars_filt["sensor_id"].unique().tolist()
before = len(sensor_points_raw)
sensor_points = sensor_points_raw[
    sensor_points_raw["SensorID"].astype(str).isin(valid_sensor_ids)
].copy()
print(f"[FILTER] sensor_points SensorID in street_chars_filt: {before} -> {len(sensor_points)}")
sensor_points = sensor_points.rename(columns={"SensorID": "sensor_id"})
sensor_points["sensor_id"] = sensor_points["sensor_id"].astype(str)
sensor_points = sensor_points[["sensor_id", "lat_y", "lon_x", "geometry"]]
# st_write(sensor_points, "LST_Analysis/Data/Orig/Sensor_Points.shp")  # not executed in source

# --- Sensor Data (merge combined_data + street characteristics) ----------
valid_ids = set(street_chars_filt["sensor_id"].unique())
combined_data["sensor_id"] = combined_data["sensor_id"].astype(str)
before = len(combined_data)
sensor_data = combined_data[combined_data["sensor_id"].isin(valid_ids)].copy()
print(f"[FILTER] combined_data sensor_id in street_chars_filt: {before} -> {len(sensor_data)}")

sensor_data = log_and_merge(sensor_data, street_chars_filt, on="sensor_id", label="sensor_data x street_chars_filt")

sensor_data["datetime_orig"] = sensor_data["datetime"]
# as_datetime(datetime_orig, tz="Etc/GMT+4") -- interpret naive clock time as EDT (localize, not convert)
sensor_data["datetime"] = sensor_data["datetime_orig"].dt.tz_localize("Etc/GMT+4")

# FLAG observations where data is missing on at least one sensor in a pair.
# If both T & C sensors are missing for a datetime observation, their data is not included here.
flag_missing_data = sensor_data.copy()
conditions = [flag_missing_data["treat_25"] == 1, flag_missing_data["treat_25"] == 0]
choices = ["treat_data", "control_data"]
flag_missing_data["missing_type"] = np.select(conditions, choices, default=None)

flag_missing_data = (
    flag_missing_data[["sensor_id", "randomize", "datetime", "Temperature", "missing_type"]]
    .drop_duplicates()
)
# NOTE: pivot_wider assumes at most one Temperature value per
# (randomize, datetime, missing_type); aggfunc="first" is used defensively in
# case of unexpected duplicates -- flag if warnings arise.
flag_missing_data = flag_missing_data.pivot_table(
    index=["randomize", "datetime"], columns="missing_type", values="Temperature", aggfunc="first"
).reset_index()
flag_missing_data.columns.name = None
for col in ["treat_data", "control_data"]:
    if col not in flag_missing_data.columns:
        flag_missing_data[col] = np.nan

flag_missing_data["pair_data_missing"] = np.where(
    flag_missing_data["treat_data"].isna() | flag_missing_data["control_data"].isna(), 1, 0
)
flag_missing_data = flag_missing_data[["randomize", "datetime", "pair_data_missing"]]


###############################################################################
# 2. Additional Control Vars
###############################################################################

# --- CERES Solar Radiation Data --------------------------------------------
# Source: https://ceres-tool.larc.nasa.gov/ord-tool/jsp/FLASH_TISASelection.jsp
# Average of the two lat/lon locations from the CERES data - roughly Raleigh area.
ceres_path = ORIGINAL_DIR / "Solar_Radiation" / "CERES_FLASH_TISA_Version1A_Subset_20250701-20251205.csv"
ceres_raw = pd.read_csv(ceres_path)
print(f"[LOAD] {ceres_path.name} columns: {ceres_raw.columns.tolist()}")
require_columns(ceres_raw, ["time", "lon", "lat", "sfc_sw_down_all_daily"], ceres_path.name)

ceres = ceres_raw[["time", "lon", "lat", "sfc_sw_down_all_daily"]].copy()
conditions = [ceres["lat"] == 35.5, ceres["lat"] == 36.5]
ceres["weight"] = np.select(conditions, [0.7, 0.3], default=np.nan)
ceres["sfc_sw_down_all_daily_wgt"] = ceres["sfc_sw_down_all_daily"] * ceres["weight"]

ceres = ceres.groupby(["time", "lon"], as_index=False).agg(
    sfc_sw_down_wgt_orig=("sfc_sw_down_all_daily_wgt", "sum"),
    sfc_sw_down_mean_orig=("sfc_sw_down_all_daily", "mean"),
)

# Ensure chronological order within each lon group before lag/lead to avoid
# cross-contamination (source relies on implicit row order).
ceres = ceres.sort_values(["lon", "time"]).reset_index(drop=True)
grp = ceres.groupby("lon")
ceres["prev_obs_wgt"] = grp["sfc_sw_down_wgt_orig"].shift(1)
ceres["next_obs_wgt"] = grp["sfc_sw_down_wgt_orig"].shift(-1)
ceres["prev_obs_mean"] = grp["sfc_sw_down_mean_orig"].shift(1)
ceres["next_obs_mean"] = grp["sfc_sw_down_mean_orig"].shift(-1)

ceres["sfc_sw_down_wgt"] = np.where(
    ceres["sfc_sw_down_wgt_orig"].notna(),
    ceres["sfc_sw_down_wgt_orig"],
    (ceres["prev_obs_wgt"] + ceres["next_obs_wgt"]) / 2,
)
# NOTE: preserved verbatim from source (case_when for sfc_sw_down_mean uses
# prev_obs_wgt, NOT prev_obs_mean, averaged with next_obs_mean). This appears
# to be a bug in the original R code but is kept exactly as written per
# translation instructions (match original statistical logic exactly).
ceres["sfc_sw_down_mean"] = np.where(
    ceres["sfc_sw_down_mean_orig"].notna(),
    ceres["sfc_sw_down_mean_orig"],
    (ceres["prev_obs_wgt"] + ceres["next_obs_mean"]) / 2,
)

ceres = ceres[["time", "sfc_sw_down_wgt", "sfc_sw_down_mean"]].rename(columns={"time": "date_dt"})
ceres["date_dt"] = pd.to_datetime(ceres["date_dt"])

# --- Hourly Weather Conditions at RDU Airport (NCSU Cardinal) ---------------
# "Times are in Local Standard Time (LST) unless otherwise noted"
rdu_hourly_path = ORIGINAL_DIR / "Weather" / "ZD7DM7W7_1.xlsx"
rdu_hourly_ncsu = pd.read_excel(rdu_hourly_path, skiprows=12)
print(f"[LOAD] {rdu_hourly_path.name} columns: {rdu_hourly_ncsu.columns.tolist()}")
require_columns(
    rdu_hourly_ncsu,
    [
        "Date/Time (Eastern)", "Top-of-the-Hour Air Temperature (F)",
        "Top-of-the-Hour Dew Point Temperature (F)", "Total Precipitation (in)",
        "Cloud Coverage & Height", "Present Weather",
    ],
    rdu_hourly_path.name,
)
rdu_hourly_ncsu = rdu_hourly_ncsu.rename(
    columns={
        "Date/Time (Eastern)": "datetime_str",
        "Top-of-the-Hour Air Temperature (F)": "temp_f_rdu_fill_rdu",
        "Top-of-the-Hour Dew Point Temperature (F)": "dewpoint_f_rdu",
        "Total Precipitation (in)": "precip_in_rdu",
        "Cloud Coverage & Height": "cloud_coverage",
    }
)
rdu_hourly_ncsu = rdu_hourly_ncsu.drop(columns=["Present Weather"])

rdu_hourly_ncsu["datetime_est"] = pd.to_datetime(rdu_hourly_ncsu["datetime_str"]).dt.tz_localize("Etc/GMT+5")
rdu_hourly_ncsu["datetime"] = rdu_hourly_ncsu["datetime_est"].dt.tz_convert("Etc/GMT+4")  # get main datetime var in EDT

rdu_hourly_ncsu["temp_f_rdu_fill_rdu"] = pd.to_numeric(rdu_hourly_ncsu["temp_f_rdu_fill_rdu"], errors="coerce")
rdu_hourly_ncsu["dewpoint_f_rdu"] = pd.to_numeric(rdu_hourly_ncsu["dewpoint_f_rdu"], errors="coerce")
rdu_hourly_ncsu["precip_in_rdu"] = pd.to_numeric(rdu_hourly_ncsu["precip_in_rdu"], errors="coerce")

rdu_hourly_ncsu["cloud_coverage"] = rdu_hourly_ncsu["cloud_coverage"].replace("MV", None)
rdu_hourly_ncsu["cloud_type"] = rdu_hourly_ncsu["cloud_coverage"].str[:3]

# Sort chronologically before lag/lead to prevent cross-contamination.
rdu_hourly_ncsu = rdu_hourly_ncsu.sort_values("datetime").reset_index(drop=True)
rdu_hourly_ncsu["lag_temp"] = rdu_hourly_ncsu["temp_f_rdu_fill_rdu"].shift(1)
rdu_hourly_ncsu["lead_temp"] = rdu_hourly_ncsu["temp_f_rdu_fill_rdu"].shift(-1)
rdu_hourly_ncsu["lag_dp"] = rdu_hourly_ncsu["dewpoint_f_rdu"].shift(1)
rdu_hourly_ncsu["lead_dp"] = rdu_hourly_ncsu["dewpoint_f_rdu"].shift(-1)
rdu_hourly_ncsu["lag_precip"] = rdu_hourly_ncsu["precip_in_rdu"].shift(1)
rdu_hourly_ncsu["lead_precip"] = rdu_hourly_ncsu["precip_in_rdu"].shift(-1)

rdu_hourly_ncsu["temp_f_rdu_fill"] = np.where(
    rdu_hourly_ncsu["temp_f_rdu_fill_rdu"].notna(),
    rdu_hourly_ncsu["temp_f_rdu_fill_rdu"],
    (rdu_hourly_ncsu["lag_temp"] + rdu_hourly_ncsu["lead_temp"]) / 2,
)
rdu_hourly_ncsu["dewpoint_f_rdu_fill"] = np.where(
    rdu_hourly_ncsu["dewpoint_f_rdu"].notna(),
    rdu_hourly_ncsu["dewpoint_f_rdu"],
    (rdu_hourly_ncsu["lag_dp"] + rdu_hourly_ncsu["lead_dp"]) / 2,
)
# NOTE: preserved verbatim from source -- precip fill uses
# (lag_precip + lag_precip)/2; lead_precip is computed but never used. This
# appears to be a bug in the original R code but is kept exactly as written.
rdu_hourly_ncsu["precip_in_rdu_fill"] = np.where(
    rdu_hourly_ncsu["precip_in_rdu"].notna(),
    rdu_hourly_ncsu["precip_in_rdu"],
    (rdu_hourly_ncsu["lag_precip"] + rdu_hourly_ncsu["lag_precip"]) / 2,
)

# If still missing (consecutive missing) OR str var, fill down
fill_cols = ["temp_f_rdu_fill", "dewpoint_f_rdu_fill", "precip_in_rdu_fill", "cloud_type"]
rdu_hourly_ncsu[fill_cols] = rdu_hourly_ncsu[fill_cols].ffill()

rdu_hourly_ncsu = rdu_hourly_ncsu.drop(
    columns=["lag_temp", "lead_temp", "lag_dp", "lead_dp", "lag_precip", "lead_precip"]
)

conditions = [
    rdu_hourly_ncsu["cloud_type"].isin(["CLR", "FEW", "SCT"]),  # less than 50% cloud coverage
    rdu_hourly_ncsu["cloud_type"].isin(["BKN", "OVC"]),  # over 50% cloud coverage
]
rdu_hourly_ncsu["sunny"] = np.select(conditions, [1, 0], default=np.nan)
rdu_hourly_ncsu["date_dt"] = pd.to_datetime(rdu_hourly_ncsu["datetime"].dt.date)

# --- Hourly Solar Radiation from Lake Wheeler Rd (NCSU Cardinal) -----------
rad_path = ORIGINAL_DIR / "Weather" / "WF2FH3Q2_1.xlsx"
hourly_rad_ncsu = pd.read_excel(rad_path, skiprows=12)
print(f"[LOAD] {rad_path.name} columns: {hourly_rad_ncsu.columns.tolist()}")
require_columns(
    hourly_rad_ncsu,
    ["Date/Time (Eastern)", "Top-of-the-Hour Solar Radiation (W/m2)", "Average Solar Radiation (W/m2)"],
    rad_path.name,
)
hourly_rad_ncsu = hourly_rad_ncsu.rename(
    columns={
        "Top-of-the-Hour Solar Radiation (W/m2)": "toh_rad_wm2_orig",
        "Average Solar Radiation (W/m2)": "avg_rad_wm2_orig",
    }
)

hourly_rad_ncsu["toh_rad_wm2_fill_up"] = np.where(
    hourly_rad_ncsu["toh_rad_wm2_orig"].isin(["QCF", "MV"]),
    np.nan,
    pd.to_numeric(hourly_rad_ncsu["toh_rad_wm2_orig"], errors="coerce"),
)
hourly_rad_ncsu["avg_rad_wm2_fill_up"] = np.where(
    hourly_rad_ncsu["avg_rad_wm2_orig"].isin(["QCF", "MV"]),
    np.nan,
    pd.to_numeric(hourly_rad_ncsu["avg_rad_wm2_orig"], errors="coerce"),
)
hourly_rad_ncsu["toh_rad_wm2_fill_down"] = hourly_rad_ncsu["toh_rad_wm2_fill_up"]
hourly_rad_ncsu["avg_rad_wm2_fill_down"] = hourly_rad_ncsu["avg_rad_wm2_fill_up"]

# tidyr::fill(.direction="up") then fill(.direction="down") on original file
# row order (datetime not yet parsed at this point in the source, matching
# source order exactly rather than re-sorting).
hourly_rad_ncsu["toh_rad_wm2_fill_up"] = hourly_rad_ncsu["toh_rad_wm2_fill_up"].bfill()
hourly_rad_ncsu["avg_rad_wm2_fill_up"] = hourly_rad_ncsu["avg_rad_wm2_fill_up"].bfill()
hourly_rad_ncsu["toh_rad_wm2_fill_down"] = hourly_rad_ncsu["toh_rad_wm2_fill_down"].ffill()
hourly_rad_ncsu["avg_rad_wm2_fill_down"] = hourly_rad_ncsu["avg_rad_wm2_fill_down"].ffill()

hourly_rad_ncsu["toh_rad_wm2_fill"] = (
    hourly_rad_ncsu["toh_rad_wm2_fill_up"] + hourly_rad_ncsu["toh_rad_wm2_fill_down"]
) / 2
hourly_rad_ncsu["avg_rad_wm2_fill"] = (
    hourly_rad_ncsu["avg_rad_wm2_fill_up"] + hourly_rad_ncsu["avg_rad_wm2_fill_down"]
) / 2

hourly_rad_ncsu["datetime_est"] = (
    pd.to_datetime(hourly_rad_ncsu["Date/Time (Eastern)"]).dt.tz_localize("Etc/GMT+5")
)
hourly_rad_ncsu["datetime"] = hourly_rad_ncsu["datetime_est"].dt.tz_convert("Etc/GMT+4")
hourly_rad_ncsu = hourly_rad_ncsu[["datetime", "toh_rad_wm2_fill", "avg_rad_wm2_fill"]]

# --- Daily Sunrise/Sunset Data for RDU --------------------------------------
# Source: https://aa.usno.navy.mil/data/RS_OneYear, manually converted to Excel. In EST - convert to EDT!
sun_path = ORIGINAL_DIR / "Solar_Radiation" / "Daily_Sunrise_Sunset.xlsx"
daily_sunlight = pd.read_excel(sun_path, dtype=str)
print(f"[LOAD] {sun_path.name} columns: {daily_sunlight.columns.tolist()}")
require_columns(daily_sunlight, ["Rise", "Set", "Month", "Day"], sun_path.name)

daily_sunlight["rise_h"] = pd.to_numeric(daily_sunlight["Rise"].str[:-2])
daily_sunlight["rise_m"] = pd.to_numeric(daily_sunlight["Rise"].str[-2:])
daily_sunlight["set_h"] = pd.to_numeric(daily_sunlight["Set"].str[:-2])
daily_sunlight["set_m"] = pd.to_numeric(daily_sunlight["Set"].str[-2:])
daily_sunlight["Month"] = pd.to_numeric(daily_sunlight["Month"])
daily_sunlight["Day"] = pd.to_numeric(daily_sunlight["Day"])

# TODO: source year is hardcoded to 2025 in the R script (make_date(year=2025, ...)).
daily_sunlight["date_dt"] = pd.to_datetime(
    dict(year=2025, month=daily_sunlight["Month"], day=daily_sunlight["Day"])
)
daily_sunlight["dt_rise_est"] = pd.to_datetime(
    dict(
        year=2025, month=daily_sunlight["Month"], day=daily_sunlight["Day"],
        hour=daily_sunlight["rise_h"], minute=daily_sunlight["rise_m"],
    )
).dt.tz_localize("Etc/GMT+5")
daily_sunlight["dt_set_est"] = pd.to_datetime(
    dict(
        year=2025, month=daily_sunlight["Month"], day=daily_sunlight["Day"],
        hour=daily_sunlight["set_h"], minute=daily_sunlight["set_m"],
    )
).dt.tz_localize("Etc/GMT+5")

daily_sunlight["dt_rise"] = daily_sunlight["dt_rise_est"].dt.tz_convert("Etc/GMT+4")
daily_sunlight["dt_set"] = daily_sunlight["dt_set_est"].dt.tz_convert("Etc/GMT+4")
daily_sunlight["daylight_mins"] = (
    (daily_sunlight["dt_set"] - daily_sunlight["dt_rise"]).dt.total_seconds() / 60
)
daily_sunlight = daily_sunlight[["date_dt", "dt_rise", "dt_set", "daylight_mins"]]

# --- Combine weather/sunlight control data ---------------------------------
rdu_weather_hourly = log_and_merge(
    rdu_hourly_ncsu, hourly_rad_ncsu, on="datetime", label="rdu_hourly_ncsu x hourly_rad_ncsu"
)
rdu_weather_hourly = log_and_merge(
    rdu_weather_hourly, daily_sunlight, on="date_dt", label="rdu_weather_hourly x daily_sunlight"
)

# only include full hours of daylight - more conservative
rdu_weather_hourly["daytime_hour"] = np.where(
    (rdu_weather_hourly["datetime"].dt.hour > rdu_weather_hourly["dt_rise"].dt.hour)
    & (rdu_weather_hourly["datetime"].dt.hour < rdu_weather_hourly["dt_set"].dt.hour),
    1,
    0,
)
rdu_weather_hourly["hour"] = rdu_weather_hourly["datetime"].dt.hour.astype(float)

drop_cols = [
    "datetime", "datetime_str", "datetime_est", "temp_f_rdu_fill_rdu",
    "dewpoint_f_rdu", "precip_in_rdu", "cloud_coverage",
]
require_columns(rdu_weather_hourly, drop_cols, "rdu_weather_hourly (pre-drop)")
rdu_weather_hourly = rdu_weather_hourly.drop(columns=drop_cols)

# only using hour because this is hourly data merging to 20min sensor data that also has datetime
rdu_weather_hourly["rh_pct"] = dewpoint_to_humidity_fahrenheit(
    rdu_weather_hourly["temp_f_rdu_fill"], rdu_weather_hourly["dewpoint_f_rdu_fill"]
)

rdu_solar_rad_daily = rdu_weather_hourly[rdu_weather_hourly["daytime_hour"] == 1].copy()
rdu_solar_rad_daily = rdu_solar_rad_daily.groupby(
    ["date_dt", "daylight_mins"], as_index=False
).agg(sunny_hours=("sunny", "sum"))

rdu_solar_rad_daily = log_and_merge(
    rdu_solar_rad_daily, ceres, on="date_dt", label="rdu_solar_rad_daily x ceres"
)

# Create lagged values! (no explicit grouping in source -> sort chronologically first)
rdu_solar_rad_daily = rdu_solar_rad_daily.sort_values("date_dt").reset_index(drop=True)
rdu_solar_rad_daily["daylight_mins_l1"] = rdu_solar_rad_daily["daylight_mins"].shift(1)
rdu_solar_rad_daily["sunny_hours_l1"] = rdu_solar_rad_daily["sunny_hours"].shift(1)
rdu_solar_rad_daily["sfc_sw_down_wgt_l1"] = rdu_solar_rad_daily["sfc_sw_down_wgt"].shift(1)
rdu_solar_rad_daily["sfc_sw_down_mean_l1"] = rdu_solar_rad_daily["sfc_sw_down_mean"].shift(1)

del ceres, daily_sunlight, rdu_hourly_ncsu


###############################################################################
# 3. Create 20-min, hourly, and daily DID datasets
###############################################################################

# Full data - 20 min intervals
did_data = log_and_merge(
    sensor_data, rdu_weather_hourly, on=["date_dt", "hour"], label="sensor_data x rdu_weather_hourly"
)
did_data = log_and_merge(
    did_data, rdu_solar_rad_daily, on="date_dt", label="did_data x rdu_solar_rad_daily",
    suffixes=("_x", "_y"),
)
require_columns(did_data, ["daylight_mins_x", "daylight_mins_y"], "did_data (post solar-rad merge)")
did_data = did_data.drop(columns="daylight_mins_x").rename(columns={"daylight_mins_y": "daylight_mins"})

did_data = log_and_merge(
    did_data, flag_missing_data, on=["datetime", "randomize"], label="did_data x flag_missing_data"
)

# Create treatpost indicator: pre-period strictly before treatment midnight;
# post-period from 2 days after treatment onward (excludes day of treatment
# and day after).
threshold_pre = did_data["treatment_date"].dt.normalize()
threshold_post = did_data["treatment_date"].dt.normalize() + pd.Timedelta(days=2)
conditions = [
    did_data["datetime"] < threshold_pre,
    did_data["datetime"] >= threshold_post,
]
did_data["post"] = np.select(conditions, [0, 1], default=np.nan)

before = len(did_data)
did_data = did_data[did_data["post"].notna()].copy()
print(f"[FILTER] did_data post not NA: {before} -> {len(did_data)}")

did_data["treat_post"] = did_data["treat_25"] * did_data["post"]

# Remove first and last weeks
before = len(did_data)
did_data = did_data[(did_data["week"] >= 26) & (did_data["week"] <= 46)].copy()
print(f"[FILTER] did_data week 26-46: {before} -> {len(did_data)}")

# DROP observations from sensor where its match was missing observations, so
# hourly/daily summaries are equivalent across pairs.
before = len(did_data)
did_data = did_data[did_data["pair_data_missing"] == 0].copy()
print(f"[FILTER] did_data pair_data_missing==0: {before} -> {len(did_data)}")

did_data["date"] = did_data["date_dt"]
did_data["treatment_date"] = did_data["treatment_date"].dt.tz_localize(None)
did_data["time_unit"] = did_data["week"] - did_data["treatment_week"]

out_path_20m = PYTHON_ANALYSIS_DIR / "05_Analysis_Input.csv"
did_data.to_csv(out_path_20m, index=False)  # Too large for GitHub, stored on DDL Gdrive
print(f"[WRITE] {out_path_20m} rows={len(did_data)}")

# --- Hourly ------------------------------------------------------------
group_cols_hourly = [
    "sensor_id", "hour", "date_dt", "week", "street_name", "GMaps_ID", "randomize", "treat_25",
    "Shade", "Sidewalk", "St2Pole_in", "COR_WIDTH", "yr_repave", "treatment_date",
    "treatment_week", "treatment_wave", "sensor_direction", "UHI_Qtile2",
    "pct_treec", "cloud_type", "sunny", "daylight_mins", "toh_rad_wm2_fill",
    "avg_rad_wm2_fill", "sunny_hours", "sfc_sw_down_wgt", "sfc_sw_down_mean", "daylight_mins_l1",
    "sunny_hours_l1", "sfc_sw_down_wgt_l1", "sfc_sw_down_mean_l1",
    "post", "treat_post", "date", "time_unit",
]
agg_map_hourly = {
    "Temperature": "mean",
    "Relative.Humidity": "mean",
    "temp_f_rdu_fill": "mean",
    "dewpoint_f_rdu_fill": "mean",
    "rh_pct": "mean",
    "precip_in_rdu_fill": "sum",
}
require_columns(did_data, group_cols_hourly, "did_data (hourly grouping columns)")
require_columns(did_data, list(agg_map_hourly), "did_data (hourly agg columns)")

did_data_hourly = did_data.groupby(group_cols_hourly, as_index=False, dropna=False).agg(agg_map_hourly)
# NOTE: R distinguishes NaN (e.g. mean() of an empty/all-NA group) from NA and
# explicitly coerces NaN -> NA. pandas represents both as the same float NaN,
# so no separate coercion step is required here.

out_path_hourly = PYTHON_ANALYSIS_DIR / "05_Analysis_Data_Hourly.csv"
did_data_hourly.to_csv(out_path_hourly, index=False)
print(f"[WRITE] {out_path_hourly} rows={len(did_data_hourly)}")

# --- Daily ---------------------------------------------------------------
group_cols_daily = [
    "sensor_id", "date_dt", "week", "street_name", "GMaps_ID", "randomize", "treat_25",
    "Shade", "Sidewalk", "St2Pole_in", "COR_WIDTH", "yr_repave", "UHI_Qtile2",
    "pct_treec", "treatment_date", "treatment_week", "treatment_wave", "sensor_direction",
    "daylight_mins", "sunny_hours", "sfc_sw_down_wgt", "sfc_sw_down_mean",
    "daylight_mins_l1", "sunny_hours_l1", "sfc_sw_down_wgt_l1", "sfc_sw_down_mean_l1",
    "post", "treat_post", "date", "time_unit",
]
agg_map_daily = {
    "Temperature": "mean",
    "Relative.Humidity": "mean",
    "temp_f_rdu_fill": "mean",
    "dewpoint_f_rdu_fill": "mean",
    "rh_pct": "mean",
    "precip_in_rdu_fill": "sum",
    # these are prob not useful, but want to compare to sfc vars
    "avg_rad_wm2_fill": "sum",
    "toh_rad_wm2_fill": "sum",
}
require_columns(did_data, group_cols_daily, "did_data (daily grouping columns)")
require_columns(did_data, list(agg_map_daily), "did_data (daily agg columns)")

did_data_daily = did_data.groupby(group_cols_daily, as_index=False, dropna=False).agg(agg_map_daily)

out_path_daily = PYTHON_ANALYSIS_DIR / "05_Analysis_Data_Daily.csv"
did_data_daily.to_csv(out_path_daily, index=False)
print(f"[WRITE] {out_path_daily} rows={len(did_data_daily)}")


###############################################################################
# ASSUMPTIONS / TODOs (consolidated)
###############################################################################
# 1. SENSOR_DATA_DIR / clean_csvs glob assumes a "Data/2025 Sensor Data
#    Collection/CleanData" folder relative to PROJECT_ROOT; original R script
#    hardcoded an absolute, machine-specific path. Verify before running.
# 2. "Sensor_Data_Week*.csv" glob approximates R's anchored regex
#    "^Sensor_Data_Week.*\\.csv$"; confirm no unintended matches/misses.
# 3. Two bugs in the original R logic were preserved verbatim (per
#    "match original logic exactly, no optimization"):
#      a. ceres sfc_sw_down_mean fallback uses prev_obs_wgt instead of
#         prev_obs_mean, averaged with next_obs_mean.
#      b. rdu_hourly_ncsu precip_in_rdu_fill fallback uses
#         (lag_precip + lag_precip)/2, never referencing lead_precip.
# 4. daily_sunlight hardcodes year=2025, matching the R source's
#    make_date(year=2025, ...).
# 5. flag_missing_data pivot uses aggfunc="first" defensively in case of
#    duplicate (randomize, datetime, missing_type) combinations; R's
#    pivot_wider would behave unpredictably (list-column or error) in that case.
# 6. lubridate::week() was reimplemented manually (NOT ISO week) --
#    week = floor((day_of_year - 1)/7) + 1.
# 7. weathermetrics::dewpoint.to.humidity() Magnus-formula constants
#    (17.625, 243.04) were reimplemented manually; verify against the
#    installed R package version if precision discrepancies are found.