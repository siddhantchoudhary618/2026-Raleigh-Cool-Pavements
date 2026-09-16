################################################################################
# Program Name: 06a_Sensor_Models_DID.py
# Program Purpose: Run main DID models on the sensor data
# Translated from: 06a_Sensor_Models_DID.R
# Original Author: Katherine Burley Farr (kburley@ad.unc.edu)
# UNC Department of Public Policy, Data-Driven EnviroLab
################################################################################

import warnings
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import seaborn as sns
import statsmodels.formula.api as smf
import statsmodels.api as sm
from statsmodels.stats.outliers_influence import variance_inflation_factor
from statsmodels.iolib.summary2 import summary_col

# ------------------------------------------------------------------------------
# 0. Paths (PROJECT_ROOT immutable; all new outputs -> Python_Analysis)
# ------------------------------------------------------------------------------

PROJECT_ROOT = Path("/Users/siddhantchoudhary/Downloads/raleigh-cool-pavements-main")
DATA_DIR = PROJECT_ROOT / "Data"

# Original (read-only) input locations - DO NOT MODIFY STRUCTURE
ANALYSIS_DATA_DIR = DATA_DIR / "Analysis"
RESULTS_DIR = DATA_DIR / "Results"          # original R results dir (read-only reference; not written to)
FIGURES_DIR = DATA_DIR / "Analysis" / "Figures"  # original R figures dir (read-only reference; not written to)

# All Python-generated outputs go here instead
PYTHON_OUTPUT_DIR = DATA_DIR / "Analysis" / "Python_Analysis_06a"
PYTHON_FIGURES_DIR = PYTHON_OUTPUT_DIR / "Figures"
PYTHON_RESULTS_DIR = PYTHON_OUTPUT_DIR / "Results"

PYTHON_OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
PYTHON_FIGURES_DIR.mkdir(parents=True, exist_ok=True)
PYTHON_RESULTS_DIR.mkdir(parents=True, exist_ok=True)

INPUT_20M = ANALYSIS_DATA_DIR / "05_Analysis_Data.csv"
INPUT_HOURLY = ANALYSIS_DATA_DIR / "05_Analysis_Data_Hourly.csv"
INPUT_DAILY = ANALYSIS_DATA_DIR / "05_Analysis_Data_Daily.csv"

output_manifest = []  # (path, row_count) tuples for final summary


def log_output(path: Path, row_count):
    print(f"[OUTPUT] Writing -> {path} (rows={row_count})")
    output_manifest.append((path, row_count))


# ------------------------------------------------------------------------------
# 1. Bring in Data
# ------------------------------------------------------------------------------

def load_and_filter(path: Path, label: str) -> pd.DataFrame:
    """
    Load a CSV and drop rows where month(date) == 6 (June),
    matching R: filter(!month(date) %in% c(6))
    """
    print(f"\n[LOAD] {label}: {path}")
    if not path.exists():
        raise FileNotFoundError(f"Required input file not found for {label}: {path}")

    df = pd.read_csv(path)
    print(f"[LOAD] {label} columns: {df.columns.tolist()}")

    required = ["date"]
    missing = set(required) - set(df.columns)
    if missing:
        raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(df.columns)}")

    n_before = len(df)
    df["date"] = pd.to_datetime(df["date"])
    df = df[df["date"].dt.month != 6].copy()
    n_after = len(df)
    print(f"[FILTER] {label}: dropped June rows. Before={n_before}, After={n_after}")

    return df


did_data = load_and_filter(INPUT_20M, "did_data (20-min)")
did_data_hourly = load_and_filter(INPUT_HOURLY, "did_data_hourly")
did_data_daily = load_and_filter(INPUT_DAILY, "did_data_daily")

# ------------------------------------------------------------------------------
# 2. Data Checks - Correlation matrices
# ------------------------------------------------------------------------------

corr_cols = [
    "Temperature", "temp_f_rdu_fill", "dewpoint_f_rdu_fill", "rh_pct",
    "precip_in_rdu_fill", "sunny", "sunny_hours", "sunny_hours_l1", "daylight_mins",
    "daylight_mins_l1", "sfc_sw_down_wgt", "sfc_sw_down_wgt_l1",
    "sfc_sw_down_mean", "sfc_sw_down_mean_l1", "toh_rad_wm2_fill",
    "avg_rad_wm2_fill",
]

missing = set(corr_cols) - set(did_data.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data.columns)}")

corr_data = (
    did_data[corr_cols]
    .loc[lambda d: d["temp_f_rdu_fill"].notna()]
    .loc[lambda d: d["sunny_hours_l1"].notna()]
    .loc[lambda d: d["sfc_sw_down_mean_l1"].notna()]
    .copy()
)
print(f"\n[CORR] corr_data rows after dropping missing key controls: {len(corr_data)}")

cor_matrix = corr_data.corr(method="pearson")

fig, ax = plt.subplots(figsize=(10, 10))
sns.heatmap(cor_matrix, annot=False, cmap="coolwarm", center=0, square=True, ax=ax)
ax.set_title("Correlation Matrix (20-min Data)")
plt.tight_layout()
corr_plot_path = PYTHON_FIGURES_DIR / "06_corrplot_20m.png"
plt.savefig(corr_plot_path, dpi=300)
plt.close(fig)
log_output(corr_plot_path, len(cor_matrix))

# Daily corr_data - note: original R uses did_data_daily columns for corr_data_daily
# but then computes cor_matrix_d from corr_data (NOT corr_data_daily) -- this appears
# to be a bug in the original R script (cor(corr_data, ...) instead of cor(corr_data_daily, ...)).
# TODO: R BUG PRESERVED - cor_matrix_d is computed from `corr_data` (20-min), not `corr_data_daily`,
# even though corr_data_daily was constructed and unused for this purpose. Preserving R's exact behavior.
daily_corr_cols = [
    "Temperature", "temp_f_rdu_fill", "dewpoint_f_rdu_fill", "rh_pct",
    "precip_in_rdu_fill", "sunny_hours", "sunny_hours_l1", "daylight_mins",
    "daylight_mins_l1", "sfc_sw_down_wgt", "sfc_sw_down_wgt_l1",
    "sfc_sw_down_mean", "sfc_sw_down_mean_l1", "toh_rad_wm2_fill",
    "avg_rad_wm2_fill",
]
missing = set(daily_corr_cols) - set(did_data_daily.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data_daily.columns)}")

corr_data_daily = (
    did_data_daily[daily_corr_cols]
    .loc[lambda d: d["temp_f_rdu_fill"].notna()]
    .loc[lambda d: d["sunny_hours_l1"].notna()]
    .loc[lambda d: d["sfc_sw_down_mean_l1"].notna()]
    .copy()
)
print(f"[CORR] corr_data_daily rows after dropping missing key controls: {len(corr_data_daily)}")

# Exact R replication: cor_matrix_d <- cor(corr_data, ...) -- uses corr_data, not corr_data_daily
cor_matrix_d = corr_data.corr(method="pearson")  # TODO: R bug preserved (see comment above)

fig, ax = plt.subplots(figsize=(10, 10))
sns.heatmap(cor_matrix_d, annot=False, cmap="coolwarm", center=0, square=True, ax=ax)
ax.set_title("Correlation Matrix (Daily Data) [R BUG: computed from 20-min corr_data]")
plt.tight_layout()
corr_plot_d_path = PYTHON_FIGURES_DIR / "06_corrplot_daily.png"
plt.savefig(corr_plot_d_path, dpi=300)
plt.close(fig)
log_output(corr_plot_d_path, len(cor_matrix_d))


# ------------------------------------------------------------------------------
# Helper: clustered SE model fitting
# ------------------------------------------------------------------------------

def fit_ols_clustered(formula: str, data: pd.DataFrame, cluster_col: str):
    """
    Fits OLS via statsmodels formula API and applies clustered standard errors
    on `cluster_col`, replicating R's coeftest(x, vcov=vcovCL(x, cluster=...)).
    """
    required = {cluster_col}
    missing = required - set(data.columns)
    if missing:
        raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(data.columns)}")

    model = smf.ols(formula=formula, data=data, missing="drop")
    fitted = model.fit()
    clustered = fitted.get_robustcov_results(
        cov_type="cluster", groups=data.loc[fitted.model.data.row_labels, cluster_col]
    )
    return clustered


def extract_coef_ci(fitted_clustered, alpha=0.05):
    """
    Replicates: coef(x) + coefci(x, vcov=...) -> data.frame(Term, Estimate, CI_2.5, CI_97.5)
    """
    params = fitted_clustered.params
    conf_int = fitted_clustered.conf_int(alpha=alpha)
    names = fitted_clustered.model.exog_names
    results = pd.DataFrame({
        "Term": names,
        "Estimate": params,
        "CI_2.5": conf_int[:, 0],
        "CI_97.5": conf_int[:, 1],
    })
    return results


def parse_factor_term(results: pd.DataFrame, var_name: str, base_label: str,
                       interaction_label: str, treat_var: str = "treat_25") -> pd.DataFrame:
    """
    Replicates:
      filter(grepl(var_name, Term)) %>%
      separate(Term, into=c("Var","Level"), sep="\\(var_name\\)") %>%
      mutate(Var = case_when(Var == "factor" ~ base_label,
                              Var == "treat_25:factor" ~ interaction_label),
             Level = as.numeric(Level))
    statsmodels/patsy factor term naming differs from R's factor()/lm term naming
    (e.g. 'C(hour)[T.5]' vs R's 'factor(hour)5'), so we parse the patsy convention
    directly rather than R's string pattern.
    """
    pattern_marker = f"C({var_name})"
    mask = results["Term"].str.contains(pattern_marker, regex=False)
    sub = results.loc[mask].copy()

    def classify(term):
        if term.startswith(f"C({var_name})[T."):
            return base_label
        elif f"{treat_var}:C({var_name})" in term:
            return interaction_label
        else:
            return None

    def extract_level(term):
        # Patsy format: C(hour)[T.5]  or  treat_25:C(hour)[T.5]
        marker = f"[T."
        if marker in term:
            level_str = term.split(marker)[-1].rstrip("]")
            try:
                return float(level_str)
            except ValueError:
                return np.nan
        return np.nan

    sub["Var"] = sub["Term"].apply(classify)
    sub["Level"] = sub["Term"].apply(extract_level)
    sub = sub.dropna(subset=["Var"])
    return sub


def plot_coef_ci(df: pd.DataFrame, x_col: str, title: str, xlabel: str, ylabel: str,
                  xticks_range, out_path: Path, dpi=300, figsize=(10, 4)):
    fig, ax = plt.subplots(figsize=figsize)
    ax.errorbar(
        df[x_col], df["Estimate"],
        yerr=[df["Estimate"] - df["CI_2.5"], df["CI_97.5"] - df["Estimate"]],
        fmt="o", color="#004351", ecolor="#166a7c", elinewidth=1.5, capsize=3, markersize=6,
    )
    ax.axhline(y=0, color="black", linestyle="--", linewidth=1)
    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.set_xticks(list(xticks_range))
    ax.xaxis.set_major_locator(mticker.FixedLocator(list(xticks_range)))
    plt.tight_layout()
    fig.savefig(out_path, dpi=dpi)
    plt.close(fig)
    return out_path


# ------------------------------------------------------------------------------
# Pre-trends: Hourly, unadjusted, week 26-36
# ------------------------------------------------------------------------------

required_h = {"Temperature", "treat_25", "week", "street_name"}
missing = required_h - set(did_data_hourly.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data_hourly.columns)}")

subset_h_2636 = did_data_hourly[
    (did_data_hourly["week"] > 26) & (did_data_hourly["week"] <= 36)
].copy()
print(f"\n[MODEL] pre_trends_hourly_unadj: rows in week(26,36] subset = {len(subset_h_2636)}")

formula_pre_h_unadj = "Temperature ~ treat_25 + C(week) + treat_25:C(week)"
pre_trends_hourly_unadj = fit_ols_clustered(formula_pre_h_unadj, subset_h_2636, "street_name")
print(pre_trends_hourly_unadj.summary())

pre_h_unadj_results = extract_coef_ci(pre_trends_hourly_unadj)
pre_h_unadj_results = parse_factor_term(
    pre_h_unadj_results, "week", "Weeks", "Treatment*Week"
).rename(columns={"Level": "Week"})

pre_h_unadj_plot_path = plot_coef_ci(
    pre_h_unadj_results[pre_h_unadj_results["Var"] == "Treatment*Week"],
    x_col="Week",
    title="a. Difference between Treatment and Match Comparison Group (Hourly Data)",
    xlabel="Treat*Week",
    ylabel="Estimate (Degrees F)",
    xticks_range=range(26, 37, 1),
    out_path=PYTHON_FIGURES_DIR / "06_pre_h_unadj_treatweek.png",
)
log_output(pre_h_unadj_plot_path, len(pre_h_unadj_results[pre_h_unadj_results["Var"] == "Treatment*Week"]))

pre_h_unadj_plot_week_path = plot_coef_ci(
    pre_h_unadj_results[pre_h_unadj_results["Var"] == "Weeks"],
    x_col="Week",
    title="b. Difference in Weekly Average Temperature (Hourly Data)",
    xlabel="Week",
    ylabel="Estimate (Degrees F)",
    xticks_range=range(26, 37, 1),
    out_path=PYTHON_FIGURES_DIR / "06_pre_h_unadj_week.png",
)
log_output(pre_h_unadj_plot_week_path, len(pre_h_unadj_results[pre_h_unadj_results["Var"] == "Weeks"]))

# ggarrange(pre_h_unadj_plot, pre_h_unadj_plot_week, ncol=1, nrow=2) -> combined figure
fig, axes = plt.subplots(2, 1, figsize=(10, 8))
for ax, var_label, title, xlab in [
    (axes[0], "Treatment*Week", "a. Difference between Treatment and Match Comparison Group (Hourly Data)", "Treat*Week"),
    (axes[1], "Weeks", "b. Difference in Weekly Average Temperature (Hourly Data)", "Week"),
]:
    sub = pre_h_unadj_results[pre_h_unadj_results["Var"] == var_label]
    ax.errorbar(
        sub["Week"], sub["Estimate"],
        yerr=[sub["Estimate"] - sub["CI_2.5"], sub["CI_97.5"] - sub["Estimate"]],
        fmt="o", color="#004351", ecolor="#166a7c", elinewidth=1.5, capsize=3, markersize=6,
    )
    ax.axhline(y=0, color="black", linestyle="--", linewidth=1)
    ax.set_title(title)
    ax.set_xlabel(xlab)
    ax.set_ylabel("Estimate (Degrees F)")
    ax.set_xticks(list(range(26, 37, 1)))
plt.tight_layout()
combined_path_1 = PYTHON_FIGURES_DIR / "06_Pre_Trends_by_week_hourly.png"
fig.savefig(combined_path_1, dpi=300)
plt.close(fig)
log_output(combined_path_1, len(pre_h_unadj_results))


# ------------------------------------------------------------------------------
# Pre-trends: Daily, unadjusted, week <= 36
# ------------------------------------------------------------------------------

required_d = {"Temperature", "treat_25", "week", "street_name"}
missing = required_d - set(did_data_daily.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data_daily.columns)}")

subset_d_36 = did_data_daily[did_data_daily["week"] <= 36].copy()
print(f"\n[MODEL] pre_trends_daily_unadj: rows in week<=36 subset = {len(subset_d_36)}")

formula_pre_d_unadj = "Temperature ~ treat_25 + C(week) + treat_25:C(week)"
pre_trends_daily_unadj = fit_ols_clustered(formula_pre_d_unadj, subset_d_36, "street_name")
print(pre_trends_daily_unadj.summary())

pre_d_unadj_results = extract_coef_ci(pre_trends_daily_unadj)
pre_d_unadj_results = parse_factor_term(
    pre_d_unadj_results, "week", "Weeks", "Treatment*Week"
).rename(columns={"Level": "Week"})

pre_d_unadj_plot_path = plot_coef_ci(
    pre_d_unadj_results[pre_d_unadj_results["Var"] == "Treatment*Week"],
    x_col="Week",
    title="Pre-Treatment Trends - Daily Data - Unadjusted",
    xlabel="Treat*Week",
    ylabel="Estimate (Degrees F)",
    xticks_range=range(26, 37, 1),
    out_path=PYTHON_FIGURES_DIR / "06_pre_d_unadj.png",
)
log_output(pre_d_unadj_plot_path, len(pre_d_unadj_results[pre_d_unadj_results["Var"] == "Treatment*Week"]))

# ggarrange(pre_h_unadj_plot, pre_d_unadj_plot, ncol=1, nrow=2) -- NOTE: R reuses the
# HOURLY treat*week plot here (pre_h_unadj_plot), not a daily "Weeks" plot.
# TODO: R BUG/INTENTIONAL PRESERVED - second ggarrange combines pre_h_unadj_plot (hourly)
# with pre_d_unadj_plot (daily), not two daily plots. Preserving exactly.
fig, axes = plt.subplots(2, 1, figsize=(10, 8))
sub_h = pre_h_unadj_results[pre_h_unadj_results["Var"] == "Treatment*Week"]
sub_d = pre_d_unadj_results[pre_d_unadj_results["Var"] == "Treatment*Week"]
for ax, sub, title, xlab in [
    (axes[0], sub_h, "a. Difference between Treatment and Match Comparison Group (Hourly Data)", "Treat*Week"),
    (axes[1], sub_d, "Pre-Treatment Trends - Daily Data - Unadjusted", "Treat*Week"),
]:
    ax.errorbar(
        sub["Week"], sub["Estimate"],
        yerr=[sub["Estimate"] - sub["CI_2.5"], sub["CI_97.5"] - sub["Estimate"]],
        fmt="o", color="#004351", ecolor="#166a7c", elinewidth=1.5, capsize=3, markersize=6,
    )
    ax.axhline(y=0, color="black", linestyle="--", linewidth=1)
    ax.set_title(title)
    ax.set_xlabel(xlab)
    ax.set_ylabel("Estimate (Degrees F)")
    ax.set_xticks(list(range(26, 37, 1)))
plt.tight_layout()
combined_path_2 = PYTHON_FIGURES_DIR / "06_Pre_Trends_by_week.png"
fig.savefig(combined_path_2, dpi=300)
plt.close(fig)
log_output(combined_path_2, len(sub_h) + len(sub_d))


# ------------------------------------------------------------------------------
# Pre-trends: By hour - Adjusted with controls, week <= 36
# ------------------------------------------------------------------------------

required_h_adj = {
    "Temperature", "treat_25", "hour", "street_name", "temp_f_rdu_fill", "rh_pct",
    "precip_in_rdu_fill", "sunny", "sfc_sw_down_wgt", "sfc_sw_down_wgt_l1",
}
missing = required_h_adj - set(did_data_hourly.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data_hourly.columns)}")

subset_h_36 = did_data_hourly[did_data_hourly["week"] <= 36].copy()
print(f"\n[MODEL] pre_trends_byhour_adj: rows in week<=36 subset = {len(subset_h_36)}")

formula_pre_hour_adj = (
    "Temperature ~ treat_25 + C(hour) + treat_25:C(hour) + "
    "temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + "
    "sfc_sw_down_wgt + sfc_sw_down_wgt_l1"
)
pre_trends_byhour_adj = fit_ols_clustered(formula_pre_hour_adj, subset_h_36, "street_name")
print(pre_trends_byhour_adj.summary())

pre_hour_adj_results = extract_coef_ci(pre_trends_byhour_adj)
pre_hour_adj_results = parse_factor_term(
    pre_hour_adj_results, "hour", "Hour", "Treatment*Hour"
).rename(columns={"Level": "Hour"})

pre_hour_adj_plot_path = plot_coef_ci(
    pre_hour_adj_results[pre_hour_adj_results["Var"] == "Treatment*Hour"],
    x_col="Hour",
    title="Pre-Treatment Trends - Hour of Day - Adjusted",
    xlabel="Treat*Hour",
    ylabel="Estimate (Degrees F)",
    xticks_range=range(0, 24, 1),
    out_path=PYTHON_FIGURES_DIR / "06_pre_hour_adj.png",
)
log_output(pre_hour_adj_plot_path, len(pre_hour_adj_results[pre_hour_adj_results["Var"] == "Treatment*Hour"]))
# NOTE (R comment): "differences in hours 8 and 9"


# ------------------------------------------------------------------------------
# Pre-trends: By hour - Unadjusted, week <= 36
# ------------------------------------------------------------------------------

required_h_unadj = {"Temperature", "treat_25", "hour", "street_name", "week"}
missing = required_h_unadj - set(did_data_hourly.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data_hourly.columns)}")

formula_pre_hour_unadj = "Temperature ~ treat_25 + C(hour) + treat_25:C(hour)"
pre_trends_byhour_unadj = fit_ols_clustered(formula_pre_hour_unadj, subset_h_36, "street_name")
print(pre_trends_byhour_unadj.summary())

pre_hour_unadj_results = extract_coef_ci(pre_trends_byhour_unadj)
pre_hour_unadj_results = parse_factor_term(
    pre_hour_unadj_results, "hour", "Hour", "Treatment*Hour"
).rename(columns={"Level": "Hour"})

pre_hour_unadj_plot_path = plot_coef_ci(
    pre_hour_unadj_results[pre_hour_unadj_results["Var"] == "Treatment*Hour"],
    x_col="Hour",
    title="Pre-Treatment Trends - Hour of Day - Unadjusted",
    xlabel="Treat*Hour",
    ylabel="Estimate (Degrees F)",
    xticks_range=range(0, 24, 1),
    out_path=PYTHON_FIGURES_DIR / "06_pre_hour_unadj.png",
)
log_output(pre_hour_unadj_plot_path, len(pre_hour_unadj_results[pre_hour_unadj_results["Var"] == "Treatment*Hour"]))
# NOTE (R comment): "differences in hours 8 and 9"


# ==============================================================================
# 2. Basic DID
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. 20-minute interval data
# ------------------------------------------------------------------------------

required_20m_base = {"Temperature", "treat_25", "post", "treat_post", "street_name"}
missing = required_20m_base - set(did_data.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data.columns)}")

# se_cluster(x): clustered SE helper on did_data$street_name
# Python equivalent applied inline via fit_ols_clustered / get_robustcov_results

# Basic DID Model
m1 = fit_ols_clustered("Temperature ~ treat_25 + post + treat_post", did_data, "street_name")
print("\n[MODEL] m1 (20m Basic DID) summary:")
print(m1.summary())

# DID Model + Time Varying Controls
required_m2 = {"hour", "temp_f_rdu_fill", "rh_pct", "precip_in_rdu_fill", "sunny",
               "sfc_sw_down_wgt", "sfc_sw_down_wgt_l1"}
missing = required_m2 - set(did_data.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data.columns)}")

formula_m2 = (
    "Temperature ~ treat_25 + post + treat_post + C(hour) + "
    "temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + "
    "sfc_sw_down_wgt + sfc_sw_down_wgt_l1"
)
m2 = fit_ols_clustered(formula_m2, did_data, "street_name")
print("\n[MODEL] m2 (20m + Time Controls) summary:")
print(m2.summary())

# DID Model + Time Varying Controls + Sensor Characteristics
required_m3 = {"Shade", "Sidewalk", "St2Pole_in", "COR_WIDTH", "sensor_direction", "pct_treec"}
missing = required_m3 - set(did_data.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data.columns)}")

formula_m3 = (
    "Temperature ~ treat_25 + post + treat_post + C(hour) + "
    "temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + "
    "sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + "
    "Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec"
)
m3 = fit_ols_clustered(formula_m3, did_data, "street_name")
print("\n[MODEL] m3 (20m + All Controls) summary:")
print(m3.summary())

# vif(m3) - Variance Inflation Factor on the design matrix (non-clustered OLS design)
m3_plain = smf.ols(formula=formula_m3, data=did_data, missing="drop").fit()
X = m3_plain.model.exog
vif_data = pd.DataFrame({
    "Variable": m3_plain.model.exog_names,
    "VIF": [variance_inflation_factor(X, i) for i in range(X.shape[1])],
})
print("\n[VIF] m3 Variance Inflation Factors:")
print(vif_data)

# rm(m1, m2, m3); gc() -- Python garbage collection is automatic; explicit del for parity
del m1, m2, m3, m3_plain

# models_20m list (re-fit per R script exactly, including dewpoint_f_rdu_fill in m2/m3 here
# vs rh_pct-only version above -- R re-defines these models with different specs for the
# stargazer table; preserving exact duplication/discrepancy from source)
required_20m_v2 = {"dewpoint_f_rdu_fill"}
missing = required_20m_v2 - set(did_data.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data.columns)}")

formula_m1_20m = "Temperature ~ treat_25 + post + treat_post"
formula_m2_20m = (
    "Temperature ~ treat_25 + post + treat_post + C(hour) + "
    "temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + sunny + "
    "sfc_sw_down_wgt + sfc_sw_down_wgt_l1"
)
formula_m3_20m = (
    "Temperature ~ treat_25 + post + treat_post + C(hour) + "
    "temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + sunny + "
    "sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + "
    "Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec"
)

m1_20m_plain = smf.ols(formula=formula_m1_20m, data=did_data, missing="drop").fit()
m2_20m_plain = smf.ols(formula=formula_m2_20m, data=did_data, missing="drop").fit()
m3_20m_plain = smf.ols(formula=formula_m3_20m, data=did_data, missing="drop").fit()

models_20m = [m1_20m_plain, m2_20m_plain, m3_20m_plain]
models_20m_clustered = [
    fit_ols_clustered(formula_m1_20m, did_data, "street_name"),
    fit_ols_clustered(formula_m2_20m, did_data, "street_name"),
    fit_ols_clustered(formula_m3_20m, did_data, "street_name"),
]

# stargazer(models_20m, type="text", omit="hour") -- text-mode summary table (console)
print("\n[TABLE] models_20m (20m) summary - text form (hour dummies omitted):")
summary_text_20m = summary_col(
    models_20m, stars=True, float_format="%0.3f",
    model_names=["Basic DID", "DID + Time Varying Controls", "DID + All Controls"],
    regressor_order=["Intercept", "treat_25", "post", "treat_post"],
    drop_omitted=False,
)
print(summary_text_20m)

# stargazer HTML output w/ clustered SEs, custom labels, omit hour dummies
covariate_labels_20m = [
    "Treat", "Post", "Treat*Post", "RDU Temp. (F)", "RDU RH (%)", "RDU Tot. Precip. (in)",
    "RDU Sunny Flag", "Surface SW Rad. Down", "Surface SW Rad. Down Lag1",
    "Shaded", "Sidewalk", "Distance to Street (in)", "Street Width (ft)", "South-Facing",
    "North-Facing", "West-Facing", "Pct Treecover",
]

def build_stargazer_style_html(models_clustered, model_names, covariate_labels, out_path: Path):
    """
    Approximate stargazer(type='html', single.row=TRUE, report='vc*p', omit='hour')
    using pandas to build an HTML regression table with clustered coefficients,
    clustered SEs, and p-values. Hour/week factor dummy terms are omitted.
    """
    rows = []
    for m in models_clustered:
        names = m.model.exog_names
        params = m.params
        bse = m.bse
        pvals = m.pvalues
        d = {}
        for i, n in enumerate(names):
            if "C(hour)" in n or "C(week)" in n:
                continue  # omit hour/week dummies, matches R's omit="hour"
            d[n] = {"coef": params[i], "se": bse[i], "p": pvals[i]}
        rows.append(d)

    all_terms = []
    for d in rows:
        for k in d.keys():
            if k not in all_terms:
                all_terms.append(k)

    table_rows = []
    for term in all_terms:
        row_cells = []
        for d in rows:
            if term in d:
                coef = d[term]["coef"]
                se = d[term]["se"]
                p = d[term]["p"]
                stars = "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.1 else ""
                cell = f"{coef:.3f}{stars} ({se:.3f})"
            else:
                cell = ""
            row_cells.append(cell)
        table_rows.append([term] + row_cells)

    label_map = dict(zip([t for t in all_terms if t != "Intercept"], covariate_labels))
    display_terms = ["Intercept"] + [t for t in all_terms if t != "Intercept"]
    table_rows_sorted = sorted(table_rows, key=lambda r: display_terms.index(r[0]) if r[0] in display_terms else 999)

    html_rows = ""
    for row in table_rows_sorted:
        term_label = label_map.get(row[0], row[0])
        cells = "".join(f"<td>{c}</td>" for c in row[1:])
        html_rows += f"<tr><td>{term_label}</td>{cells}</tr>\n"

    header_cells = "".join(f"<th>{name}</th>" for name in model_names)
    html = f"""<html><head><title>DID Models</title></head><body>
<table border="1" cellspacing="0" cellpadding="4">
<tr><th>Variable</th>{header_cells}</tr>
{html_rows}
</table>
</body></html>"""

    out_path.parent.mkdir(parents=True, exist_ok=True)
    print(f"[OUTPUT] Writing -> {out_path}")
    with open(out_path, "w") as f:
        f.write(html)
    return out_path


html_20m_path = PYTHON_RESULTS_DIR / "06_DID_Models_20m.html"
build_stargazer_style_html(
    models_20m_clustered,
    model_names=["Basic DID", "DID + Time Varying Controls", "DID + All Controls"],
    covariate_labels=covariate_labels_20m,
    out_path=html_20m_path,
)
log_output(html_20m_path, len(did_data))


# ------------------------------------------------------------------------------
# 2. Hourly data
# ------------------------------------------------------------------------------

required_hourly_base = {"Temperature", "treat_25", "post", "treat_post", "street_name"}
missing = required_hourly_base - set(did_data_hourly.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data_hourly.columns)}")

# Basic DID Model
m1_h = fit_ols_clustered("Temperature ~ treat_25 + post + treat_post", did_data_hourly, "street_name")
print("\n[MODEL] m1_h (Hourly Basic DID) summary:")
print(m1_h.summary())

# DID Model + Time Varying Controls
formula_m2_h = (
    "Temperature ~ treat_25 + post + treat_post + C(hour) + "
    "temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + "
    "sfc_sw_down_wgt + sfc_sw_down_wgt_l1"
)
m2_h = fit_ols_clustered(formula_m2_h, did_data_hourly, "street_name")
print("\n[MODEL] m2_h (Hourly + Time Controls) summary:")
print(m2_h.summary())

# DID Model + Time Varying Controls + Sensor Characteristics
formula_m3_h = (
    "Temperature ~ treat_25 + post + treat_post + C(hour) + "
    "temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + "
    "sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + "
    "Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec"
)
m3_h = fit_ols_clustered(formula_m3_h, did_data_hourly, "street_name")
print("\n[MODEL] m3_h (Hourly + All Controls) summary:")
print(m3_h.summary())

# vif(m3_h)
m3_h_plain = smf.ols(formula=formula_m3_h, data=did_data_hourly, missing="drop").fit()
X_h = m3_h_plain.model.exog
vif_data_h = pd.DataFrame({
    "Variable": m3_h_plain.model.exog_names,
    "VIF": [variance_inflation_factor(X_h, i) for i in range(X_h.shape[1])],
})
print("\n[VIF] m3_h Variance Inflation Factors:")
print(vif_data_h)
# NOTE (R comment): "removed daylight mins ~ 6 - probably bc of downward daily radiation vars?"

del m1_h, m2_h, m3_h, m3_h_plain

models_hourly = [
    smf.ols(formula="Temperature ~ treat_25 + post + treat_post", data=did_data_hourly, missing="drop").fit(),
    smf.ols(formula=formula_m2_h, data=did_data_hourly, missing="drop").fit(),
    smf.ols(formula=formula_m3_h, data=did_data_hourly, missing="drop").fit(),
]
models_hourly_clustered = [
    fit_ols_clustered("Temperature ~ treat_25 + post + treat_post", did_data_hourly, "street_name"),
    fit_ols_clustered(formula_m2_h, did_data_hourly, "street_name"),
    fit_ols_clustered(formula_m3_h, did_data_hourly, "street_name"),
]

print("\n[TABLE] models_hourly summary - text form (hour dummies omitted):")
summary_text_hourly = summary_col(
    models_hourly, stars=True, float_format="%0.3f",
    model_names=["Basic DID", "DID + Time Varying Controls", "DID + All Controls"],
    regressor_order=["Intercept", "treat_25", "post", "treat_post"],
    drop_omitted=False,
)
print(summary_text_hourly)

html_hourly_path = PYTHON_RESULTS_DIR / "06_DID_Models_Hourly.html"
build_stargazer_style_html(
    models_hourly_clustered,
    model_names=["Basic DID", "DID + Time Varying Controls", "DID + All Controls"],
    covariate_labels=covariate_labels_20m,  # same label set used in R for hourly table
    out_path=html_hourly_path,
)
log_output(html_hourly_path, len(did_data_hourly))


# ------------------------------------------------------------------------------
# 2. Daily data
# ------------------------------------------------------------------------------

required_daily_base = {"Temperature", "treat_25", "post", "treat_post", "street_name"}
missing = required_daily_base - set(did_data_daily.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data_daily.columns)}")

# Basic DID Model
m1_d = fit_ols_clustered("Temperature ~ treat_25 + post + treat_post", did_data_daily, "street_name")
print("\n[MODEL] m1_d (Daily Basic DID) summary:")
print(m1_d.summary())

# DID Model + Time Varying Controls
formula_m2_d = (
    "Temperature ~ treat_25 + post + treat_post + "
    "temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny_hours + "
    "sfc_sw_down_wgt + sfc_sw_down_wgt_l1"
)
m2_d = fit_ols_clustered(formula_m2_d, did_data_daily, "street_name")
print("\n[MODEL] m2_d (Daily + Time Controls) summary:")
print(m2_d.summary())

# DID Model + Time Varying Controls + Sensor Characteristics
formula_m3_d = (
    "Temperature ~ treat_25 + post + treat_post + "
    "temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny_hours + "
    "sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + "
    "Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec"
)
m3_d = fit_ols_clustered(formula_m3_d, did_data_daily, "street_name")
print("\n[MODEL] m3_d (Daily + All Controls) summary:")
print(m3_d.summary())

# vif(m3_d)  # NOTE (R comment): "daylight_mins close to 7 this time, otherwise all low - daylight_mins removed"
m3_d_plain = smf.ols(formula=formula_m3_d, data=did_data_daily, missing="drop").fit()
X_d = m3_d_plain.model.exog
vif_data_d = pd.DataFrame({
    "Variable": m3_d_plain.model.exog_names,
    "VIF": [variance_inflation_factor(X_d, i) for i in range(X_d.shape[1])],
})
print("\n[VIF] m3_d Variance Inflation Factors:")
print(vif_data_d)

del m1_d, m2_d, m3_d, m3_d_plain

models_daily = [
    smf.ols(formula="Temperature ~ treat_25 + post + treat_post", data=did_data_daily, missing="drop").fit(),
    smf.ols(formula=formula_m2_d, data=did_data_daily, missing="drop").fit(),
    smf.ols(formula=formula_m3_d, data=did_data_daily, missing="drop").fit(),
]
models_daily_clustered = [
    fit_ols_clustered("Temperature ~ treat_25 + post + treat_post", did_data_daily, "street_name"),
    fit_ols_clustered(formula_m2_d, did_data_daily, "street_name"),
    fit_ols_clustered(formula_m3_d, did_data_daily, "street_name"),
]

print("\n[TABLE] models_daily summary - text form (hour dummies omitted - none present in daily spec):")
summary_text_daily = summary_col(
    models_daily, stars=True, float_format="%0.3f",
    model_names=["Basic DID", "DID + Time Varying Controls", "DID + All Controls"],
    regressor_order=["Intercept", "treat_25", "post", "treat_post"],
    drop_omitted=False,
)
print(summary_text_daily)

# Daily covariate labels (RDU Total Sunny Hours instead of RDU Sunny Flag)
covariate_labels_daily = [
    "Treat", "Post", "Treat*Post", "RDU Temp. (F)", "RDU RH (%)", "RDU Tot. Precip. (in)",
    "RDU Total Sunny Hours", "Surface SW Rad. Down", "Surface SW Rad. Down Lag1",
    "Shaded", "Sidewalk", "Distance to Street (in)", "Street Width (ft)", "South-Facing",
    "North-Facing", "West-Facing", "Pct Treecover",
]

html_daily_path = PYTHON_RESULTS_DIR / "06_DID_Models_Daily.html"
build_stargazer_style_html(
    models_daily_clustered,
    model_names=["Basic DID", "DID + Time Varying Controls", "DID + All Controls"],
    covariate_labels=covariate_labels_daily,
    out_path=html_daily_path,
)
log_output(html_daily_path, len(did_data_daily))


# ==============================================================================
# VALIDATION / FINAL SUMMARY
# ==============================================================================

print("\n" + "=" * 80)
print("VALIDATION SUMMARY")
print("=" * 80)

for name, df in [
    ("did_data (20m)", did_data),
    ("did_data_hourly", did_data_hourly),
    ("did_data_daily", did_data_daily),
]:
    print(f"\n--- {name} ---")
    print(f"Shape: {df.shape}")
    print(f"Columns: {df.columns.tolist()}")
    print("First 5 rows:")
    print(df.head(5))
    if "date" in df.columns:
        print(f"Date range: {df['date'].min()} to {df['date'].max()}")
    if "street_name" in df.columns:
        print(f"Unique street_name count: {df['street_name'].nunique()}")
    if "Temperature" in df.columns:
        print(f"Temperature summary:\n{df['Temperature'].describe()}")

print("\n--- OUTPUT FILE MANIFEST ---")
for path, rows in output_manifest:
    print(f"{path} | rows/records={rows}")

# ------------------------------------------------------------------------------
# ASSUMPTIONS / NOTES
# ------------------------------------------------------------------------------
# 1. R's factor(hour)/factor(week) -> Python C(hour)/C(week) via statsmodels formula API;
#    term naming differs (R: 'factor(hour)5', patsy: 'C(hour)[T.5]'), so parse_factor_term()
#    uses patsy-style parsing instead of R's grepl/separate on "(hour)"/"(week)".
# 2. Clustered SEs replicate R's vcovCL/coeftest via statsmodels' get_robustcov_results
#    with cov_type='cluster', matching se_cluster()/se_cluster_h()/se_cluster_d() helpers.
# 3. TODO: R BUG PRESERVED - cor_matrix_d is computed from `corr_data` (20-min data), not
#    `corr_data_daily`, even though corr_data_daily is separately constructed. See inline note.
# 4. TODO: R BUG/QUIRK PRESERVED - second ggarrange() call combines the HOURLY treat*week
#    plot (pre_h_unadj_plot) with the DAILY unadjusted plot (pre_d_unadj_plot), rather than
#    two daily-specific plots. Preserved exactly as written.
# 5. models_20m/models_hourly/models_daily in R are re-fit with a (sometimes) different
#    formula spec than the standalone m1/m2/m3 printed just above them (e.g., 20m list uses
#    dewpoint_f_rdu_fill in addition to temp_f_rdu_fill, while the standalone m2/m3 do not).
#    This discrepancy is preserved exactly; both specs are fit separately in this script.
# 6. stargazer() HTML/text table generation has no direct Python equivalent; text tables use
#    statsmodels.iolib.summary2.summary_col, and HTML tables are built manually in
#    build_stargazer_style_html() to approximate stargazer(single.row=TRUE, report='vc*p'),
#    using clustered coefficients/SEs/p-values and omitting hour/week factor dummy terms
#    (matching omit="hour" in R). This is an approximation of stargazer's exact formatting,
#    not a byte-identical replication, since no Python library reproduces stargazer's HTML.
# 7. vif() computed via statsmodels variance_inflation_factor on the *unclustered* OLS design
#    matrix (VIF depends only on the design matrix, not on SE estimation method), matching
#    R's car::vif() applied to the plain lm() object.
# 8. Original R filenames the figures/results directories under ../Data/Analysis/Figures and
#    ../Data/Results (read-only originals). All Python-generated figures/tables are instead
#    written to PYTHON_FIGURES_DIR / PYTHON_RESULTS_DIR under Python_Analysis, per PATH RULE.
# 9. correlation method: pandas .corr(method='pearson') matches R's cor(..., method='pearson').
#    R's use="everything" (no missing-value handling at the cor() call, since missings were
#    already filtered out beforehand) has no direct pandas equivalent needed here since NAs
#    were already dropped in the preceding chained filters.
################################################################################