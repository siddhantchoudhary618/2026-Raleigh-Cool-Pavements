################################################################################
# Program Name: 06b_Sensor_Models_DID_wModifiers.py
# Program Purpose: Test DID results across various time-varying and time
#                   invariant modifiers, by running separate models
# Translated from: 06b_Sensor_Models_DID_wModifiers.R
# Original Author: Katherine Burley Farr (kburley@ad.unc.edu)
# UNC Department of Public Policy, Data-Driven EnviroLab
################################################################################

from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.formula.api as smf

# ------------------------------------------------------------------------------
# 0. Paths (PROJECT_ROOT immutable; all new outputs -> Python_Analysis)
# ------------------------------------------------------------------------------

PROJECT_ROOT = Path("/Users/siddhantchoudhary/Downloads/raleigh-cool-pavements-main")
DATA_DIR = PROJECT_ROOT / "Data"

# Original (read-only) input locations - DO NOT MODIFY STRUCTURE
# NOTE (R source): original script hardcoded setwd("/Users/siddhantchoudhary/Downloads/raleigh-cool-pavements-main")
# and read paths relative to that (e.g. "Data/Analysis/..."). This absolute setwd() is
# machine-specific and not portable; per PATH RULE we resolve all inputs relative to
# PROJECT_ROOT instead of replicating the hardcoded setwd().
# TODO: R absolute setwd() path not replicated (machine-specific); using PROJECT_ROOT-relative paths.
ANALYSIS_DATA_DIR = DATA_DIR / "Analysis"
RESULTS_DIR = DATA_DIR / "Results"                       # original R results dir (read-only reference)
FIGURES_DIR = DATA_DIR / "Analysis" / "Figures"           # original R figures dir (read-only reference)

# All Python-generated outputs go here instead
PYTHON_OUTPUT_DIR = DATA_DIR / "Analysis" / "Python_Analysis_06b"
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
# Helper: clustered SE model fitting / extraction (parity with 06a)
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


def coef_ci_rows(fitted_clustered, extra_cols: dict, alpha=0.05) -> pd.DataFrame:
    """
    Replicates:
      coeffs_i <- coef(model)
      ci_i <- coefci(model, vcov=vcovCL(...))
      row_data <- data.frame(<extra_cols>, Term=names(coeffs_i), Estimate=coeffs_i,
                              CI_2.5=ci_i[,1], CI_97.5=ci_i[,2])
    """
    params = fitted_clustered.params
    conf_int = fitted_clustered.conf_int(alpha=alpha)
    names = fitted_clustered.model.exog_names
    df = pd.DataFrame({
        "Term": names,
        "Estimate": params,
        "CI_2.5": conf_int[:, 0],
        "CI_97.5": conf_int[:, 1],
    })
    for k, v in extra_cols.items():
        df[k] = v
    return df


def se_from_clustered(fitted_clustered) -> pd.Series:
    """Replicates coeftest(x, vcov=vcovCL(...))[, "Std. Error"]."""
    return pd.Series(fitted_clustered.bse, index=fitted_clustered.model.exog_names, name="Std. Error")


def build_stargazer_style_html(models_clustered, model_names, covariate_labels, out_path: Path,
                                omit_hour=True):
    """
    Approximate stargazer(type='html', single.row=TRUE, report='vc*p', omit='hour')
    using pandas to build an HTML regression table with clustered coefficients,
    clustered SEs, and p-values. Hour factor dummy terms omitted when omit_hour=True.
    """
    rows = []
    for m in models_clustered:
        names = m.model.exog_names
        params = m.params
        bse = m.bse
        pvals = m.pvalues
        d = {}
        for i, n in enumerate(names):
            if omit_hour and "C(hour)" in n:
                continue
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

    label_map = {}
    non_intercept_terms = [t for t in all_terms if t != "Intercept"]
    if covariate_labels:
        label_map = dict(zip(non_intercept_terms, covariate_labels))

    html_rows = ""
    for row in table_rows:
        term_label = label_map.get(row[0], row[0])
        cells = "".join(f"<td>{c}</td>" for c in row[1:])
        html_rows += f"<tr><td>{term_label}</td>{cells}</tr>\n"

    header_cells = "".join(f"<th>{name}</th>" for name in model_names)
    html = f"""<html><head><title>DID Modifier Models</title></head><body>
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


def plot_point_ci(df: pd.DataFrame, x_col: str, title: str, xlabel: str, ylabel: str,
                   out_path: Path, xticks_range=None, color="#004351", ecolor="#166a7c",
                   ylim=None, coord_flip=False, x_labels_map=None, dpi=300, figsize=(8, 5)):
    """
    Generic point + errorbar plot matching geom_point + geom_errorbar + geom_hline(0).
    coord_flip=True swaps axes to replicate ggplot's coord_flip().
    """
    fig, ax = plt.subplots(figsize=figsize)

    x_vals = df[x_col]
    y_vals = df["Estimate"]
    yerr = [y_vals - df["CI_2.5"], df["CI_97.5"] - y_vals]

    if coord_flip:
        # flipped: categories on y-axis, estimate on x-axis
        y_pos = np.arange(len(df))
        ax.errorbar(y_vals, y_pos, xerr=yerr, fmt="o", color=color, ecolor=ecolor,
                    elinewidth=1.5, capsize=3, markersize=6)
        ax.axvline(x=0, color="black", linestyle="--", linewidth=1)
        ax.set_yticks(y_pos)
        labels = [x_labels_map.get(str(v), str(v)) if x_labels_map else str(v) for v in x_vals]
        ax.set_yticklabels(labels)
        ax.set_xlabel(ylabel)
        ax.set_ylabel(xlabel)
        if ylim:
            ax.set_xlim(ylim)
    else:
        ax.errorbar(x_vals, y_vals, yerr=yerr, fmt="o", color=color, ecolor=ecolor,
                    elinewidth=1.5, capsize=3, markersize=6)
        ax.axhline(y=0, color="black", linestyle="--", linewidth=1)
        ax.set_xlabel(xlabel)
        ax.set_ylabel(ylabel)
        if xticks_range is not None:
            ax.set_xticks(list(xticks_range))
        if ylim:
            ax.set_ylim(ylim)

    ax.set_title(title)
    plt.tight_layout()
    fig.savefig(out_path, dpi=dpi)
    plt.close(fig)
    return out_path


# ==============================================================================
# 2. Treatment Effects by Hour
# ==============================================================================

required_hourly_full = {
    "Temperature", "treat_25", "post", "treat_post", "hour", "temp_f_rdu_fill",
    "rh_pct", "precip_in_rdu_fill", "sunny", "sfc_sw_down_wgt", "sfc_sw_down_wgt_l1",
    "Shade", "Sidewalk", "St2Pole_in", "COR_WIDTH", "sensor_direction", "pct_treec",
    "street_name",
}
missing = required_hourly_full - set(did_data_hourly.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data_hourly.columns)}")

formula_by_hour = (
    "Temperature ~ treat_25 + post + treat_post + "
    "temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + "
    "sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + "
    "Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec"
)

hours_sequence = range(0, 24)
results_rows = []

for h in hours_sequence:
    subset_h = did_data_hourly[did_data_hourly["hour"] == h]
    print(f"[LOOP] hour={h}, rows={len(subset_h)}")
    if len(subset_h) == 0:
        print(f"[WARN] hour={h} has 0 rows; skipping model fit.")
        continue
    model = fit_ols_clustered(formula_by_hour, subset_h, "street_name")
    row_df = coef_ci_rows(model, extra_cols={"Hour": h})
    results_rows.append(row_df)

results_df = pd.concat(results_rows, ignore_index=True) if results_rows else pd.DataFrame(
    columns=["Term", "Estimate", "CI_2.5", "CI_97.5", "Hour"]
)
print(f"\n[RESULTS] results_df shape: {results_df.shape}")

hourly_results = results_df[results_df["Term"] == "treat_post"].copy()

hourly_coef_plot_path = plot_point_ci(
    hourly_results, x_col="Hour",
    title="Treat-Post Coefficient in Each Hour with 95% CI",
    xlabel="Hour", ylabel="Estimate (Degrees F)",
    xticks_range=range(0, 24, 1),
    out_path=PYTHON_FIGURES_DIR / "Hourly_Coefficients.png",
)
log_output(hourly_coef_plot_path, len(hourly_results))

# Compare Treatment Effects to Other Model Coefficients by Hour:

hourly_results_shade = results_df[results_df["Term"] == "ShadeYes"].copy()
if hourly_results_shade.empty:
    print("[WARN] Term 'ShadeYes' not found; check patsy dummy-coding naming for Shade factor.")
plot_point_ci(
    hourly_results_shade, x_col="Hour",
    title="Value with 95% Confidence Intervals",
    xlabel="Hour", ylabel="Estimate (Degrees F)",
    xticks_range=range(0, 24, 1),
    color="blue", ecolor="red",
    out_path=PYTHON_FIGURES_DIR / "06b_hourly_results_shade.png",
)

hourly_results_sunny = results_df[results_df["Term"] == "sunny"].copy()
plot_point_ci(
    hourly_results_sunny, x_col="Hour",
    title="Value with 95% Confidence Intervals",
    xlabel="Hour", ylabel="Estimate",
    xticks_range=range(0, 24, 1),
    color="blue", ecolor="red",
    out_path=PYTHON_FIGURES_DIR / "06b_hourly_results_sunny.png",
)
# NOTE (R comment): "weird patterns here"

hourly_results_sfc = results_df[results_df["Term"] == "sfc_sw_down_wgt"].copy()
plot_point_ci(
    hourly_results_sfc, x_col="Hour",
    title="Value with 95% Confidence Intervals",
    xlabel="Hour", ylabel="Estimate",
    xticks_range=range(0, 24, 1),
    color="blue", ecolor="red",
    out_path=PYTHON_FIGURES_DIR / "06b_hourly_results_sfc.png",
)

hourly_results_sfcl1 = results_df[results_df["Term"] == "sfc_sw_down_wgt_l1"].copy()
plot_point_ci(
    hourly_results_sfcl1, x_col="Hour",
    title="Value with 95% Confidence Intervals",
    xlabel="Hour", ylabel="Estimate",
    xticks_range=range(0, 24, 1),
    color="blue", ecolor="red",
    out_path=PYTHON_FIGURES_DIR / "06b_hourly_results_sfcl1.png",
)


# ==============================================================================
# 3. DID with Modifiers
# ==============================================================================
# NOTE: Additional modifier models can be assessed by modifying the code in each
#       numbered section below. Base model reference (Model 3 from 06a):
#       Temperature ~ treat_25 + post + treat_post + C(hour) + temp_f_rdu_fill +
#         rh_pct + precip_in_rdu_fill + sunny + sfc_sw_down_wgt + sfc_sw_down_wgt_l1 +
#         Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec

# ------------------------------------------------------------------------------
# 3.1 Sunny vs. Not Sunny
# ------------------------------------------------------------------------------

required_sunny = {"sunny"}
missing = required_sunny - set(did_data_hourly.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data_hourly.columns)}")

formula_mod1 = (
    "Temperature ~ treat_25 + post + treat_post + C(hour) + "
    "temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + "
    "sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + "
    "Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec"
)

subset_sunny1 = did_data_hourly[did_data_hourly["sunny"] == 1].copy()
subset_sunny0 = did_data_hourly[did_data_hourly["sunny"] == 0].copy()
print(f"\n[MOD1] sunny==1 rows={len(subset_sunny1)}, sunny==0 rows={len(subset_sunny0)}")

mod1_m1_plain = smf.ols(formula=formula_mod1, data=subset_sunny1, missing="drop").fit()
mod1_m2_plain = smf.ols(formula=formula_mod1, data=subset_sunny0, missing="drop").fit()
mod1_models = [mod1_m1_plain, mod1_m2_plain]

mod1_m1_clustered = fit_ols_clustered(formula_mod1, subset_sunny1, "street_name")
mod1_m2_clustered = fit_ols_clustered(formula_mod1, subset_sunny0, "street_name")

mod1_m1_se = se_from_clustered(mod1_m1_clustered)
mod1_m2_se = se_from_clustered(mod1_m2_clustered)

covariate_labels_mod1 = [
    "Treat", "Post", "Treat*Post", "RDU Temp. (F)", "RDU RH (%)", "RDU Tot. Precip. (in)",
    "Surface SW Rad. Down", "Surface SW Rad. Down Lag1",
    "Shaded", "Sidewalk", "Distance to Street (in)", "Street Width (ft)",
    "South-Facing", "North-Facing", "West-Facing", "Pct Treecover (%)",
]
mod1_html_path = PYTHON_RESULTS_DIR / "06_Modifier_Model_1_Hourly_Sunny.html"
build_stargazer_style_html(
    [mod1_m1_clustered, mod1_m2_clustered],
    model_names=["Sunny Hours", "Not Sunny Hours"],
    covariate_labels=covariate_labels_mod1,
    out_path=mod1_html_path,
)
log_output(mod1_html_path, len(subset_sunny1) + len(subset_sunny0))

# PLOT RESULTS USING LOOP: sunny vs not sunny
sunny_levels = did_data_hourly["sunny"].unique()
sunny_results_rows = []
for s in sunny_levels:
    subset_s = did_data_hourly[did_data_hourly["sunny"] == s]
    print(f"[LOOP sunny] sunny={s}, rows={len(subset_s)}")
    model = fit_ols_clustered(formula_mod1, subset_s, "street_name")
    row_df = coef_ci_rows(model, extra_cols={"sunny": s})
    sunny_results_rows.append(row_df)

sunny_results_df = pd.concat(sunny_results_rows, ignore_index=True)
sunny_results = sunny_results_df[sunny_results_df["Term"] == "treat_post"].copy()

sunny_x_labels = {"1": "Sunny", "0": "Not Sunny"}
sunny_plot_path = plot_point_ci(
    sunny_results, x_col="sunny",
    title="Treat-Post Coefficient with 95% CI",
    xlabel="", ylabel="",
    out_path=PYTHON_FIGURES_DIR / "06b_sunny_plot.png",
    ylim=(-1, 1), coord_flip=True, x_labels_map=sunny_x_labels,
)
log_output(sunny_plot_path, len(sunny_results))


# ------------------------------------------------------------------------------
# 3.2 High vs. Low Humidity
# ------------------------------------------------------------------------------

required_rh = {"date_dt", "hour", "rh_pct", "sensor_id", "randomize"}
missing = required_rh - set(did_data_hourly.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data_hourly.columns)}")

humidity = (
    did_data_hourly[["date_dt", "hour", "rh_pct"]]
    .drop_duplicates()
    .copy()
)
# ntile(x=rh_pct, n=4) -> pd.qcut with 4 quantile bins, labeled 1..4
humidity["quantile"] = pd.qcut(humidity["rh_pct"], q=4, labels=[1, 2, 3, 4])
humidity = humidity[["date_dt", "quantile"]]

n_before_rh = len(did_data_hourly)
did_data_hourly_rh = did_data_hourly.merge(humidity, on="date_dt", how="left")
n_after_rh = len(did_data_hourly_rh)
print(f"\n[MERGE] did_data_hourly x humidity on 'date_dt': before={n_before_rh}, after={n_after_rh}")
if n_after_rh != n_before_rh:
    print(f"[MERGE WARNING] Row count changed after humidity merge: {n_before_rh} -> {n_after_rh} "
          f"(possible duplicate date_dt keys in humidity lookup)")

formula_mod2 = (
    "Temperature ~ treat_25 + post + treat_post + C(hour) + "
    "temp_f_rdu_fill + precip_in_rdu_fill + sunny + "
    "sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + "
    "Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec"
)

subset_rh_q1 = did_data_hourly_rh[did_data_hourly_rh["quantile"] == 1].copy()
subset_rh_q4 = did_data_hourly_rh[did_data_hourly_rh["quantile"] == 4].copy()
print(f"[MOD2] quantile==1 rows={len(subset_rh_q1)}, quantile==4 rows={len(subset_rh_q4)}")

mod2_m1_clustered = fit_ols_clustered(formula_mod2, subset_rh_q1, "street_name")
mod2_m2_clustered = fit_ols_clustered(formula_mod2, subset_rh_q4, "street_name")
mod2_m1_se = se_from_clustered(mod2_m1_clustered)
mod2_m2_se = se_from_clustered(mod2_m2_clustered)

covariate_labels_mod2 = [
    "Treat", "Post", "Treat*Post", "RDU Temp. (F)", "RDU Tot. Precip. (in)",
    "Sunny", "Surface SW Rad. Down", "Surface SW Rad. Down Lag1",
    "Shaded", "Sidewalk", "Distance to Street (in)", "Street Width (ft)",
    "South-Facing", "North-Facing", "West-Facing", "Pct Treecover (%)",
]
mod2_html_path = PYTHON_RESULTS_DIR / "06_Modifier_Model_1_Hourly_Humidity.html"
build_stargazer_style_html(
    [mod2_m1_clustered, mod2_m2_clustered],
    model_names=["Bottom 25% Daily RH %", "Top 25% Daily RH %"],
    covariate_labels=covariate_labels_mod2,
    out_path=mod2_html_path,
)
log_output(mod2_html_path, len(subset_rh_q1) + len(subset_rh_q4))

# PLOT RESULTS USING LOOP: rh quantile 1 vs 4 only
rhquant_levels = [1, 4]  # don't use unique(), only top/bottom quantiles
rhquant_results_rows = []
for q in rhquant_levels:
    subset_q = did_data_hourly_rh[did_data_hourly_rh["quantile"] == q]
    print(f"[LOOP rhquant] quantile={q}, rows={len(subset_q)}")
    model = fit_ols_clustered(formula_mod2, subset_q, "street_name")
    row_df = coef_ci_rows(model, extra_cols={"rhquant": q})
    rhquant_results_rows.append(row_df)

rhquant_results_df = pd.concat(rhquant_results_rows, ignore_index=True)
rhquant_results = rhquant_results_df[rhquant_results_df["Term"] == "treat_post"].copy()

rhquant_x_labels = {"1": "Bottom 25% RH", "4": "Top 25% RH"}
rhquant_plot_path = plot_point_ci(
    rhquant_results, x_col="rhquant",
    title="", xlabel="", ylabel="",
    out_path=PYTHON_FIGURES_DIR / "06b_rhquant_plot.png",
    ylim=(-1, 1), coord_flip=True, x_labels_map=rhquant_x_labels,
)
log_output(rhquant_plot_path, len(rhquant_results))


# ------------------------------------------------------------------------------
# 3.3 SW Radiation + SW Radiation Lag
# ------------------------------------------------------------------------------

required_mod3 = {
    "sfc_sw_down_wgt", "sfc_sw_down_wgt_l1", "temp_f_rdu_fill", "dewpoint_f_rdu_fill",
    "precip_in_rdu_fill", "Shade", "Sidewalk", "St2Pole_in", "COR_WIDTH",
    "sensor_direction", "pct_treec", "rh_pct", "sunny",
}
missing = required_mod3 - set(did_data_hourly.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data_hourly.columns)}")

formula_mod3 = (
    "Temperature ~ treat_25 + post + treat_post + C(hour) + "
    "temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + "
    "Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec"
)

sw = did_data_hourly["sfc_sw_down_wgt"]
sw_l1 = did_data_hourly["sfc_sw_down_wgt_l1"]

subset_mod3_hh = did_data_hourly[(sw >= 198) & (sw_l1 >= 198)].copy()
subset_mod3_hl = did_data_hourly[(sw >= 198) & (sw_l1 < 198)].copy()
subset_mod3_lh = did_data_hourly[(sw < 198) & (sw_l1 >= 198)].copy()
subset_mod3_ll = did_data_hourly[(sw < 198) & (sw_l1 < 198)].copy()
print(f"\n[MOD3] hi/hi={len(subset_mod3_hh)}, hi/lo={len(subset_mod3_hl)}, "
      f"lo/hi={len(subset_mod3_lh)}, lo/lo={len(subset_mod3_ll)}")

mod3_m1_clustered = fit_ols_clustered(formula_mod3, subset_mod3_hh, "street_name")
mod3_m2_clustered = fit_ols_clustered(formula_mod3, subset_mod3_hl, "street_name")
mod3_m3_clustered = fit_ols_clustered(formula_mod3, subset_mod3_lh, "street_name")
mod3_m4_clustered = fit_ols_clustered(formula_mod3, subset_mod3_ll, "street_name")

mod3_m1_se = se_from_clustered(mod3_m1_clustered)
mod3_m2_se = se_from_clustered(mod3_m2_clustered)
mod3_m3_se = se_from_clustered(mod3_m3_clustered)
mod3_m4_se = se_from_clustered(mod3_m4_clustered)

covariate_labels_mod3 = [
    "Treat", "Post", "Treat*Post", "RDU Temp. (F)", "RDU RH (%)", "RDU Tot. Precip. (in)",
    "RDU Sunny Flag", "Surface SW Rad. Down", "Surface SW Rad. Down Lag1",
    "Shaded", "Sidewalk", "Distance to Street (in)", "Street Width (ft)", "South-Facing",
    "North-Facing", "West-Facing", "Pct Treecover (%)",
]
mod3_html_path = PYTHON_RESULTS_DIR / "06_Modifier_Model_2_Hourly_SWRadiation.html"
build_stargazer_style_html(
    [mod3_m1_clustered, mod3_m2_clustered, mod3_m3_clustered, mod3_m4_clustered],
    model_names=["Hi SW, Hi SW Lag 1", "Hi SW, Lo SW Lag 1", "Lo SW, Hi SW Lag 1", "Lo SW, Lo SW Lag 1"],
    covariate_labels=covariate_labels_mod3,
    out_path=mod3_html_path,
)
log_output(mod3_html_path, len(subset_mod3_hh) + len(subset_mod3_hl) + len(subset_mod3_lh) + len(subset_mod3_ll))

# PLOT RESULTS USING LOOP: mod3_group categories
conditions_mod3 = [
    (sw >= 198) & (sw_l1 >= 198),
    (sw >= 198) & (sw_l1 < 198),
    (sw < 198) & (sw_l1 >= 198),
    (sw < 198) & (sw_l1 < 198),
]
choices_mod3 = [
    "Hi SW, Hi SW Lag 1", "Hi SW, Lo SW Lag 1", "Lo SW, Hi SW Lag 1", "Lo SW, Lo SW Lag 1",
]
mod3_data = did_data_hourly.copy()
mod3_data["mod3_group"] = np.select(conditions_mod3, choices_mod3, default=None)
mod3_data = mod3_data[mod3_data["mod3_group"].notna()].copy()

formula_mod3_loop = (
    "Temperature ~ treat_25 + post + treat_post + "
    "temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + "
    "Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec"
)

radiation_groups = mod3_data["mod3_group"].unique()
rad_results_rows = []
for r in radiation_groups:
    subset_r = mod3_data[mod3_data["mod3_group"] == r]
    print(f"[LOOP rad] group={r}, rows={len(subset_r)}")
    model = fit_ols_clustered(formula_mod3_loop, subset_r, "street_name")
    row_df = coef_ci_rows(model, extra_cols={"radiation_group": r})
    rad_results_rows.append(row_df)

rad_results_df = pd.concat(rad_results_rows, ignore_index=True)
rad_results = rad_results_df[rad_results_df["Term"] == "treat_post"].copy()

rad_plot_path = plot_point_ci(
    rad_results, x_col="radiation_group",
    title="", xlabel="", ylabel="",
    out_path=PYTHON_FIGURES_DIR / "06b_rad_plot.png",
    ylim=(-1, 1), coord_flip=True,
)
log_output(rad_plot_path, len(rad_results))


# ------------------------------------------------------------------------------
# 3.4 Sunny Hours + Sunny Hours Lag (Daily)
# ------------------------------------------------------------------------------

required_mod4 = {
    "sunny_hours", "sunny_hours_l1", "temp_f_rdu_fill", "dewpoint_f_rdu_fill",
    "precip_in_rdu_fill", "Shade", "Sidewalk", "St2Pole_in", "COR_WIDTH", "sensor_direction",
}
missing = required_mod4 - set(did_data_daily.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data_daily.columns)}")

formula_mod4 = (
    "Temperature ~ treat_25 + post + treat_post + "
    "temp_f_rdu_fill + dewpoint_f_rdu_fill + precip_in_rdu_fill + "
    "Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction"
)

sh = did_data_daily["sunny_hours"]
sh_l1 = did_data_daily["sunny_hours_l1"]

subset_mod4_hh = did_data_daily[(sh >= 9) & (sh_l1 >= 9)].copy()
subset_mod4_hl = did_data_daily[(sh >= 9) & (sh_l1 < 9)].copy()
subset_mod4_lh = did_data_daily[(sh < 9) & (sh_l1 >= 9)].copy()
subset_mod4_ll = did_data_daily[(sh < 9) & (sh_l1 < 9)].copy()
print(f"\n[MOD4] hi/hi={len(subset_mod4_hh)}, hi/lo={len(subset_mod4_hl)}, "
      f"lo/hi={len(subset_mod4_lh)}, lo/lo={len(subset_mod4_ll)}")

mod4_m1_clustered = fit_ols_clustered(formula_mod4, subset_mod4_hh, "street_name")
mod4_m2_clustered = fit_ols_clustered(formula_mod4, subset_mod4_hl, "street_name")
mod4_m3_clustered = fit_ols_clustered(formula_mod4, subset_mod4_lh, "street_name")
mod4_m4_clustered = fit_ols_clustered(formula_mod4, subset_mod4_ll, "street_name")

mod4_m1_se = se_from_clustered(mod4_m1_clustered)
mod4_m2_se = se_from_clustered(mod4_m2_clustered)
mod4_m3_se = se_from_clustered(mod4_m3_clustered)
mod4_m4_se = se_from_clustered(mod4_m4_clustered)

# NOTE (R source): covariate.labels for this table is commented out in the original R
# script, so stargazer would fall back to raw term names. Preserving that: no custom labels.
mod4_html_path = PYTHON_RESULTS_DIR / "06_Modifier_Model_3_Daily_SunnyHours.html"
build_stargazer_style_html(
    [mod4_m1_clustered, mod4_m2_clustered, mod4_m3_clustered, mod4_m4_clustered],
    model_names=["Hi SW, Hi SW Lag 1", "Hi SW, Lo SW Lag 1", "Lo SW, Hi SW Lag 1", "Lo SW, Lo SW Lag 1"],
    covariate_labels=None,  # TODO: R source has covariate.labels commented out for this table
    out_path=mod4_html_path,
)
log_output(mod4_html_path, len(subset_mod4_hh) + len(subset_mod4_hl) + len(subset_mod4_lh) + len(subset_mod4_ll))


# ------------------------------------------------------------------------------
# 3.5 Shaded vs. Unshaded - Hourly
# ------------------------------------------------------------------------------

required_mod5 = {
    "Shade", "temp_f_rdu_fill", "rh_pct", "precip_in_rdu_fill", "sfc_sw_down_wgt",
    "sfc_sw_down_wgt_l1", "sunny", "Sidewalk", "St2Pole_in", "COR_WIDTH",
    "sensor_direction", "pct_treec",
}
missing = required_mod5 - set(did_data_hourly.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data_hourly.columns)}")

formula_mod5 = (
    "Temperature ~ treat_25 + post + treat_post + C(hour) + "
    "temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + "
    "sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + sunny + "
    "Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec"
)

subset_shade_yes = did_data_hourly[did_data_hourly["Shade"] == "Yes"].copy()
subset_shade_no = did_data_hourly[did_data_hourly["Shade"] == "No"].copy()
print(f"\n[MOD5] Shade=='Yes' rows={len(subset_shade_yes)}, Shade=='No' rows={len(subset_shade_no)}")

mod5_m1_clustered = fit_ols_clustered(formula_mod5, subset_shade_yes, "street_name")
mod5_m2_clustered = fit_ols_clustered(formula_mod5, subset_shade_no, "street_name")
mod5_m1_se = se_from_clustered(mod5_m1_clustered)
mod5_m2_se = se_from_clustered(mod5_m2_clustered)

# NOTE (R source): covariate.labels also commented out for this table.
mod5_html_path = PYTHON_RESULTS_DIR / "06_Modifier_Model_4_Hourly_Shade.html"
build_stargazer_style_html(
    [mod5_m1_clustered, mod5_m2_clustered],
    model_names=["Shaded", "Unshaded"],
    covariate_labels=None,  # TODO: R source has covariate.labels commented out for this table
    out_path=mod5_html_path,
)
log_output(mod5_html_path, len(subset_shade_yes) + len(subset_shade_no))

# PLOT RESULTS USING LOOP: shade Yes/No
shade_levels = did_data_hourly["Shade"].unique()
shade_results_rows = []
for s in shade_levels:
    subset_s = did_data_hourly[did_data_hourly["Shade"] == s]
    print(f"[LOOP shade] Shade={s}, rows={len(subset_s)}")
    model = fit_ols_clustered(formula_mod5, subset_s, "street_name")
    row_df = coef_ci_rows(model, extra_cols={"shade": s})
    shade_results_rows.append(row_df)

shade_results_df = pd.concat(shade_results_rows, ignore_index=True)
shade_results = shade_results_df[shade_results_df["Term"] == "treat_post"].copy()

shade_x_labels = {"Yes": "Shaded", "No": "Not Shaded"}
shade_plot_path = plot_point_ci(
    shade_results, x_col="shade",
    title="", xlabel="", ylabel="Estimate (Degrees F)",
    out_path=PYTHON_FIGURES_DIR / "06b_shade_plot.png",
    ylim=(-1, 1), coord_flip=True, x_labels_map=shade_x_labels,
)
log_output(shade_plot_path, len(shade_results))


# ------------------------------------------------------------------------------
# 3.7 Percent Treecover
# ------------------------------------------------------------------------------
# NOTE (R source): section numbering skips "6" in the original script (jumps from
# "5. Shaded vs. Unshaded" directly to "7. Percent Treecover"); preserved as-is.

required_treec = {"sensor_id", "treat_25", "randomize", "pct_treec"}
missing = required_treec - set(did_data_hourly.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data_hourly.columns)}")

treecover = (
    did_data_hourly[["sensor_id", "treat_25", "randomize", "pct_treec"]]
    .drop_duplicates()
    .copy()
)
treecover["quantile"] = pd.qcut(treecover["pct_treec"], q=4, labels=[1, 2, 3, 4])

randomize_filter = [8, 17, 27, 30, 101, 1, 9, 19, 31, 35]
treecover = treecover[treecover["randomize"].isin(randomize_filter)]
treecover = treecover[["sensor_id", "randomize", "quantile"]]
print(f"\n[TREECOVER] treecover lookup rows after randomize filter: {len(treecover)}")

n_before_treec = len(did_data_hourly)
did_data_hourly_treec = did_data_hourly.merge(treecover, on=["sensor_id", "randomize"], how="left")
n_after_treec = len(did_data_hourly_treec)
print(f"[MERGE] did_data_hourly x treecover on ['sensor_id','randomize']: "
      f"before={n_before_treec}, after={n_after_treec}")
if n_after_treec != n_before_treec:
    print(f"[MERGE WARNING] Row count changed after treecover merge: "
          f"{n_before_treec} -> {n_after_treec} (possible duplicate keys)")

required_mod7 = {
    "temp_f_rdu_fill", "rh_pct", "precip_in_rdu_fill", "sunny", "sfc_sw_down_wgt",
    "sfc_sw_down_wgt_l1", "Shade", "Sidewalk", "St2Pole_in", "COR_WIDTH", "sensor_direction",
    "pct_treec",
}
missing = required_mod7 - set(did_data_hourly_treec.columns)
if missing:
    raise ValueError(f"Missing: {sorted(missing)}\nAvailable: {list(did_data_hourly_treec.columns)}")

formula_mod7 = (
    "Temperature ~ treat_25 + post + treat_post + C(hour) + "
    "temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + "
    "sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + "
    "Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction + pct_treec"
)

subset_treec_q1 = did_data_hourly_treec[did_data_hourly_treec["quantile"] == 1].copy()
subset_treec_q4 = did_data_hourly_treec[did_data_hourly_treec["quantile"] == 4].copy()
print(f"[MOD7] quantile==1 rows={len(subset_treec_q1)}, quantile==4 rows={len(subset_treec_q4)}")

mod7_m1_clustered = fit_ols_clustered(formula_mod7, subset_treec_q1, "street_name")
mod7_m2_clustered = fit_ols_clustered(formula_mod7, subset_treec_q4, "street_name")
mod7_m1_se = se_from_clustered(mod7_m1_clustered)
mod7_m2_se = se_from_clustered(mod7_m2_clustered)

# NOTE (R source): the stargazer column.labels for mod7_models say
# "Bottom 25% Daily RH %" / "Top 25% Daily RH %" even though this is the TREECOVER
# modifier table, not humidity -- this is a copy-paste label bug in the original R script.
# TODO: R BUG PRESERVED - mod7 column.labels incorrectly say "Daily RH %" instead of "Treecover".
covariate_labels_mod7 = [
    "Treat", "Post", "Treat*Post", "RDU Temp. (F)", "RDU RH (%)", "RDU Tot. Precip. (in)",
    "Sunny", "Surface SW Rad. Down", "Surface SW Rad. Down Lag1",
    "Shaded", "Sidewalk", "Distance to Street (in)", "Street Width (ft)",
    "South-Facing", "North-Facing", "West-Facing",
]
mod7_html_path = PYTHON_RESULTS_DIR / "06_Modifier_Model_7_Hourly_Treecover.html"
build_stargazer_style_html(
    [mod7_m1_clustered, mod7_m2_clustered],
    model_names=["Bottom 25% Daily RH %", "Top 25% Daily RH %"],  # TODO: R bug preserved (mislabeled)
    covariate_labels=covariate_labels_mod7,
    out_path=mod7_html_path,
)
log_output(mod7_html_path, len(subset_treec_q1) + len(subset_treec_q4))

# PLOT RESULTS USING LOOP: treecover quantile 1 vs 4 only
# NOTE (R source): this loop's model formula omits `pct_treec` itself as a control
# (unlike mod7_m1/mod7_m2 above, which include it) -- preserved exactly.
formula_mod7_loop = (
    "Temperature ~ treat_25 + post + treat_post + C(hour) + "
    "temp_f_rdu_fill + rh_pct + precip_in_rdu_fill + sunny + "
    "sfc_sw_down_wgt + sfc_sw_down_wgt_l1 + "
    "Shade + Sidewalk + St2Pole_in + COR_WIDTH + sensor_direction"
)

treecquant_levels = [1, 4]  # don't use unique(), only top/bottom quantiles
treecquant_results_rows = []
for q in treecquant_levels:
    subset_q = did_data_hourly_treec[did_data_hourly_treec["quantile"] == q]
    print(f"[LOOP treecquant] quantile={q}, rows={len(subset_q)}")
    model = fit_ols_clustered(formula_mod7_loop, subset_q, "street_name")
    row_df = coef_ci_rows(model, extra_cols={"treecquant": q})
    treecquant_results_rows.append(row_df)

treecquant_results_df = pd.concat(treecquant_results_rows, ignore_index=True)
treecquant_results = treecquant_results_df[treecquant_results_df["Term"] == "treat_post"].copy()

treecquant_x_labels = {"1": "Bottom 25% Treecover", "4": "Top 25% Treecover"}
treecquant_plot_path = plot_point_ci(
    treecquant_results, x_col="treecquant",
    title="", xlabel="", ylabel="",
    out_path=PYTHON_FIGURES_DIR / "06b_treecquant_plot.png",
    ylim=(-1, 1), coord_flip=True, x_labels_map=treecquant_x_labels,
)
log_output(treecquant_plot_path, len(treecquant_results))


# ------------------------------------------------------------------------------
# Combined Modifier Models Plot
# ------------------------------------------------------------------------------
# May include any modifier model results from above.
# ggarrange(sunny_plot, rhquant_plot, rad_plot, treecquant_plot, nrow=4, ncol=1, align="v")

fig, axes = plt.subplots(4, 1, figsize=(8, 10))

combined_specs = [
    (axes[0], sunny_results, "sunny", sunny_x_labels, "Treat-Post Coefficient with 95% CI"),
    (axes[1], rhquant_results, "rhquant", rhquant_x_labels, ""),
    (axes[2], rad_results, "radiation_group", None, ""),
    (axes[3], treecquant_results, "treecquant", treecquant_x_labels, ""),
]

for ax, df_sub, x_col, labels_map, title in combined_specs:
    x_vals = df_sub[x_col]
    y_vals = df_sub["Estimate"]
    yerr = [y_vals - df_sub["CI_2.5"], df_sub["CI_97.5"] - y_vals]
    y_pos = np.arange(len(df_sub))
    ax.errorbar(y_vals, y_pos, xerr=yerr, fmt="o", color="#004351", ecolor="#166a7c",
                elinewidth=1.5, capsize=3, markersize=6)
    ax.axvline(x=0, color="black", linestyle="--", linewidth=1)
    ax.set_yticks(y_pos)
    if labels_map:
        tick_labels = [labels_map.get(str(v), str(v)) for v in x_vals]
    else:
        tick_labels = [str(v) for v in x_vals]
    ax.set_yticklabels(tick_labels, fontsize=11)
    ax.set_xlim(-1, 1)
    ax.set_title(title)

plt.tight_layout()
combined_modifier_path = PYTHON_FIGURES_DIR / "Modifier_models.png"
fig.savefig(combined_modifier_path, dpi=300)
plt.close(fig)
log_output(combined_modifier_path,
           len(sunny_results) + len(rhquant_results) + len(rad_results) + len(treecquant_results))


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
    ("did_data_hourly_rh", did_data_hourly_rh),
    ("did_data_hourly_treec", did_data_hourly_treec),
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
# 1. R's absolute setwd("/Users/siddhantchoudhary/Downloads/raleigh-cool-pavements-main")
#    is machine-specific and not portable; per the strict PATH RULE, this script resolves
#    all inputs relative to PROJECT_ROOT = Path(__file__).resolve().parent.parent instead.
# 2. R's factor(hour) -> Python C(hour) via statsmodels formula API. Clustered SEs replicate
#    R's vcovCL/coeftest via statsmodels' get_robustcov_results(cov_type='cluster').
# 3. ntile(x, n=4) -> pd.qcut(x, q=4, labels=[1,2,3,4]), matching quartile assignment logic.
# 4. TODO: R BUG PRESERVED - the stargazer column.labels for mod7_models (Percent Treecover
#    section) read "Bottom 25% Daily RH %" / "Top 25% Daily RH %", a copy-paste artifact from
#    the humidity modifier (mod2) rather than treecover-specific labels. Preserved exactly.
# 5. TODO: R QUIRK PRESERVED - the section numbering in the original script skips "6" (jumps
#    from "5. Shaded vs. Unshaded" to "7. Percent Treecover"); no section 6 exists in source.
# 6. TODO: R QUIRK PRESERVED - the treecover PLOT LOOP formula (mod7 loop) omits `pct_treec`
#    as a control variable, while the mod7_m1/mod7_m2 models used for the stargazer table DO
#    include it. This inconsistency between the table models and the loop models is preserved
#    exactly as written in the source.
# 7. TODO: R covariate.labels commented out (left as R defaults) for mod4 (Daily SunnyHours)
#    and mod5 (Shaded vs Unshaded) stargazer tables; covariate_labels=None passed through so
#    raw model term names are used in the HTML table, matching stargazer's fallback behavior.
# 8. Two merges introduce modifier context: humidity (on 'date_dt') and treecover (on
#    ['sensor_id','randomize']). Row-count parity is explicitly checked and logged before/after
#    each merge per the MERGES rule; any row-count change is flagged as a MERGE WARNING.
# 9. coord_flip() plots (sunny_plot, rhquant_plot, rad_plot, treecquant_plot, shade_plot)
#    are implemented by plotting Estimate on the x-axis and category on the y-axis via
#    ax.errorbar(..., xerr=...) with categorical y-ticks, replicating ggplot's coord_flip().
# 10. stargazer HTML table generation approximated via build_stargazer_style_html() (see 06a
#     notes); this is not a byte-identical replication of stargazer's exact HTML formatting.
################################################################################