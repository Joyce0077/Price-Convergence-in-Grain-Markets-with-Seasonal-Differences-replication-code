Replication Guide

This repository contains code and data to replicate figures and tables in **Price Convergence in Grain Markets with Seasonal Differences**.

## What each script does

- networkplot_season.R – Generates **Figure 1: Market linkages** (network plot of market pairs).
- pricegap_plosone.R – Produces **Summary Table 2** and **Appendix Table 4** (price gap summaries).
- pseasonality1_plosone 2.R – Uses our price data to create **Figure 2** (seasonality from own dataset).
- pseasonality2.R – Uses FEWS NET data to create **Appendix Figure 3** (seasonality from FEWS NET).
- season_summary_plosone.R – Produces **Summary Table 1** (descriptive statistics).
- seasonality_regression.R – Runs the **main regressions** for **Table 3** and the **stationarity tests** reported in the appendix.

> Scripts are independent—no specific run order required. Open any `.R` file and run it to reproduce the associated output.

## Data files and roles

- price_season_analysis1.csv – **Main regression panel** (market-pair × item × time) with all variables used in Table 3 and stationarity tests.
- price_dt.csv – **Raw/clean price panel** (market × item × time) used to build Figure 2 and descriptive stats.
- marketpair_plosone.csv – **Appendix regression panel** (market-pair × item × time) used in Appendix Table 4.
- FEWS_NET_Staple_Food_Price_Data.xlsx – **FEWS NET price data** used to build Appendix Figure 3.
- marketfew.xls – **Metadata** for FEWS markets (market names/IDs; used by FEWS-derived plots).
- growing season.xlsx – **Growing-season calendar/metadata** used in seasonality summaries and plots.

## How to run

1. Open an `.R` script in R/RStudio.
2. Set the working directory to the repo root (`setwd(".../repo")`).
3. Run the script.
