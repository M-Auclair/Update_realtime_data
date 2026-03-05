# realtime WL data updater

This repo automatically fetches realtime WL data for stations daily 

## Overview

This project runs an R script (update_realtime_data.R) that:
- fetches realtime WL data for stations in the NWT and Mackenzie River Basin (MRB)
- saves the data as an RDS file (realtime_WL_data.rds)
- updates the file in the repo daily

## Files

-`scripts/update_realtime_data.R` - R script that fetches realtime data
- `.github/workflows/update-realtime-data.yml` - GitHub Actions workflow config
- `data/MRB_stations.rds` - Station list (required)
- `data/realtime_WL_data.rds` - Output file (auto-generated)

## Setup

1. Ensure `data/MRB_stations.rds` exists with your station list
2. Push this repository to GitHub
3. GitHub Actions will automatically run daily

## Manual trigger

You can manually trigger the workflow in your repo from the GitHub Actions tab

## Data Source

Data is sourced from Water Survey of Canada using the tidyhydat package in R
