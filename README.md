# Ebola Outbreak Tracker

This repository contains a simple Shiny application for visualizing the 
2014-2015 Ebola outbreak. It is designed for analyzing about data 
visualization and basic public health tracking.

## Dataset
The app uses `country_timeseries.csv`. It tracks the number of Cases and 
Deaths in affected countries.

## Requirements
- R (>= 4.0)
- RStudio
- Internet connection (for automatic package installation)
- Packages: `shiny`, `ggplot2`, `dplyr`, `tidyr`

## How to run
1. Keep `country_timeseries.csv` and `app.R` in the same folder.
2. Open `app.R` in RStudio.
3. Click **Run App**.

The app automatically installs required packages if they are missing.
