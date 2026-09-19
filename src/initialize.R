# ==============================================================================
# Script Name: initialize.R
# Description: Environment setup and library initialization for the rainfall 
#              time-series analysis pipeline.
# Dependencies: Requires R (>= 4.0.0) and CRAN connectivity for auto-installation.
# Working Directory: Root of Project1
# ==============================================================================

# 1. Package Inventory ---------------------------------------------------------
# Define all required packages across data manipulation, spatial operations, 
# time-series analysis, visualization, and spline modeling.
required_packages <- c(
  # Data Manipulation & Wrangling
  "dplyr",             # Data manipulation grammar
  "tidyr",             # Data tidying and reshaping
  "stringr",           # String manipulation operations
  "haven",             # Reading external file formats (SAS, SPSS, Stata)
  "reshape2",          # Legacy data restructuring (flexibility for wide/long)
  "tidyverse",         # Core meta-package (includes ggplot2, dplyr, etc.)
  
  # Time Series Analysis
  "forecast",          # Time series forecasting tools and models
  "zoo",               # Infrastructure for regular/irregular time series (moving averages)
  "stats",             # Core statistical algorithms and time-series modeling
  
  # Spatial Data Analysis
  "sf",                # Simple Features spatial vector data interface
  "rnaturalearth",     # World vector map data from Natural Earth
  "rnaturalearthdata", # High-resolution supporting datasets for rnaturalearth
  
  # Statistical & Spline Modeling
  "splines",           # Base regression spline functions (B-splines, natural splines)
  "splines2",          # Advanced non-parametric spline functions
  
  # Visualization & Plot Management
  "ggplot2",           # Grammar of graphics plotting library
  "gridExtra"          # Arrangement of multiple grid-based plots
)

# 2. Automated Dependency Management -----------------------------------------
# Check for missing packages, install them automatically from CRAN, and load all.
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(missing_packages) > 0) {
  message(sprintf("Installing %d missing dependency package(s)...", length(missing_packages)))
  install.packages(missing_packages, repos = "https://cloud.r-project.org/")
} else {
  message("All required packages are already installed.")
}

# 3. Load Libraries Cleanly ---------------------------------------------------
# Suppress startup messages to keep logging clean during sequential source calls.
suppressPackageStartupMessages({
  invisible(lapply(required_packages, library, character.only = TRUE))
})

message("Environment initialized successfully. All required packages loaded.")