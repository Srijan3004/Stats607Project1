# ==============================================================================
# Script Name: hypertuning.R
# Description: Loads Mean Squared Error (MSE) cross-validation and hyperparameter 
#              tuning datasets for standard and boundary-extended spatial models.
# Dependencies: initialize.R
# ==============================================================================

# Ensure environment variables, path directories, and dependencies are initialized
if (!exists("required_packages")) {
  source("src/initialize.R")
}

# ==============================================================================
# 1. Load Hyperparameter Tuning & Cross-Validation Datasets for Kernel Smoothing
# ==============================================================================
mse_h_val_ts <- read.csv("artifacts/hyperparameter_ms/mse_h_val_ts.csv")[, -1]
mse_h_ts <- read.csv("artifacts/hyperparameter_ms/mse_h_ts.csv")[, -1]


mse_h_val_ts_ext <- read.csv("artifacts/hyperparameter_mse/mse_h_val_ts_ext.csv")[, -1]
mse_h_ts_ext <- read.csv("artifacts/hyperparameter_ms/mse_h_ts_ext.csv")[, -1]


# ======================================================================================
# 2. Load Hyperparameter Tuning & Cross-Validation Datasets for Trend Seasonality Penalty
# ======================================================================================
mse_arr <- readRDS("artifacts/hyperparameter_ms/mse_arr.Rds")
mse_arr_ext <- readRDS("artifacts/hyperparameter_ms/mse_arr_ext.Rds")

