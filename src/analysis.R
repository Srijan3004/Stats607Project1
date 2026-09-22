# ==============================================================================
# Script Name: analysis.R
# Description: Generates PDF model diagnostic and comparison plots comparing 
#              Trend-Seasonality penalty models and Smoothing models, with and 
#              without reflecting boundaries.
# Dependencies: initialize.R, artifacts/hyperparameter_mse datasets
# Output Files: results/model_plots/Trend_SS_Pred_for_Smooth_TS.pdf
#               results/model_plots/Trend_SS_Pred_for_Smooth_TS_reflecting_boundary.pdf
# ==============================================================================

# Ensure environment variables, helper functions, data, and packages are initialized
if (!exists("required_packages")) {
  source("src/initialize.R")
}

# Source custom functions required for estimation routines
if (file.exists("artifacts/models/functions.R")) {
  source("artifacts/models/functions.R")
} else {
  warning("artifacts/models/functions.R not found. Ensure helper functions are loaded.")
}

spat_full_data <- read.csv("artifacts/sufficient-stats/spat_full_data_regions.csv")
nStn = nrow(spat_full_data)

# ==============================================================================
# Load Hyperparameter Tuning & Cross-Validation Datasets for Kernel Smoothing
# ==============================================================================
mse_h_val_ts <- read.csv("artifacts/hyperparameter_mse/mse_h_val_ts.csv")[, -1]
mse_h_ts <- read.csv("artifacts/hyperparameter_mse/mse_h_ts.csv")[, -1]


mse_h_val_ts_ext <- read.csv("artifacts/hyperparameter_mse/mse_h_val_ts_ext.csv")[, -1]
mse_h_ts_ext <- read.csv("artifacts/hyperparameter_mse/mse_h_ts_ext.csv")[, -1]


# ======================================================================================
# Load Hyperparameter Tuning & Cross-Validation Datasets for Trend Seasonality Penalty
# ======================================================================================
mse_arr <- readRDS("artifacts/hyperparameter_mse/mse_arr.Rds")
mse_arr_ext <- readRDS("artifacts/hyperparameter_mse/mse_arr_ext.Rds")

# ==============================================================================
# 1. Standard Boundary Model Plots
# ==============================================================================

pdf("results/model_plots/Trend_SS_Pred_for_Smooth_TS.pdf", width = 18, height = 10)

for (i in seq_len(nStn)) {
  if (is.na(sum(mse_arr[i, , ])) == FALSE && 
      !inherits(try(trend_ss_estimate_smoothing(i, 8, mse_h_ts[i], 9), silent = TRUE), "try-error") && 
      is.na(mse_h_ts[i]) == FALSE) {
    
    st_type <- as.character(spat_full_data[i, 4])
    mat <- mse_arr[i, , ]
    vec <- as.vector(mat)
    vmin <- min(vec)
    q <- quantile(vec, 0.1)
    which_idx <- which(mat <= q, arr.ind = TRUE)
    var_pred <- numeric()
    
    for (j in 1:nrow(which_idx)) {
      key <- paste(st_type, which_idx[j, 1], which_idx[j, 2], sep = "_")
      FF1_mat <- FF1_cache[[key]]
      ts <- trend_ss_estimate_ind_diffpenalty_train_test(
        i, 8, lambda_seq[which_idx[j, 1]], lambda_seq[which_idx[j, 2]], FF1_mat
      )
      var_pred[j] <- var(ts$predicted)
    }
    
    j_min <- which.min(var_pred)
    key <- paste(st_type, which_idx[j_min, 1], which_idx[j_min, 2], sep = "_")
    FF1_mat <- FF1_cache[[key]]
    
    penalty_result_diffpen <- trend_ss_estimate_ind_diffpenalty(
      i, 8, lambda_seq[which_idx[j_min, 1]], lambda_seq[which_idx[j_min, 2]], FF1_mat
    )
    penalty_trend_diffpen <- penalty_result_diffpen$trend
    penalty_seasonality_diffpen <- penalty_result_diffpen$seasonality_period
    penalty_pred_diffpen <- penalty_result_diffpen$predicted
    
    smooth_result <- trend_ss_estimate_smoothing(i, 8, mse_h_ts[i], 9)
    smooth_trend <- smooth_result$trend
    smooth_seasonality <- smooth_result$seasonality_period
    smooth_pred <- smooth_result$predicted
    actual_data <- smooth_result$rain
    
    penalty_error_diffpen <- actual_data - penalty_pred_diffpen
    
    plot_data <- data.frame(
      x = seq_along(penalty_trend_diffpen),
      Actual = actual_data,
      TrendSSPenalty = penalty_pred_diffpen,
      TrendSSPenalty_trend = penalty_trend_diffpen,
      smooth_pred = smooth_pred,
      smooth_trend = smooth_trend
    )
    
    rmse_TS <- sqrt(mean((plot_data$Actual - plot_data$TrendSSPenalty)^2, na.rm = TRUE))
    mae_TS  <- mean(abs(plot_data$Actual - plot_data$TrendSSPenalty), na.rm = TRUE)
    rmse_smooth <- sqrt(mean((plot_data$Actual - plot_data$smooth_pred)^2, na.rm = TRUE))
    mae_smooth  <- mean(abs(plot_data$Actual - plot_data$smooth_pred), na.rm = TRUE)
    
    sd1_TS <- round(sd(penalty_seasonality_diffpen$V1, na.rm = TRUE), 2)
    sd2_TS <- round(sd(penalty_seasonality_diffpen$V2, na.rm = TRUE), 2)
    sd3_TS <- round(sd(penalty_seasonality_diffpen$V3, na.rm = TRUE), 2)
    sd4_TS <- round(sd(penalty_seasonality_diffpen$V4, na.rm = TRUE), 2)
    
    sd1_smooth <- round(sd(smooth_seasonality$V1, na.rm = TRUE), 2)
    sd2_smooth <- round(sd(smooth_seasonality$V2, na.rm = TRUE), 2)
    sd3_smooth <- round(sd(smooth_seasonality$V3, na.rm = TRUE), 2)
    sd4_smooth <- round(sd(smooth_seasonality$V4, na.rm = TRUE), 2)
    
    p1 <- ggplot(penalty_seasonality_diffpen) +
      geom_line(aes(x = 1:12, y = V1, colour = "1901-1930"), linewidth = 1.2) +
      geom_line(aes(x = 1:12, y = V2, colour = "1931-1960"), linewidth = 1.2) +
      geom_line(aes(x = 1:12, y = V3, colour = "1961-1990"), linewidth = 1.2) +
      geom_line(aes(x = 1:12, y = V4, colour = "1991-2020"), linewidth = 1.2) +
      labs(
        x = "Months",
        y = "Seasonality",
        title = paste("TS Seasonality Estimate for", spat_full_data[i, 3]),
        subtitle = paste0(
          "SD 1st: ", sd1_TS,
          " | 2nd: ", sd2_TS,
          " | 3rd: ", sd3_TS,
          " | 4th: ", sd4_TS
        ),
        color = "Time Period"
      ) +
      scale_color_manual(values = c(
        "1901-1930" = "#1f78b4",
        "1931-1960" = "#33a02c",
        "1961-1990" = "#e31a1c",
        "1991-2020" = "#ff7f00"
      )) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
        axis.text.x   = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y   = element_text(size = 14),
        axis.title.x  = element_text(size = 16, face = "bold"),
        axis.title.y  = element_text(size = 16, face = "bold"),
        legend.title  = element_text(size = 18, face = "bold"),
        legend.text   = element_text(size = 16),
        legend.position = "bottom",
        legend.box      = "horizontal"
      )
    
    p2 <- smooth_seasonality %>%
      ggplot() +
      geom_line(aes(x = 1:12, y = V1, colour = "1901-1930"), linewidth = 1.2) +
      geom_line(aes(x = 1:12, y = V2, colour = "1931-1960"), linewidth = 1.2) +
      geom_line(aes(x = 1:12, y = V3, colour = "1961-1990"), linewidth = 1.2) +
      geom_line(aes(x = 1:12, y = V4, colour = "1991-2020"), linewidth = 1.2) +
      labs(
        x = "Months",
        y = "Seasonality",
        title = paste("Smoothing Seasonality Estimate for", spat_full_data[i, 3]),
        subtitle = paste0(
          "SD 1st: ", sd1_smooth,
          " | 2nd: ", sd2_smooth,
          " | 3rd: ", sd3_smooth,
          " | 4th: ", sd4_smooth
        ),
        color = "Time Period"
      ) +
      scale_color_manual(values = c(
        "1901-1930" = "#1f78b4",
        "1931-1960" = "#33a02c",
        "1961-1990" = "#e31a1c",
        "1991-2020" = "#ff7f00"
      )) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
        axis.text.x   = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y   = element_text(size = 14),
        axis.title.x  = element_text(size = 16, face = "bold"),
        axis.title.y  = element_text(size = 16, face = "bold"),
        legend.title  = element_text(size = 18, face = "bold"),
        legend.text   = element_text(size = 16),
        legend.position = "bottom",
        legend.box      = "horizontal"
      )
    
    p3 <- ggplot(plot_data, aes(x = x)) +
      geom_line(aes(y = Actual, color = "Actual"), linewidth = 1.3, alpha = 0.95) +
      geom_line(aes(y = TrendSSPenalty, color = "Predicted"), linewidth = 1, alpha = 0.6) +
      labs(
        title = paste("TS Prediction vs Actual for", spat_full_data[i, 3]),
        subtitle = paste("RMSE:", round(rmse_TS, 2), "| MAE:", round(mae_TS, 2)),
        x = "Index",
        y = "Value",
        color = "Method"
      ) +
      scale_color_manual(values = c(
        "Actual"    = "#003f5c",
        "Predicted" = "#ffa600"
      )) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
        axis.text.x   = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y   = element_text(size = 12),
        axis.title.x  = element_text(size = 16, face = "bold"),
        axis.title.y  = element_text(size = 16, face = "bold"),
        legend.title  = element_text(size = 18, face = "bold"),
        legend.text   = element_text(size = 16),
        legend.position = "bottom",
        legend.box    = "horizontal"
      )
    
    p4 <- ggplot(plot_data, aes(x = x)) +
      geom_line(aes(y = Actual, color = "Actual"), linewidth = 1.3, alpha = 0.95) +
      geom_line(aes(y = smooth_pred, color = "Predicted"), linewidth = 1, alpha = 0.6) +
      labs(
        title = paste("Smoothed Prediction vs Actual for", spat_full_data[i, 3]),
        subtitle = paste("RMSE:", round(rmse_smooth, 2), "| MAE:", round(mae_smooth, 2)),
        x = "Index",
        y = "Value",
        color = "Method"
      ) +
      scale_color_manual(values = c(
        "Actual"    = "#003f5c",
        "Predicted" = "#ffa600"
      )) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
        axis.text.x   = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y   = element_text(size = 12),
        axis.title.x  = element_text(size = 16, face = "bold"),
        axis.title.y  = element_text(size = 16, face = "bold"),
        legend.title  = element_text(size = 18, face = "bold"),
        legend.text   = element_text(size = 16),
        legend.position = "bottom",
        legend.box    = "horizontal"
      )
    
    rmse_TS_smooth <- sqrt(mean((plot_data$TrendSSPenalty_trend - plot_data$smooth_trend)^2, na.rm = TRUE))
    mae_TS_smooth <- mean(abs(plot_data$TrendSSPenalty_trend - plot_data$smooth_trend), na.rm = TRUE)
    
    p5 <- ggplot(plot_data, aes(x = x)) +
      geom_line(aes(y = TrendSSPenalty_trend, color = "TS"), linewidth = 1) +
      geom_line(aes(y = smooth_trend, color = "Smoothing"), linewidth = 1) +
      labs(
        title = paste("Trend Estimates for ", spat_full_data[i, 3]),
        subtitle = paste("RMSE:", round(rmse_TS_smooth, 2), "| MAE:", round(mae_TS_smooth, 2)),
        x = "Index",
        y = "Value",
        color = "Method"
      ) +
      scale_color_manual(values = c(
        "Smoothing" = "#4A90E2",
        "TS"        = "#ffa600"
      )) +
      theme_minimal() +
      theme(
        plot.title    = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
        axis.text.x   = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y   = element_text(size = 12),
        axis.title.x  = element_text(size = 16, face = "bold"),
        axis.title.y  = element_text(size = 16, face = "bold"),
        legend.title  = element_text(size = 18, face = "bold"),
        legend.text   = element_text(size = 16),
        legend.position = "bottom",
        legend.box    = "horizontal"
      )
    
    print(p3)
    print(p4)
    print(p5)
    print(p1)
    print(p2)
  }
}

dev.off()


# ==============================================================================
# 2. Reflecting Boundary Model Plots
# ==============================================================================

pdf("results/model_plots/Trend_SS_Pred_for_Smooth_TS_reflecting_boundary.pdf", width = 18, height = 10)

for (i in seq_len(nStn)) {
  if (is.na(sum(mse_arr_ext[i, , ])) == FALSE && 
      !inherits(try(trend_ss_estimate_smoothing_ext(i, 8, mse_h_ts[i], 9), silent = TRUE), "try-error") && 
      is.na(mse_h_ts[i]) == FALSE) {
    
    st_type <- as.character(spat_full_data[i, 4])
    mat <- mse_arr_ext[i, , ]
    vec <- as.vector(mat)
    vmin <- min(vec)
    q <- quantile(vec, 0.1)
    which_idx <- which(mat <= q, arr.ind = TRUE)
    var_pred <- numeric()
    
    for (j in 1:nrow(which_idx)) {
      key <- paste(st_type, which_idx[j, 1], which_idx[j, 2], sep = "_")
      FF1_mat <- FF1_cache_ext[[key]]
      ts <- trend_ss_estimate_ind_diffpenalty_train_test_ext(
        i, 8, lambda_seq[which_idx[j, 1]], lambda_seq[which_idx[j, 2]], FF1_mat
      )
      var_pred[j] <- var(ts$predicted)
    }
    
    j_min <- which.min(var_pred)
    key <- paste(st_type, which_idx[j_min, 1], which_idx[j_min, 2], sep = "_")
    FF1_mat <- FF1_cache_ext[[key]]
    
    penalty_result_diffpen <- trend_ss_estimate_ind_diffpenalty_ext(
      i, 8, lambda_seq[which_idx[j_min, 1]], lambda_seq[which_idx[j_min, 2]], FF1_mat
    )
    penalty_trend_diffpen <- penalty_result_diffpen$trend
    penalty_seasonality_diffpen <- penalty_result_diffpen$seasonality_period
    penalty_pred_diffpen <- penalty_result_diffpen$predicted
    
    smooth_result <- trend_ss_estimate_smoothing_ext(i, 8, mse_h_ts[i], 9)
    smooth_trend <- smooth_result$trend
    smooth_seasonality <- smooth_result$seasonality_period
    smooth_pred <- smooth_result$predicted
    actual_data <- smooth_result$rain
    
    penalty_error_diffpen <- actual_data - penalty_pred_diffpen
    
    plot_data <- data.frame(
      x = seq_along(penalty_trend_diffpen),
      Actual = actual_data,
      TrendSSPenalty = penalty_pred_diffpen,
      TrendSSPenalty_trend = penalty_trend_diffpen,
      smooth_pred = smooth_pred,
      smooth_trend = smooth_trend
    )
    
    rmse_TS <- sqrt(mean((plot_data$Actual - plot_data$TrendSSPenalty)^2, na.rm = TRUE))
    mae_TS  <- mean(abs(plot_data$Actual - plot_data$TrendSSPenalty), na.rm = TRUE)
    rmse_smooth <- sqrt(mean((plot_data$Actual - plot_data$smooth_pred)^2, na.rm = TRUE))
    mae_smooth  <- mean(abs(plot_data$Actual - plot_data$smooth_pred), na.rm = TRUE)
    
    sd1_TS <- round(sd(penalty_seasonality_diffpen$V1, na.rm = TRUE), 2)
    sd2_TS <- round(sd(penalty_seasonality_diffpen$V2, na.rm = TRUE), 2)
    sd3_TS <- round(sd(penalty_seasonality_diffpen$V3, na.rm = TRUE), 2)
    sd4_TS <- round(sd(penalty_seasonality_diffpen$V4, na.rm = TRUE), 2)
    
    sd1_smooth <- round(sd(smooth_seasonality$V1, na.rm = TRUE), 2)
    sd2_smooth <- round(sd(smooth_seasonality$V2, na.rm = TRUE), 2)
    sd3_smooth <- round(sd(smooth_seasonality$V3, na.rm = TRUE), 2)
    sd4_smooth <- round(sd(smooth_seasonality$V4, na.rm = TRUE), 2)
    
    p1 <- ggplot(penalty_seasonality_diffpen) +
      geom_line(aes(x = 1:12, y = V1, colour = "1901-1930"), linewidth = 1.2) +
      geom_line(aes(x = 1:12, y = V2, colour = "1931-1960"), linewidth = 1.2) +
      geom_line(aes(x = 1:12, y = V3, colour = "1961-1990"), linewidth = 1.2) +
      geom_line(aes(x = 1:12, y = V4, colour = "1991-2020"), linewidth = 1.2) +
      labs(
        x = "Months",
        y = "Seasonality",
        title = paste("TS Seasonality Estimate for", spat_full_data[i, 3]),
        subtitle = paste0(
          "SD 1st: ", sd1_TS,
          " | 2nd: ", sd2_TS,
          " | 3rd: ", sd3_TS,
          " | 4th: ", sd4_TS
        ),
        color = "Time Period"
      ) +
      scale_color_manual(values = c(
        "1901-1930" = "#1f78b4",
        "1931-1960" = "#33a02c",
        "1961-1990" = "#e31a1c",
        "1991-2020" = "#ff7f00"
      )) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
        axis.text.x   = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y   = element_text(size = 14),
        axis.title.x  = element_text(size = 16, face = "bold"),
        axis.title.y  = element_text(size = 16, face = "bold"),
        legend.title  = element_text(size = 18, face = "bold"),
        legend.text   = element_text(size = 16),
        legend.position = "bottom",
        legend.box      = "horizontal"
      )
    
    p2 <- smooth_seasonality %>%
      ggplot() +
      geom_line(aes(x = 1:12, y = V1, colour = "1901-1930"), linewidth = 1.2) +
      geom_line(aes(x = 1:12, y = V2, colour = "1931-1960"), linewidth = 1.2) +
      geom_line(aes(x = 1:12, y = V3, colour = "1961-1990"), linewidth = 1.2) +
      geom_line(aes(x = 1:12, y = V4, colour = "1991-2020"), linewidth = 1.2) +
      labs(
        x = "Months",
        y = "Seasonality",
        title = paste("Smoothing Seasonality Estimate for", spat_full_data[i, 3]),
        subtitle = paste0(
          "SD 1st: ", sd1_smooth,
          " | 2nd: ", sd2_smooth,
          " | 3rd: ", sd3_smooth,
          " | 4th: ", sd4_smooth
        ),
        color = "Time Period"
      ) +
      scale_color_manual(values = c(
        "1901-1930" = "#1f78b4",
        "1931-1960" = "#33a02c",
        "1961-1990" = "#e31a1c",
        "1991-2020" = "#ff7f00"
      )) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
        axis.text.x   = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y   = element_text(size = 14),
        axis.title.x  = element_text(size = 16, face = "bold"),
        axis.title.y  = element_text(size = 16, face = "bold"),
        legend.title  = element_text(size = 18, face = "bold"),
        legend.text   = element_text(size = 16),
        legend.position = "bottom",
        legend.box      = "horizontal"
      )
    
    p3 <- ggplot(plot_data, aes(x = x)) +
      geom_line(aes(y = Actual, color = "Actual"), linewidth = 1.3, alpha = 0.95) +
      geom_line(aes(y = TrendSSPenalty, color = "Predicted"), linewidth = 1, alpha = 0.6) +
      geom_vline(xintercept = c(120, 1561), linetype = "dashed", color = "violet", linewidth = 1.2) +
      labs(
        title = paste("TS Prediction vs Actual for", spat_full_data[i, 3]),
        subtitle = paste("RMSE:", round(rmse_TS, 2), "| MAE:", round(mae_TS, 2)),
        x = "Index",
        y = "Value",
        color = "Method"
      ) +
      scale_color_manual(values = c(
        "Actual"    = "#003f5c",
        "Predicted" = "#ffa600"
      )) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
        axis.text.x   = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y   = element_text(size = 12),
        axis.title.x  = element_text(size = 16, face = "bold"),
        axis.title.y  = element_text(size = 16, face = "bold"),
        legend.title  = element_text(size = 18, face = "bold"),
        legend.text   = element_text(size = 16),
        legend.position = "bottom",
        legend.box    = "horizontal"
      )
    
    p4 <- ggplot(plot_data, aes(x = x)) +
      geom_line(aes(y = Actual, color = "Actual"), linewidth = 1.3, alpha = 0.95) +
      geom_line(aes(y = smooth_pred, color = "Predicted"), linewidth = 1, alpha = 0.6) +
      geom_vline(xintercept = c(120, 1561), linetype = "dashed", color = "violet", linewidth = 1.2) +
      labs(
        title = paste("Smoothed Prediction vs Actual for", spat_full_data[i, 3]),
        subtitle = paste("RMSE:", round(rmse_smooth, 2), "| MAE:", round(mae_smooth, 2)),
        x = "Index",
        y = "Value",
        color = "Method"
      ) +
      scale_color_manual(values = c(
        "Actual"    = "#003f5c",
        "Predicted" = "#ffa600"
      )) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title    = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
        axis.text.x   = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y   = element_text(size = 12),
        axis.title.x  = element_text(size = 16, face = "bold"),
        axis.title.y  = element_text(size = 16, face = "bold"),
        legend.title  = element_text(size = 18, face = "bold"),
        legend.text   = element_text(size = 16),
        legend.position = "bottom",
        legend.box    = "horizontal"
      )
    
    rmse_TS_smooth <- sqrt(mean((plot_data$TrendSSPenalty_trend - plot_data$smooth_trend)^2, na.rm = TRUE))
    mae_TS_smooth <- mean(abs(plot_data$TrendSSPenalty_trend - plot_data$smooth_trend), na.rm = TRUE)
    
    p5 <- ggplot(plot_data, aes(x = x)) +
      geom_line(aes(y = TrendSSPenalty_trend, color = "TS"), linewidth = 1) +
      geom_line(aes(y = smooth_trend, color = "Smoothing"), linewidth = 1) +
      geom_vline(xintercept = c(120, 1561), linetype = "dashed", color = "violet", linewidth = 1.2) +
      labs(
        title = paste("Trend Estimates for ", spat_full_data[i, 3]),
        subtitle = paste("RMSE:", round(rmse_TS_smooth, 2), "| MAE:", round(mae_TS_smooth, 2)),
        x = "Index",
        y = "Value",
        color = "Method"
      ) +
      scale_color_manual(values = c(
        "Smoothing" = "#4A90E2",
        "TS"        = "#ffa600"
      )) +
      theme_minimal() +
      theme(
        plot.title    = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
        axis.text.x   = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y   = element_text(size = 12),
        axis.title.x  = element_text(size = 16, face = "bold"),
        axis.title.y  = element_text(size = 16, face = "bold"),
        legend.title  = element_text(size = 18, face = "bold"),
        legend.text   = element_text(size = 16),
        legend.position = "bottom",
        legend.box    = "horizontal"
      )
    
    print(p3)
    print(p4)
    print(p5)
    print(p1)
    print(p2)
  }
}

dev.off()