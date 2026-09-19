# ==============================================================================
# Script Name: functions.R
# Description: Generates Spline and Cosine Basis Matrices (standard and extended)
#              and provides utility functions for Kernel Smoothing and 
#              Penalized Estimation across Spatial Weather Stations.
# Dependencies: splines2, dplyr
# ==============================================================================

# Ensure environment and packages are loaded
if (!exists("required_packages")) {
  source("src/initialize.R")
}

library(splines2)
library(dplyr)

# ==============================================================================
# 1. Standard Basis Functions Construction (1901 - 2020)
# ==============================================================================
df <- 8
ss_period <- 4
tot_x <- 1:length(rep(seq(1901, 2020), each = 12))

# Trend Basis via B-Splines
trend_basis <- bSpline(tot_x, df = df, degree = 3, intercept = TRUE)

# Seasonality Basis via Cosine Expansion
len <- length(tot_x) / ss_period
seasonality_basis <- matrix(0, nrow = length(tot_x), ncol = 12 * ss_period)

for (block in 1:ss_period) {
  seasonality_basis[seq(len * (block - 1) + 1, len * block, 1), seq(12 * (block - 1) + 1, 12 * block, 1)] <- 
    cbind(
      cos(pi * 0.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 1.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 2.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 3.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 4.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 5.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 6.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 7.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 8.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 9.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 10.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 11.5 * (c(1:len) %% 12 + 0.5) / 12)
    )
}

seasonality_basis_1 <- seasonality_basis
rowsum_ssbs <- apply(seasonality_basis[1:12, 1:12], 2, sum)
rowsum_ssbs <- rowsum_ssbs / rowsum_ssbs[12]
rowsum_ssbs <- c(rowsum_ssbs[12], rowsum_ssbs[1:11])

for (i in 1:(ncol(seasonality_basis))) {
  seasonality_basis_1[, i] <- seasonality_basis_1[, i] - rowsum_ssbs[i %% 12 + 1] * seasonality_basis_1[, ncol(seasonality_basis_1)]
}
seasonality_basis_1 <- seasonality_basis_1[, -(ss_period * 12)]

# Combined Trend-Seasonality Basis Matrix
trend_ss_basis <- cbind(trend_basis, seasonality_basis_1)


# ==============================================================================
# 2. Standard Station Matrix Builders
# ==============================================================================
trend_ss_matrix <- function(s, df) {
  station <- as.character(spat_full_data[s, 3])
  station_month_ind <- which(data_monthly_agg$STATION == station)
  dat_station_month <- as.data.frame(data_monthly_agg[station_month_ind, -c(1:4)])
  start_year_station <- 1901
  end_year_station <- 2020
  missing_year <- setdiff(c(start_year_station:end_year_station), dat_station_month$YEAR)
  
  new_dat_station_month <- matrix(0, nrow = (end_year_station - start_year_station + 1), ncol = ncol(dat_station_month))
  new_dat_station_month[, 1] <- c(start_year_station:end_year_station)
  
  for (i in 1:nrow(new_dat_station_month)) {
    if (new_dat_station_month[i, 1] %in% dat_station_month$YEAR) {
      new_dat_station_month[i, ] <- as.numeric(dat_station_month[which(dat_station_month$YEAR == new_dat_station_month[i, 1])[1], ])
    } else {
      new_dat_station_month[i, 2:13] <- NA
    }
  }
  
  dat_station_month <- new_dat_station_month
  station_rain <- as.numeric(t(dat_station_month[, -1]))
  year_station <- rep(c(start_year_station:end_year_station), each = 12)
  month <- rep(1:12, (end_year_station - start_year_station + 1))
  df_station <- cbind.data.frame(year_station, month, station_rain)
  station_data <- df_station[, c(1, 2, 3)]
  
  x <- 1:length(rep(seq(start_year_station, end_year_station), each = 12))
  y <- station_data$station_rain
  w <- ifelse(is.na(y), 0, 1)
  station_data$indicator <- w
  trend_ss_basis_s <- trend_ss_basis * w
  
  return(list(rain = y, basis = trend_ss_basis_s, trend_ss_mat = t(trend_ss_basis_s) %*% trend_ss_basis_s, indicator = w))
}

trend_ss_matrix_train_test <- function(s, df, ratio = 0.9) {
  station <- as.character(spat_full_data[s, 3])
  station_month_ind <- which(data_monthly_agg$STATION == station)
  dat_station_month <- as.data.frame(data_monthly_agg[station_month_ind, -c(1:4)])
  start_year_station <- 1901
  end_year_station <- 2020
  missing_year <- setdiff(c(start_year_station:end_year_station), dat_station_month$YEAR)
  
  new_dat_station_month <- matrix(0, nrow = (end_year_station - start_year_station + 1), ncol = ncol(dat_station_month))
  new_dat_station_month[, 1] <- c(start_year_station:end_year_station)
  
  for (i in 1:nrow(new_dat_station_month)) {
    if (new_dat_station_month[i, 1] %in% dat_station_month$YEAR) {
      new_dat_station_month[i, ] <- as.numeric(dat_station_month[which(dat_station_month$YEAR == new_dat_station_month[i, 1])[1], ])
    } else {
      new_dat_station_month[i, 2:13] <- NA
    }
  }
  
  dat_station_month <- new_dat_station_month
  station_rain <- as.numeric(t(dat_station_month[, -1]))
  
  non_na <- which(!is.na(station_rain))
  perm <- sample(non_na, length(non_na))
  i90 <- perm[1:floor(ratio * length(perm))]
  i10 <- perm[(floor(ratio * length(perm)) + 1):length(perm)]
  
  train <- replace(station_rain, -i90, NA)
  test <- replace(station_rain, -i10, NA)
  
  year_station <- rep(c(start_year_station:end_year_station), each = 12)
  month <- rep(1:12, (end_year_station - start_year_station + 1))
  df_station_train <- cbind.data.frame(year_station, month, train)
  station_data <- df_station_train[, c(1, 2, 3)]
  
  x <- 1:length(rep(seq(start_year_station, end_year_station), each = 12))
  y <- station_data$train
  w <- ifelse(is.na(y), 0, 1)
  station_data$indicator <- w
  trend_ss_basis_s <- trend_ss_basis * w
  
  return(list(rain = y, basis = trend_ss_basis_s, trend_ss_mat = t(trend_ss_basis_s) %*% trend_ss_basis_s, indicator = w, test = test))
}


# ==============================================================================
# 3. Extended Basis Functions Construction (Reflecting Boundary: 1891 - 2030)
# ==============================================================================
tot_x_ext <- 1:length(rep(seq(1891, 2030), each = 12))
trend_basis_ext <- bSpline(tot_x_ext, df = df, degree = 3, intercept = TRUE)
ss_period <- 4
len <- length(tot_x_ext) / ss_period
seasonality_basis_ext <- matrix(0, nrow = length(tot_x_ext), ncol = 12 * ss_period)

for (block in 1:ss_period) {
  seasonality_basis_ext[seq(len * (block - 1) + 1, len * block, 1), seq(12 * (block - 1) + 1, 12 * block, 1)] <- 
    cbind(
      cos(pi * 0.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 1.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 2.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 3.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 4.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 5.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 6.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 7.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 8.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 9.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 10.5 * (c(1:len) %% 12 + 0.5) / 12),
      cos(pi * 11.5 * (c(1:len) %% 12 + 0.5) / 12)
    )
}

seasonality_basis_1_ext <- seasonality_basis_ext
rowsum_ssbs_ext <- apply(seasonality_basis_ext[1:12, 1:12], 2, sum)
rowsum_ssbs_ext <- rowsum_ssbs_ext / rowsum_ssbs_ext[12]
rowsum_ssbs_ext <- c(rowsum_ssbs_ext[12], rowsum_ssbs_ext[1:11])

for (i in 1:(ncol(seasonality_basis_ext))) {
  seasonality_basis_1_ext[, i] <- seasonality_basis_1_ext[, i] - rowsum_ssbs_ext[i %% 12 + 1] * seasonality_basis_ext[, ncol(seasonality_basis_ext)]
}
seasonality_basis_1_ext <- seasonality_basis_1_ext[, -(ss_period * 12)]

trend_ss_basis_ext <- cbind(trend_basis_ext, seasonality_basis_1_ext)


# ==============================================================================
# 4. Extended Station Matrix Builders
# ==============================================================================
trend_ss_matrix_ext <- function(s, df) {
  station <- as.character(spat_full_data[s, 3])
  station_month_ind <- which(data_monthly_agg$STATION == station)
  dat_station_month <- as.data.frame(data_monthly_agg[station_month_ind, -c(1:4)])
  start_year_station <- 1891
  end_year_station <- 2030
  missing_year <- setdiff(c(start_year_station:end_year_station), dat_station_month$YEAR)
  
  new_dat_station_month <- matrix(0, nrow = (end_year_station - start_year_station + 1), ncol = ncol(dat_station_month))
  new_dat_station_month[, 1] <- c(start_year_station:end_year_station)
  
  for (i in 1:nrow(new_dat_station_month)) {
    if (new_dat_station_month[i, 1] %in% dat_station_month$YEAR) {
      new_dat_station_month[i, ] <- as.numeric(dat_station_month[which(dat_station_month$YEAR == new_dat_station_month[i, 1])[1], ])
    } else {
      new_dat_station_month[i, 2:13] <- NA
    }
  }
  
  dat_station_month <- new_dat_station_month
  dat_station_month[1:10, 1:12] <- dat_station_month[11:20, 1:12]
  dat_station_month[131:140, 1:12] <- dat_station_month[121:130, 1:12]
  
  station_rain <- as.numeric(t(dat_station_month[, -1]))
  year_station <- rep(c(start_year_station:end_year_station), each = 12)
  month <- rep(1:12, (end_year_station - start_year_station + 1))
  df_station <- cbind.data.frame(year_station, month, station_rain)
  station_data <- df_station[, c(1, 2, 3)]
  
  x <- 1:length(rep(seq(start_year_station, end_year_station), each = 12))
  y <- station_data$station_rain
  w <- ifelse(is.na(y), 0, 1)
  station_data$indicator <- w
  trend_ss_basis_s <- trend_ss_basis_ext * w
  
  return(list(rain = y, basis = trend_ss_basis_s, trend_ss_mat = t(trend_ss_basis_s) %*% trend_ss_basis_s, indicator = w))
}

trend_ss_matrix_train_test_ext <- function(s, df, ratio = 0.9) {
  station <- as.character(spat_full_data[s, 3])
  station_month_ind <- which(data_monthly_agg$STATION == station)
  dat_station_month <- as.data.frame(data_monthly_agg[station_month_ind, -c(1:4)])
  start_year_station <- 1891
  end_year_station <- 2030
  missing_year <- setdiff(c(start_year_station:end_year_station), dat_station_month$YEAR)
  
  new_dat_station_month <- matrix(0, nrow = (end_year_station - start_year_station + 1), ncol = ncol(dat_station_month))
  new_dat_station_month[, 1] <- c(start_year_station:end_year_station)
  
  for (i in 1:nrow(new_dat_station_month)) {
    if (new_dat_station_month[i, 1] %in% dat_station_month$YEAR) {
      new_dat_station_month[i, ] <- as.numeric(dat_station_month[which(dat_station_month$YEAR == new_dat_station_month[i, 1])[1], ])
    } else {
      new_dat_station_month[i, 2:13] <- NA
    }
  }
  
  dat_station_month <- new_dat_station_month
  dat_station_month[1:10, 1:12] <- dat_station_month[11:20, 1:12]
  dat_station_month[131:140, 1:12] <- dat_station_month[121:130, 1:12]
  
  station_rain <- as.numeric(t(dat_station_month[, -1]))
  
  non_na <- which(!is.na(station_rain))
  perm <- sample(non_na, length(non_na))
  i90 <- perm[1:floor(ratio * length(perm))]
  i10 <- perm[(floor(ratio * length(perm)) + 1):length(perm)]
  
  train <- replace(station_rain, -i90, NA)
  test <- replace(station_rain, -i10, NA)
  
  year_station <- rep(c(start_year_station:end_year_station), each = 12)
  month <- rep(1:12, (end_year_station - start_year_station + 1))
  df_station_train <- cbind.data.frame(year_station, month, train)
  station_data <- df_station_train[, c(1, 2, 3)]
  
  x <- 1:length(rep(seq(start_year_station, end_year_station), each = 12))
  y <- station_data$train
  w <- ifelse(is.na(y), 0, 1)
  station_data$indicator <- w
  trend_ss_basis_s <- trend_ss_basis_ext * w
  
  return(list(rain = y, basis = trend_ss_basis_s, trend_ss_mat = t(trend_ss_basis_s) %*% trend_ss_basis_s, indicator = w, test = test))
}


# ==============================================================================
# 5. Spatial Kernel Smoothing Functions
# ==============================================================================
trend_ss_estimate_smoothing <- function(station_cur_ind, df, h, knn) {
  dist_station_cur <- numeric()
  for (i in 1:nrow(spat_full_data)) {
    dist_station_cur[i] <- spatial_dist(station_cur_ind, i)
  }
  
  nearest_station_cur <- sort(dist_station_cur, decreasing = FALSE, index.return = TRUE)$ix[-1][1:9]
  sorted_distances <- sort(dist_station_cur, decreasing = FALSE)
  sorted_distances <- sorted_distances / max(sorted_distances)
  
  t0 <- trend_ss_matrix(station_cur_ind, df)
  a0 <- (1 / h) * t0$trend_ss_mat
  t0_rain <- t0$rain
  t0_rain[is.na(t0_rain)] <- 0
  b0 <- (1 / h) * t(t0$basis) %*% as.matrix(t0_rain, 1)
  
  a_list <- list(a0)
  b_list <- list(b0)
  indices <- 1:knn
  
  for (i in indices) {
    var_name_1 <- paste0("t", i)
    assign(var_name_1, trend_ss_matrix(nearest_station_cur[i], df))
    var_name_2 <- paste0("a", i)
    var_name_3 <- paste0("b", i)
    t_value <- get(var_name_1)
    
    assign(var_name_2, (1 / h) * 1 / (sqrt(2 * pi)) * exp(-((sorted_distances[i + 1] / h)^2) / 2) * t_value$trend_ss_mat)
    t_rain <- t_value$rain
    t_rain[is.na(t_rain)] <- 0
    assign(var_name_3, (1 / h) * 1 / (sqrt(2 * pi)) * exp(-((sorted_distances[i + 1] / h)^2) / 2) * t(t_value$basis) %*% as.matrix(t_rain, 1))
    
    a_list[[i + 1]] <- get(var_name_2)
    b_list[[i + 1]] <- get(var_name_3)
  }
  
  A <- Reduce("+", a_list)
  b <- Reduce("+", b_list)
  coeff <- solve(t(A) %*% A) %*% t(A) %*% b
  
  beta_trend <- coeff[1:df]
  beta_ss <- coeff[(df + 1):length(coeff)]
  
  trend <- as.numeric(as.matrix(trend_basis %*% as.matrix(beta_trend)))
  ss <- as.numeric(as.matrix(seasonality_basis_1 %*% as.matrix(beta_ss)))
  ss_df <- matrix(ss, ncol = 12, byrow = TRUE)
  seasonality_period <- as.data.frame(t(ss_df[!duplicated(ss_df), ]))
  
  random <- t0$rain - trend - ss
  pred <- trend + ss
  
  return(list(trend = trend, seasonality_period = seasonality_period, rain = t0$rain, predicted = pred, error = random))
}

trend_ss_estimate_smoothing_train_test <- function(station_cur_ind, df, h, knn, ratio = 0.9) {
  dist_station_cur <- numeric()
  for (i in 1:nrow(spat_full_data)) {
    dist_station_cur[i] <- spatial_dist(station_cur_ind, i)
  }
  
  nearest_station_cur <- sort(dist_station_cur, decreasing = FALSE, index.return = TRUE)$ix[-1][1:9]
  sorted_distances <- sort(dist_station_cur, decreasing = FALSE)
  sorted_distances <- sorted_distances / max(sorted_distances)
  
  t0 <- trend_ss_matrix_train_test(station_cur_ind, df, ratio)
  a0 <- (1 / h) * t0$trend_ss_mat
  t0_rain <- t0$rain
  t0_rain[is.na(t0_rain)] <- 0
  b0 <- (1 / h) * t(t0$basis) %*% as.matrix(t0_rain, 1)
  
  a_list <- list(a0)
  b_list <- list(b0)
  indices <- 1:knn
  
  for (i in indices) {
    var_name_1 <- paste0("t", i)
    assign(var_name_1, trend_ss_matrix(nearest_station_cur[i], df))
    var_name_2 <- paste0("a", i)
    var_name_3 <- paste0("b", i)
    t_value <- get(var_name_1)
    
    assign(var_name_2, (1 / h) * 1 / (sqrt(2 * pi)) * exp(-((sorted_distances[i + 1] / h)^2) / 2) * t_value$trend_ss_mat)
    t_rain <- t_value$rain
    t_rain[is.na(t_rain)] <- 0
    assign(var_name_3, (1 / h) * 1 / (sqrt(2 * pi)) * exp(-((sorted_distances[i + 1] / h)^2) / 2) * t(t_value$basis) %*% as.matrix(t_rain, 1))
    
    a_list[[i + 1]] <- get(var_name_2)
    b_list[[i + 1]] <- get(var_name_3)
  }
  
  A <- Reduce("+", a_list)
  b <- Reduce("+", b_list)
  coeff <- solve(t(A) %*% A) %*% t(A) %*% b
  
  beta_trend <- coeff[1:df]
  beta_ss <- coeff[(df + 1):length(coeff)]
  
  trend <- as.numeric(as.matrix(trend_basis %*% as.matrix(beta_trend)))
  ss <- as.numeric(as.matrix(seasonality_basis_1 %*% as.matrix(beta_ss)))
  ss_df <- matrix(ss, ncol = 12, byrow = TRUE)
  seasonality_period <- as.data.frame(t(ss_df[!duplicated(ss_df), ]))
  
  random <- t0$rain - trend - ss
  pred <- trend + ss
  
  return(list(trend = trend, seasonality_period = seasonality_period, rain = t0$rain, predicted = pred, error = random, test = t0$test))
}

trend_ss_estimate_smoothing_ext <- function(station_cur_ind, df, h, knn) {
  dist_station_cur <- numeric()
  for (i in 1:nrow(spat_full_data)) {
    dist_station_cur[i] <- spatial_dist(station_cur_ind, i)
  }
  
  nearest_station_cur <- sort(dist_station_cur, decreasing = FALSE, index.return = TRUE)$ix[-1][1:9]
  sorted_distances <- sort(dist_station_cur, decreasing = FALSE)
  sorted_distances <- sorted_distances / max(sorted_distances)
  
  t0 <- trend_ss_matrix_ext(station_cur_ind, df)
  a0 <- (1 / h) * t0$trend_ss_mat
  t0_rain <- t0$rain
  t0_rain[is.na(t0_rain)] <- 0
  b0 <- (1 / h) * t(t0$basis) %*% as.matrix(t0_rain, 1)
  
  a_list <- list(a0)
  b_list <- list(b0)
  indices <- 1:knn
  
  for (i in indices) {
    var_name_1 <- paste0("t", i)
    assign(var_name_1, trend_ss_matrix_ext(nearest_station_cur[i], df))
    var_name_2 <- paste0("a", i)
    var_name_3 <- paste0("b", i)
    t_value <- get(var_name_1)
    
    assign(var_name_2, (1 / h) * 1 / (sqrt(2 * pi)) * exp(-((sorted_distances[i + 1] / h)^2) / 2) * t_value$trend_ss_mat)
    t_rain <- t_value$rain
    t_rain[is.na(t_rain)] <- 0
    assign(var_name_3, (1 / h) * 1 / (sqrt(2 * pi)) * exp(-((sorted_distances[i + 1] / h)^2) / 2) * t(t_value$basis) %*% as.matrix(t_rain, 1))
    
    a_list[[i + 1]] <- get(var_name_2)
    b_list[[i + 1]] <- get(var_name_3)
  }
  
  A <- Reduce("+", a_list)
  b <- Reduce("+", b_list)
  coeff <- solve(t(A) %*% A) %*% t(A) %*% b
  
  beta_trend <- coeff[1:df]
  beta_ss <- coeff[(df + 1):length(coeff)]
  
  trend <- as.numeric(as.matrix(trend_basis_ext %*% as.matrix(beta_trend)))
  ss <- as.numeric(as.matrix(seasonality_basis_1 %*% as.matrix(beta_ss)))
  ss_df <- matrix(ss, ncol = 12, byrow = TRUE)
  seasonality_period <- as.data.frame(t(ss_df[!duplicated(ss_df), ]))
  
  random <- t0$rain - trend - ss
  pred <- trend + ss
  
  return(list(trend = trend, seasonality_period = seasonality_period, rain = t0$rain, predicted = pred, error = random))
}

trend_ss_estimate_smoothing_train_test_ext <- function(station_cur_ind, df, h, knn, ratio = 0.9) {
  dist_station_cur <- numeric()
  for (i in 1:nrow(spat_full_data)) {
    dist_station_cur[i] <- spatial_dist(station_cur_ind, i)
  }
  
  nearest_station_cur <- sort(dist_station_cur, decreasing = FALSE, index.return = TRUE)$ix[-1][1:9]
  sorted_distances <- sort(dist_station_cur, decreasing = FALSE)
  sorted_distances <- sorted_distances / max(sorted_distances)
  
  t0 <- trend_ss_matrix_train_test_ext(station_cur_ind, df, ratio)
  a0 <- (1 / h) * t0$trend_ss_mat
  t0_rain <- t0$rain
  t0_rain[is.na(t0_rain)] <- 0
  b0 <- (1 / h) * t(t0$basis) %*% as.matrix(t0_rain, 1)
  
  a_list <- list(a0)
  b_list <- list(b0)
  indices <- 1:knn
  
  for (i in indices) {
    var_name_1 <- paste0("t", i)
    assign(var_name_1, trend_ss_matrix_ext(nearest_station_cur[i], df))
    var_name_2 <- paste0("a", i)
    var_name_3 <- paste0("b", i)
    t_value <- get(var_name_1)
    
    assign(var_name_2, (1 / h) * 1 / (sqrt(2 * pi)) * exp(-((sorted_distances[i + 1] / h)^2) / 2) * t_value$trend_ss_mat)
    t_rain <- t_value$rain
    t_rain[is.na(t_rain)] <- 0
    assign(var_name_3, (1 / h) * 1 / (sqrt(2 * pi)) * exp(-((sorted_distances[i + 1] / h)^2) / 2) * t(t_value$basis) %*% as.matrix(t_rain, 1))
    
    a_list[[i + 1]] <- get(var_name_2)
    b_list[[i + 1]] <- get(var_name_3)
  }
  
  A <- Reduce("+", a_list)
  b <- Reduce("+", b_list)
  coeff <- solve(t(A) %*% A) %*% t(A) %*% b
  
  beta_trend <- coeff[1:df]
  beta_ss <- coeff[(df + 1):length(coeff)]
  
  trend <- as.numeric(as.matrix(trend_basis_ext %*% as.matrix(beta_trend)))
  ss <- as.numeric(as.matrix(seasonality_basis_1 %*% as.matrix(beta_ss)))
  ss_df <- matrix(ss, ncol = 12, byrow = TRUE)
  seasonality_period <- as.data.frame(t(ss_df[!duplicated(ss_df), ]))
  
  random <- t0$rain - trend - ss
  pred <- trend + ss
  
  return(list(trend = trend, seasonality_period = seasonality_period, rain = t0$rain, predicted = pred, error = random, test = t0$test))
}


# ==============================================================================
# 6. Regional Difference Penalization Functions
# ==============================================================================
EE1E2_s_diffpenalty <- function(s, df, lambda1, lambda2) {
  t_s <- trend_ss_matrix(s, df)
  E2_s <- t_s$trend_ss_mat
  L <- diag(c(rep(lambda1, df), rep(lambda2, (nrow(E2_s) - df))))
  E_s <- E2_s + L
  rain_s <- t_s$rain
  rain_s[is.na(rain_s)] <- 0
  E1_s <- t(t_s$basis) %*% as.matrix(rain_s, 1)
  
  return(list(E1 = E1_s, Es = E_s, E2 = E2_s, t0 = t_s))
}

FF1_diffpenalty <- function(df, lambda1, lambda2, type) {
  F1 <- 0
  F2 <- 0
  check <- ifelse(spat_full_data$region == type, 1, 0)
  check_ind <- which(check == 1)
  
  for (i in 1:length(check_ind)) {
    info <- EE1E2_s_diffpenalty(check_ind[i], df, lambda1, lambda2)
    F1 <- F1 + solve(info$Es) %*% info$E2
    F2 <- F2 + solve(info$Es) %*% info$E1
  }
  
  return(solve(F1) %*% F2)
}

trend_ss_estimate_ind_diffpenalty <- function(s, df, lambda1, lambda2, FF1_reg) {
  type <- spat_full_data[s, 4]
  info <- EE1E2_s_diffpenalty(s, df, lambda1, lambda2)
  L <- diag(c(rep(lambda1, df), rep(lambda2, (nrow(info$Es) - df))))
  coeff <- solve(info$Es) %*% (info$E1 + L %*% FF1_reg)
  
  beta_trend <- coeff[1:df]
  beta_ss <- coeff[(df + 1):length(coeff)]
  
  trend <- as.numeric(as.matrix(trend_basis %*% as.matrix(beta_trend)))
  ss <- as.numeric(as.matrix(seasonality_basis_1 %*% as.matrix(beta_ss)))
  ss_df <- matrix(ss, ncol = 12, byrow = TRUE)
  seasonality_period <- as.data.frame(t(ss_df[!duplicated(ss_df), ]))
  
  t0 <- info$t0
  random <- t0$rain - trend - ss
  pred <- trend + ss
  
  return(list(trend = trend, seasonality_period = seasonality_period, predicted = pred, error = random))
}

EE1E2_s_diffpenalty_train_test <- function(s, df, lambda1, lambda2, ratio = 0.9) {
  t_s <- trend_ss_matrix_train_test(s, df, ratio)
  E2_s <- t_s$trend_ss_mat
  L <- diag(c(rep(lambda1, df), rep(lambda2, (nrow(E2_s) - df))))
  E_s <- E2_s + L
  rain_s <- t_s$rain
  rain_s[is.na(rain_s)] <- 0
  E1_s <- t(t_s$basis) %*% as.matrix(rain_s, 1)
  
  return(list(E1 = E1_s, Es = E_s, E2 = E2_s, test = t_s$test))
}

trend_ss_estimate_ind_diffpenalty_train_test <- function(s, df, lambda1, lambda2, FF1_reg, ratio = 0.9) {
  type <- spat_full_data[s, 4]
  info <- EE1E2_s_diffpenalty_train_test(s, df, lambda1, lambda2, ratio)
  L <- diag(c(rep(lambda1, df), rep(lambda2, (nrow(info$Es) - df))))
  coeff <- solve(info$Es) %*% (info$E1 + L %*% FF1_reg)
  
  beta_trend <- coeff[1:df]
  beta_ss <- coeff[(df + 1):length(coeff)]
  
  trend <- as.numeric(as.matrix(trend_basis %*% as.matrix(beta_trend)))
  ss <- as.numeric(as.matrix(seasonality_basis_1 %*% as.matrix(beta_ss)))
  ss_df <- matrix(ss, ncol = 12, byrow = TRUE)
  seasonality_period <- as.data.frame(t(ss_df[!duplicated(ss_df), ]))
  
  t0 <- info$t0
  random <- t0$rain - trend - ss
  pred <- trend + ss
  
  return(list(trend = trend, seasonality_period = seasonality_period, predicted = pred, error = random, test = info$test))
}

EE1E2_s_diffpenalty_ext <- function(s, df, lambda1, lambda2) {
  t_s <- trend_ss_matrix_ext(s, df)
  E2_s <- t_s$trend_ss_mat
  L <- diag(c(rep(lambda1, df), rep(lambda2, (nrow(E2_s) - df))))
  E_s <- E2_s + L
  rain_s <- t_s$rain
  rain_s[is.na(rain_s)] <- 0
  E1_s <- t(t_s$basis) %*% as.matrix(rain_s, 1)
  
  return(list(E1 = E1_s, Es = E_s, E2 = E2_s, t0 = t_s))
}

FF1_diffpenalty_ext <- function(df, lambda1, lambda2, type) {
  F1 <- 0
  F2 <- 0
  check <- ifelse(spat_full_data$region == type, 1, 0)
  check_ind <- which(check == 1)
  
  for (i in 1:length(check_ind)) {
    info <- EE1E2_s_diffpenalty_ext(check_ind[i], df, lambda1, lambda2)
    F1 <- F1 + solve(info$Es) %*% info$E2
    F2 <- F2 + solve(info$Es) %*% info$E1
  }
  
  return(solve(F1) %*% F2)
}

trend_ss_estimate_ind_diffpenalty_ext <- function(s, df, lambda1, lambda2, FF1_reg) {
  type <- spat_full_data[s, 4]
  info <- EE1E2_s_diffpenalty_ext(s, df, lambda1, lambda2)
  L <- diag(c(rep(lambda1, df), rep(lambda2, (nrow(info$Es) - df))))
  coeff <- solve(info$Es) %*% (info$E1 + L %*% FF1_reg)
  
  beta_trend <- coeff[1:df]
  beta_ss <- coeff[(df + 1):length(coeff)]
  
  trend <- as.numeric(as.matrix(trend_basis_ext %*% as.matrix(beta_trend)))
  ss <- as.numeric(as.matrix(seasonality_basis_1 %*% as.matrix(beta_ss)))
  ss_df <- matrix(ss, ncol = 12, byrow = TRUE)
  seasonality_period <- as.data.frame(t(ss_df[!duplicated(ss_df), ]))
  
  t0 <- info$t0
  random <- t0$rain - trend - ss
  pred <- trend + ss
  
  return(list(trend = trend, seasonality_period = seasonality_period, predicted = pred, error = random))
}

EE1E2_s_diffpenalty_train_test_ext <- function(s, df, lambda1, lambda2, ratio = 0.9) {
  t_s <- trend_ss_matrix_train_test_ext(s, df, ratio)
  E2_s <- t_s$trend_ss_mat
  L <- diag(c(rep(lambda1, df), rep(lambda2, (nrow(E2_s) - df))))
  E_s <- E2_s + L
  rain_s <- t_s$rain
  rain_s[is.na(rain_s)] <- 0
  E1_s <- t(t_s$basis) %*% as.matrix(rain_s, 1)
  
  return(list(E1 = E1_s, Es = E_s, E2 = E2_s, test = t_s$test))
}

trend_ss_estimate_ind_diffpenalty_train_test_ext <- function(s, df, lambda1, lambda2, FF1_reg, ratio = 0.9) {
  type <- spat_full_data[s, 4]
  info <- EE1E2_s_diffpenalty_train_test_ext(s, df, lambda1, lambda2, ratio)
  L <- diag(c(rep(lambda1, df), rep(lambda2, (nrow(info$Es) - df))))
  coeff <- solve(info$Es) %*% (info$E1 + L %*% FF1_reg)
  
  beta_trend <- coeff[1:df]
  beta_ss <- coeff[(df + 1):length(coeff)]
  
  trend <- as.numeric(as.matrix(trend_basis_ext %*% as.matrix(beta_trend)))
  ss <- as.numeric(as.matrix(seasonality_basis_1 %*% as.matrix(beta_ss)))
  ss_df <- matrix(ss, ncol = 12, byrow = TRUE)
  seasonality_period <- as.data.frame(t(ss_df[!duplicated(ss_df), ]))
  
  t0 <- info$t0
  random <- t0$rain - trend - ss
  pred <- trend + ss
  
  return(list(trend = trend, seasonality_period = seasonality_period, predicted = pred, error = random, test = info$test))
}