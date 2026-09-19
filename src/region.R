# ==============================================================================
# Script Name: region.R
# Description: Defines spatial distance metric (Haversine distance in km) between 
#              weather stations and categorizes stations into geographic regions
#              (North, West, South West Bengal).
# Input Files:  data/processed/full_data_wb.csv
# Output Files: artifacts/sufficient-stats/spat_full_data_regions.csv
# ==============================================================================

# Ensure environment and libraries are loaded
if (!exists("required_packages")) {
  source("src/initialize.R")
}

# 1. Load or Construct spat_full_data Workspace Object ------------------------
if (!exists("spat_full_data")) {
  if (exists("full_data")) {
    spat_full_data <- unique(full_data %>% select("LAT", "LON", "STATION"))
  } else if (file.exists("data/processed/full_data_wb.csv")) {
    full_data <- read.csv("data/processed/full_data_wb.csv")
    spat_full_data <- unique(full_data %>% select("LAT", "LON", "STATION"))
  } else {
    stop("Error: full_data dataset not found in memory or data/processed/.")
  }
}

# 2. Haversine Spatial Distance Metric ----------------------------------------
#' Calculate Great-Circle Distance Between Two Weather Stations
#' 
#' Computes the Haversine distance in kilometers between station indices `i` and `j`
#' using their geographic latitude and longitude coordinates.
#' 
#' @param i Index of the primary station
#' @param j Index of the target station
#' @return Numeric scalar distance in kilometers (km)
spatial_dist = function(i, j) {
  s_i = as.numeric(spat_full_data[i, 1:2])
  s_j = as.numeric(spat_full_data[j, 1:2])
  phi1 = s_i[1] * pi / 180
  phi2 = s_j[1] * pi / 180
  lambda1 = s_i[2] * pi / 180
  lambda2 = s_j[2] * pi / 180
  d_ij = sin((phi2 - phi1) / 2)^2 + cos(phi1) * cos(phi2) * sin((lambda2 - lambda1) / 2)^2
  d = 2 * 6371 * asin(sqrt(d_ij))
  return(d)
}

# 3. Format Spatial Column Headers & Types -------------------------------------
colnames(spat_full_data) = c("latitude", "longitude", "station")
spat_full_data$latitude = as.numeric(spat_full_data$latitude)
spat_full_data$longitude = as.numeric(spat_full_data$longitude)

# 4. Regional Categorization Logic ---------------------------------------------
# Assign stations to North ("N"), West ("W"), or South ("S") sub-regions
message("Categorizing weather stations into regional zones (N/W/S)...")
spat_full_data$region <- ifelse(spat_full_data$latitude > 25, "N",
                                ifelse(spat_full_data$longitude < 87.5, "W", "S"))

# 5. Persist Regional Mappings ------------------------------------------------
if (!dir.exists("artifacts/sufficient-stats")) {
  dir.create("artifacts/sufficient-stats", recursive = TRUE)
}

write.csv(spat_full_data, "artifacts/sufficient-stats/spat_full_data_regions.csv", row.names = FALSE)

message("Regional categorization complete. Saved mapping to artifacts/sufficient-stats/.")