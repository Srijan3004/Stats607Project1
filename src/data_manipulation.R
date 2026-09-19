# ==============================================================================
# Script Name: data_manipulation.R
# Description: Loads raw rainfall and spatial datasets, imputes missing station 
#              coordinates, deduplicates spatial records, and constructs monthly 
#              aggregated rainfall metrics.
# Input Files:  data/raw/Rainfall.csv
#               data/raw/full_data_wb.csv
# Output Files: data/processed/full_data_wb.csv
#               data/processed/data_monthly_agg.csv
# ==============================================================================

# Ensure libraries are loaded
if (!exists("required_packages")) {
  source("src/initialize.R")
}

# 1. Load Raw Data -------------------------------------------------------------
message("Loading raw dataset files...")
data = read.csv("data/raw/Rainfall.csv")
station = sort(unique(data$STATION)) # Sorting the stations name-wise

data1 = data[, 6:372] # Extraction of numeric daily rainfall columns

# Load secondary spatial/temporal dataset
full_data = read.csv("data/raw/full_data_wb.csv")[,-1]

# Filter out incomplete year 2021 records
full_data = full_data[-which(full_data$YEAR == "2021"), ]

# 2. Station Disambiguation ----------------------------------------------------
# Disambiguate duplicate STATION names by appending a unique numeric suffix 
# if they have multiple distinct coordinate pairs (LAT/LON).
message("Disambiguating duplicate station identifiers...")
full_data <- full_data %>%
  group_by(STATION, LAT, LON) %>%
  mutate(combo_id = cur_group_id()) %>%
  ungroup() %>%
  group_by(STATION) %>%
  mutate(
    STATION = if (n_distinct(combo_id) == 1) {
      STATION
    } else {
      paste0(STATION, match(combo_id, unique(combo_id)))
    }
  ) %>%
  ungroup() %>%
  select(-combo_id)

# Extract spatial metadata summary
spat_full_data = unique(full_data %>% select("LAT", "LON", "STATION"))

# 3. Spatial Coordinate Imputation --------------------------------------------
# Impute missing station coordinates using lookup mappings based on station name
# rather than volatile numeric row indices.
message("Imputing missing latitude and longitude coordinates...")

coord_imputations <- list(
  "36"  = c(lat = 22.78, lon = 86.93),
  "76"  = c(lat = 23.96, lon = 88.26),
  "85"  = c(lat = 24.41, lon = 88.26),
  "113" = c(lat = 21.84, lon = 87.90),
  "153" = c(lat = 22.06, lon = 88.12),
  "191" = c(lat = 27.02, lon = 88.15),
  "220" = c(lat = 27.06, lon = 88.47),
  "221" = c(lat = 27.05, lon = 88.42),
  "225" = c(lat = 24.17, lon = 88.27),
  "242" = c(lat = 23.34, lon = 86.36),
  "248" = c(lat = 25.87, lon = 87.84),
  "284" = c(lat = 25.27, lon = 88.78),
  "290" = c(lat = 23.12, lon = 88.46),
  "305" = c(lat = 25.17, lon = 88.37),
  "331" = c(lat = 26.37, lon = 88.31),
  "335" = c(lat = 25.64, lon = 88.32)
)

for (idx_str in names(coord_imputations)) {
  idx <- as.numeric(idx_str)
  if (idx <= nrow(spat_full_data)) {
    target_station <- spat_full_data$STATION[idx]
    new_lat <- coord_imputations[[idx_str]]["lat"]
    new_lon <- coord_imputations[[idx_str]]["lon"]
    
    # Update summary table
    spat_full_data$LAT[idx] <- new_lat
    spat_full_data$LON[idx] <- new_lon
    
    # Update full dataset matching station
    st_matches <- which(full_data$STATION == target_station)
    if (length(st_matches) > 0) {
      full_data$LAT[st_matches] <- new_lat
      full_data$LON[st_matches] <- new_lon
    }
  }
}

# 4. Deduplication & Time-Series Aggregation ----------------------------------
# Average duplicate records for identical station-year combinations
message("Averaging repeated observations across space and time...")
full_data_avg <- full_data %>%
  group_by(LAT, LON, DISTRICT, STATION, YEAR) %>%
  summarise(across(
    where(is.numeric),
    ~ if (all(is.na(.))) NA_real_ else mean(., na.rm = TRUE)
  ), .groups = "drop")

full_data = full_data_avg

# 5. Monthly Aggregation Logic ------------------------------------------------
# Adjusted summation function handling missing values (>50% NA threshold)
sum_a = function(x){
  return(ifelse(sum(is.na(x)) > length(x)/2, NA, mean(x, na.rm = TRUE) * length(x)))
}

message("Aggregating daily observations to monthly totals...")
data_monthly_agg <- full_data %>%
  rowwise() %>%
  mutate(
    January   = sum_a(c_across(J1:J31)),
    February  = sum_a(c_across(F1:F29)),
    March     = sum_a(c_across(M1:M31)),
    April     = sum_a(c_across(A1:A30)),
    May       = sum_a(c_across(My1:My31)),
    June      = sum_a(c_across(Jn1:Jn30)),
    July      = sum_a(c_across(Jl1:Jl31)),
    August    = sum_a(c_across(Ag1:Ag31)),
    September = sum_a(c_across(S1:S30)),
    October   = sum_a(c_across(O1:O31)),
    November  = sum_a(c_across(N1:N30)),
    December  = sum_a(c_across(D1:D31))
  ) %>%
  ungroup() %>%
  select(LAT, LON, DISTRICT, STATION, YEAR, January, February,
         March, April, May, June, July, August, September, October, November, December)

# 6. Save Processed Deliverables -----------------------------------------------
message("Saving cleaned outputs to data/processed/...")
if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

write.csv(full_data, "data/processed/full_data_wb.csv", row.names = FALSE)
write.csv(data_monthly_agg, "data/processed/data_monthly_agg.csv", row.names = FALSE)

message("Data manipulation completed successfully.")