# ==============================================================================
# Script Name: visualization.R
# Description: Exploratory data analysis and missingness visualization for West 
#              Bengal rainfall time-series (1920–2020) and spatial coverage maps.
# Input Files:  data/processed/full_data_wb.csv
#               data/processed/data_monthly_agg.csv
# Output Files: results/figures/missingness_fraction_1920_2020.png
#               results/figures/missingness_day_categories.png
#               results/figures/spatial_station_data_count.png
#               results/figures/spatial_avg_rainfall_decades.png
# ==============================================================================

# Ensure environment and libraries are loaded
if (!exists("required_packages")) {
  source("src/initialize.R")
}

# Ensure results/figures directory exists
if (!dir.exists("results/figures")) dir.create("results/figures", recursive = TRUE)

# Load Processed Data Dependencies if not already present in workspace
if (!exists("data_monthly_agg")) {
  message("Loading processed dataset: data/processed/data_monthly_agg.csv")
  data_monthly_agg <- read.csv("data/processed/data_monthly_agg.csv")
}
if (!exists("full_data")) {
  message("Loading processed dataset: data/processed/full_data_wb.csv")
  full_data <- read.csv("data/processed/full_data_wb.csv")
}

# 1. Monthly Missingness Time-Series (1920–2020) ------------------------------
message("Generating monthly missingness trend plots...")

all_years <- 1920:2020
all_stations <- unique(data_monthly_agg$STATION)
all_months <- month.name  # "January", "February", ..., "December"

# Full grid of all possible station-year-month combinations
full_grid <- expand.grid(
  STATION = all_stations,
  YEAR = all_years,
  month = all_months,
  stringsAsFactors = FALSE
)

# Reshape data to long format
data_long <- data_monthly_agg %>%
  pivot_longer(
    cols = January:December,
    names_to = "month",
    values_to = "rain"
  ) %>%
  select(STATION, LAT, LON, YEAR, month, rain)

# Calculate missingness proportion across stations
missing_info <- full_grid %>%
  left_join(data_long, by = c("STATION", "YEAR", "month")) %>%
  mutate(
    missing = is.na(rain),
    month = factor(month, levels = month.name, ordered = TRUE) 
  ) %>%
  group_by(YEAR, month) %>%
  summarise(
    missing_count = sum(missing) / length(all_stations),
    .groups = "drop"
  ) %>%
  arrange(YEAR, month)

# Plot missingness proportion facet
p1 <- ggplot(missing_info, aes(x = YEAR, y = missing_count)) +
  geom_line(color = "darkmagenta", size = 1) +
  facet_wrap(~month, nrow = 3, ncol = 4) +
  labs(
    title = "Stations Missing Monthly Rainfall Data (1920–2020)",
    x = "Year",
    y = "Fraction of Stations Missing"
  ) +
  theme_minimal(base_size = 18) +   # sets a large base font size
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    panel.grid.minor = element_blank()
  )

ggsave("results/exploratory/missingness_fraction_1920_2020.png", plot = p1, width = 16, height = 10, dpi = 300)

# 2. Daily Missing-Day Binned Categorization -----------------------------------
message("Categorizing missing daily observations per month...")

# Pivot daily data into long format
data_long_daily <- full_data %>%
  pivot_longer(
    cols = -(1:5),          # keep LAT, LON, DISTRICT, STATION, YEAR
    names_to = "daycode",
    values_to = "rain"
  ) %>%
  mutate(
    missing = is.na(rain)
  )

# Extract month from daycode prefix
data_long_daily <- data_long_daily %>%
  mutate(
    month_abbr = str_extract(daycode, "^[A-Za-z]+"),  # take the leading letters
    month = case_when(
      month_abbr %in% c("J")   ~ "January",
      month_abbr %in% c("F")   ~ "February",
      month_abbr %in% c("M")   ~ "March",
      month_abbr %in% c("A")   ~ "April",
      month_abbr %in% c("My")  ~ "May",
      month_abbr %in% c("Jn")  ~ "June",
      month_abbr %in% c("Jl")  ~ "July",
      month_abbr %in% c("Ag")  ~ "August",
      month_abbr %in% c("S")   ~ "September",
      month_abbr %in% c("O")   ~ "October",
      month_abbr %in% c("N")   ~ "November",
      month_abbr %in% c("D")   ~ "December",
      TRUE ~ NA_character_
    ),
    month = factor(month, levels = month.name, ordered = TRUE)
  )

# Count missing days per station-year-month
station_month_missing <- data_long_daily %>%
  group_by(STATION, YEAR, month) %>%
  summarise(
    missing_days = sum(missing, na.rm = TRUE),
    .groups = "drop"
  )

# Categorize missingness into operational severity bins
station_month_missing <- station_month_missing %>%
  mutate(
    category = case_when(
      missing_days <= 5  ~ "0–5 days",
      missing_days <= 15 ~ "6–15 days",
      TRUE               ~ ">15 days"
    )
  )

# Compute proportion of stations in each category
summary_counts <- station_month_missing %>%
  group_by(YEAR, month, category) %>%
  summarise(stations = n() / length(all_stations), .groups = "drop")

# Facet plot by category
p2 <- ggplot(summary_counts, aes(x = YEAR, y = stations, color = category)) +
  geom_line(size = 1.2) +
  facet_wrap(~month, nrow = 3, ncol = 4) +
  scale_color_manual(values = c("0–5 days" = "darkgreen",
                                "6–15 days" = "orange",
                                ">15 days" = "red")) +
  labs(
    title = "Stations by Missing-Day Category per Month (1920–2020)",
    x = "Year",
    y = "Number of Stations",
    color = "Missing Days"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    strip.text = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 22),
    legend.position = "bottom",               # move legend to bottom
    legend.box = "horizontal"                # arrange items in a row
  )

ggsave("results/exploratory/missingness_day_categories.png", plot = p2, width = 16, height = 10, dpi = 300)

# 3. Spatial Distribution & Non-Missing Data Count Map -----------------------
message("Generating spatial map for station data completeness...")

# Count months with data per station
months_per_station <- data_long %>%
  group_by(STATION, LAT, LON) %>%
  summarise(months_with_data = sum(!is.na(rain)) / ((2020 - 1901 + 1) * 12), .groups = "drop")

# Retrieve spatial vector shape for West Bengal
india <- rnaturalearth::ne_states(country = "India", returnclass = "sf")
west_bengal <- india %>% filter(name == "West Bengal")

p3 <- ggplot() +
  geom_sf(data = west_bengal, fill = "grey", color = "black") +
  geom_point(
    data = months_per_station,
    aes(x = as.numeric(LON), y = as.numeric(LAT),
        color = months_with_data),
    size = 3, alpha = 0.8
  ) +
  scale_color_viridis_c(option = "plasma", name = "Months with Data") +
  guides(
    color = guide_colorbar(
      barheight = unit(6, "cm"),
      barwidth  = unit(0.6, "cm"),
      title.position = "top"
    )
  ) +
  labs(
    title = "Weather Stations (Non-Missing Data Count)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text  = element_text(size = 16)
  )

ggsave("results/exploratory/spatial_station_data_count.png", plot = p3, width = 8, height = 10, dpi = 300)

# 4. Decadal Average Rainfall Facet Map ---------------------------------------
message("Generating decadal spatial comparison maps...")

# Compute average rainfall per station for target landmark years
avg_rain_per_year <- data_long %>%
  filter(YEAR %in% c(1901, 1941, 1981, 2020)) %>%
  group_by(STATION, LAT, LON, YEAR) %>%
  summarise(avg_rain = mean(rain, na.rm = TRUE), .groups = "drop")

p4 <- ggplot() +
  geom_sf(data = west_bengal, fill = "grey", color = "black") +
  geom_point(
    data = avg_rain_per_year,
    aes(x = as.numeric(LON), y = as.numeric(LAT), color = avg_rain),
    size = 3, alpha = 0.8
  ) +
  scale_color_viridis_c(option = "plasma", name = "Avg Rainfall (mm)") +
  facet_wrap(~YEAR, nrow = 1) +
  labs(
    title = "Average Rainfall at Weather Stations",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold")
  )

ggsave("results/exploratory/spatial_avg_rainfall_decades.png", plot = p4, width = 16, height = 8, dpi = 300)

# 5. Station Summary Audit -----------------------------------------------------
station_summary_audit <- data_monthly_agg %>%
  group_by(LAT, LON, DISTRICT, STATION) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

write.csv(station_summary_audit, "results/tables/station_summary_audit.csv", row.names = FALSE)

message("Visualization script execution complete. All figures saved to results/exploratory/.")