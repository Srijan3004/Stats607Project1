library(haven)
library(dplyr)
library(forecast)
library(ggplot2)
library(stringr)
library(tidyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(zoo)
library(reshape2)
library(splines)
library(tidyverse)
library(stats)
library(gridExtra)
library(forecast)
library(splines2)


# Reading the actual data
data = read.csv("/Users/srijanch/Downloads/Rainfall.csv")
station = sort(unique(data$STATION)) # sorting the stations name wise

data1 = data[,6:372] #just taking the numeric rainfall part!

# taking lattitude, lomgitude, station name, district, rainfall
#full_data = cbind.data.frame(LAT = data$LAT,LON = data$LON,DISTRICT = data$DISTRICT,STATION = data$STATION,data1) 
full_data = read.csv("/Users/srijanch/Downloads/full_data_wb.csv")[,-1]

full_data = full_data[-which(full_data$YEAR == "2021"),]

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


# storing spatial data!
spat_full_data = unique(full_data %>% 
                          select("LAT","LON","STATION"))
spat_full_data$LAT[36] = full_data$LAT[which(full_data$STATION == spat_full_data$STATION[36])] = 22.78
spat_full_data$LON[36] = full_data$LON[which(full_data$STATION == spat_full_data$STATION[36])] = 86.93
spat_full_data$LAT[76] = full_data$LAT[which(full_data$STATION == spat_full_data$STATION[76])] = 23.96
spat_full_data$LON[76] = full_data$LON[which(full_data$STATION == spat_full_data$STATION[76])] = 88.26
spat_full_data$LAT[85] = full_data$LAT[which(full_data$STATION == spat_full_data$STATION[85])] = 24.41
spat_full_data$LON[85] = full_data$LON[which(full_data$STATION == spat_full_data$STATION[85])] = 88.26
spat_full_data$LAT[113] = full_data$LAT[which(full_data$STATION == spat_full_data$STATION[113])] = 21.84
spat_full_data$LON[113] = full_data$LON[which(full_data$STATION == spat_full_data$STATION[113])] = 87.90
spat_full_data$LAT[153] = full_data$LAT[which(full_data$STATION == spat_full_data$STATION[153])] = 22.06
spat_full_data$LON[153] = full_data$LON[which(full_data$STATION == spat_full_data$STATION[153])] = 88.12
spat_full_data$LAT[191] = full_data$LAT[which(full_data$STATION == spat_full_data$STATION[191])] = 27.02
spat_full_data$LON[191] = full_data$LON[which(full_data$STATION == spat_full_data$STATION[191])] = 88.15
spat_full_data$LAT[220] = full_data$LAT[which(full_data$STATION == spat_full_data$STATION[220])] = 27.06
spat_full_data$LON[220] = full_data$LON[which(full_data$STATION == spat_full_data$STATION[220])] = 88.47
spat_full_data$LAT[221] = full_data$LAT[which(full_data$STATION == spat_full_data$STATION[221])] = 27.05
spat_full_data$LON[221] = full_data$LON[which(full_data$STATION == spat_full_data$STATION[221])] = 88.42
spat_full_data$LAT[225] = full_data$LAT[which(full_data$STATION == spat_full_data$STATION[225])] = 24.17
spat_full_data$LON[225] = full_data$LON[which(full_data$STATION == spat_full_data$STATION[225])] = 88.27
spat_full_data$LAT[242] = full_data$LAT[which(full_data$STATION == spat_full_data$STATION[242])] = 23.34
spat_full_data$LON[242] = full_data$LON[which(full_data$STATION == spat_full_data$STATION[242])] = 86.36
spat_full_data$LAT[248] = full_data$LAT[which(full_data$STATION == spat_full_data$STATION[248])] = 25.87
spat_full_data$LON[248] = full_data$LON[which(full_data$STATION == spat_full_data$STATION[248])] = 87.84
spat_full_data$LAT[284] = full_data$LAT[which(full_data$STATION == spat_full_data$STATION[284])] = 25.27
spat_full_data$LON[284] = full_data$LON[which(full_data$STATION == spat_full_data$STATION[284])] = 88.78
spat_full_data$LAT[290] = full_data$LAT[which(full_data$STATION == spat_full_data$STATION[290])] = 23.12
spat_full_data$LON[290] = full_data$LON[which(full_data$STATION == spat_full_data$STATION[290])] = 88.46
spat_full_data$LAT[305] = full_data$LAT[which(full_data$STATION == spat_full_data$STATION[305])] = 25.17
spat_full_data$LON[305] = full_data$LON[which(full_data$STATION == spat_full_data$STATION[305])] = 88.37
spat_full_data$LAT[331] = full_data$LAT[which(full_data$STATION == spat_full_data$STATION[331])] = 26.37
spat_full_data$LON[331] = full_data$LON[which(full_data$STATION == spat_full_data$STATION[331])] = 88.31
spat_full_data$LAT[335] = full_data$LAT[which(full_data$STATION == spat_full_data$STATION[335])] = 25.64
spat_full_data$LON[335] = full_data$LON[which(full_data$STATION == spat_full_data$STATION[335])] = 88.32

# adjusting for the repeated cases
full_data_avg <- full_data %>%
  group_by(LAT, LON, DISTRICT, STATION, YEAR) %>%
  summarise(across(
    where(is.numeric),
    ~ if (all(is.na(.))) NA_real_ else mean(., na.rm = TRUE)
  ), .groups = "drop")

full_data = full_data_avg


# adjusted sum function
sum_a = function(x){
  return(ifelse(sum(is.na(x)) > length(x)/2, NA, mean(x,na.rm=T)*length(x)))
}

# aggregating monthly rainfall data station wise and year wise, adjusting for the NA cases
data_monthly_agg <- full_data %>%
  rowwise() %>%
  mutate(
    January = sum_a(c_across(J1:J31)),
    February = sum_a(c_across(F1:F29)),
    March = sum_a(c_across(M1:M31)),
    April = sum_a(c_across(A1:A30)),
    May = sum_a(c_across(My1:My31)),
    June = sum_a(c_across(Jn1:Jn30)),
    July = sum_a(c_across(Jl1:Jl31)),
    August = sum_a(c_across(Ag1:Ag31)),
    September = sum_a(c_across(S1:S30)),
    October = sum_a(c_across(O1:O31)),
    November = sum_a(c_across(N1:N30)),
    December = sum_a(c_across(D1:D31))
  ) %>%
  ungroup() %>%
  select(LAT, LON, DISTRICT, STATION, YEAR, January,February,
         March,April,May,June,July,August,September,October,November,December)


# visulaizing the missingness. 
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

# putting the data in long form
data_long <- data_monthly_agg %>%
  pivot_longer(
    cols = January:December,
    names_to = "month",
    values_to = "rain"
  ) %>%
  select(STATION, LAT, LON, YEAR, month, rain)

# calculating the missingness
missing_info <- full_grid %>%
  left_join(data_long, by = c("STATION", "YEAR", "month")) %>%
  mutate(
    missing = is.na(rain),
    month = factor(month, levels = month.name, ordered = TRUE) 
  ) %>%
  group_by(YEAR, month) %>%
  summarise(
    missing_count = sum(missing)/length(all_stations),
    .groups = "drop"
  ) %>%
  arrange(YEAR, month)

# visualizing the missingness
ggplot(missing_info, aes(x = YEAR, y = missing_count)) +
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

# Extract month from daycode
data_long_daily <- data_long_daily %>%
  mutate(
    month_abbr = str_extract(daycode, "^[A-Za-z]+"),  # take the leading letters
    month = case_when(
      month_abbr %in% c("J")    ~ "January",
      month_abbr %in% c("F")    ~ "February",
      month_abbr %in% c("M")    ~ "March",
      month_abbr %in% c("A")    ~ "April",
      month_abbr %in% c("My")   ~ "May",
      month_abbr %in% c("Jn")   ~ "June",
      month_abbr %in% c("Jl")   ~ "July",
      month_abbr %in% c("Ag")   ~ "August",
      month_abbr %in% c("S")    ~ "September",
      month_abbr %in% c("O")    ~ "October",
      month_abbr %in% c("N")    ~ "November",
      month_abbr %in% c("D")    ~ "December",
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

# Categorize into bins
station_month_missing <- station_month_missing %>%
  mutate(
    category = case_when(
      missing_days <= 5  ~ "0–5 days",
      missing_days <= 15 ~ "6–15 days",
      TRUE               ~ ">15 days"
    )
  )

# Count stations per year-month-category
summary_counts <- station_month_missing %>%
  group_by(YEAR, month, category) %>%
  summarise(stations = n()/length(all_stations), .groups = "drop")

# Facet Plot
ggplot(summary_counts, aes(x = YEAR, y = stations, color = category)) +
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
    legend.position = "bottom",              # move legend to bottom
    legend.box = "horizontal"                # arrange items in a row
  )


# count months with data per station ----
months_per_station <- data_long %>%
  group_by(STATION, LAT, LON) %>%
  summarise(months_with_data = sum(!is.na(rain))/((2020-1901+1)*12), .groups = "drop")

# get map of India and crop to West Bengal ----
india <- rnaturalearth::ne_states(country = "India", returnclass = "sf")
west_bengal <- india %>% filter(name == "West Bengal")

# plot
ggplot() +
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


# compute avg rainfall per station per year 
avg_rain_per_year <- data_long %>%
  filter(YEAR %in% c(1901, 1941, 1981, 2020)) %>%
  group_by(STATION, LAT, LON, YEAR) %>%
  summarise(avg_rain = mean(rain, na.rm = TRUE), .groups = "drop")

# facet map
ggplot() +
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


data_monthly_agg %>%
  group_by(LAT, LON, DISTRICT, STATION) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

### Moving Average Method

### ALIPUR
station_alipur = 72 #ALIPUR

station = "ALIPUR          "
station_daily = full_data[which(full_data$STATION == "ALIPUR          "), ]
station_month_ind = which(data_monthly_agg$STATION == station)
dat_station_month = as.matrix(data_monthly_agg[station_month_ind,-c(1:4)])
start_year_station = 1901
end_year_station = 2020
# imputing using previous year data
new_dat_station1 = matrix(0,nrow = 120,ncol= ncol(dat_station_month))
new_dat_station1[1:42,] = dat_station_month[1:42,]
new_dat_station1[43,] = dat_station_month[42,]
new_dat_station1[43,1] = 1943
new_dat_station1[44:109,] = dat_station_month[43:108,]
new_dat_station1[110,] = dat_station_month[108,]
new_dat_station1[110,1] = 2010
new_dat_station1[111:120,] = dat_station_month[109:118,]
dat_station1 = new_dat_station1
station1_rain = na.locf(as.numeric(t(dat_station1[,-1])))
year_station1 = rep(1901:2020,
                    each = 12)
month = rep(1:12, 120)
df1 = cbind.data.frame(year_station1,month,station1_rain)
# use a 30 year moving average for estimating the trend
df1$ma12 <- stats::filter(df1$station1_rain, filter = rep(1/360, 360), sides = 2)
trend0 = df1$ma12
trend0[is.na(trend0)] = 0
df1$diff <- df1$station1_rain - trend0
df1$diff[is.na(df1$ma12)] = NA

# estimate the seasonality
season <- na.omit(df1$diff)
dat1 = na.omit(df1 %>% 
                 filter(year_station1 %in% c(1901:1930)))
dat2 = na.omit(df1 %>% 
                 filter(year_station1 %in% c(1931:1960)))
dat3 = na.omit(df1 %>% 
                 filter(year_station1 %in% c(1961:1990)))
dat4 = na.omit(df1 %>% 
                 filter(year_station1 %in% c(1991:2020)))
dat1$sn = dat2$sn = dat3$sn = dat4$sn = 0
for(i in 1:12){
  dat1$sn[which(dat1$month == i)] = mean(dat1[which(dat1$month == i),5])
}
for(i in 1:12){
  dat2$sn[which(dat2$month == i)] = mean(dat2[which(dat2$month == i),5])
}
for(i in 1:12){
  dat3$sn[which(dat3$month == i)] = mean(dat3[which(dat3$month == i),5])
}
for(i in 1:12){
  dat4$sn[which(dat4$month == i)] = mean(dat4[which(dat4$month == i),5])
}
df1$sn = c(rep(NA,360-(length(dat1$sn))),dat1$sn,dat2$sn,dat3$sn,dat4$sn,rep(NA,360-(length(dat4$sn))))
df2 = na.omit(df1)
seasonality1 = dat1$sn[2:13] 
seasonality2 = dat2$sn[1:12] 
seasonality3 = dat3$sn[1:12] 
seasonality4 = dat4$sn[1:12] 

season = cbind.data.frame(time1 = seasonality1,time2 = seasonality2,
                          time3 = seasonality3,time4 = seasonality4)

# plot trend and seasonality
df2 %>% 
  ggplot() +
  aes(x = year_station1, y = ma12) +
  geom_line(color = "blue", size = 1.2) +  # Use a teal-like color for the line
  scale_x_continuous(breaks = seq(1901, 2020, by = 10), limits = c(1901, 2020)) +  # Set x-axis limits and breaks
  labs(x = "Year (Monthwise)", y = "Trend",  title = "MA Trend Estimate for ALIPUR") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
  )


season %>% 
  ggplot()+
  geom_line(aes(x = 1:12,y = time1, colour = "1901-1930"))+
  geom_line(aes(x = 1:12,y = time2, colour = "1931-1960"))+
  geom_line(aes(x = 1:12,y = time3, colour = "1961-1990"))+
  geom_line(aes(x = 1:12,y = time4, colour = "1991-2020"))+
  labs(x = "Months", y = "Seasonality", title = "MA Seasonality Estimate for ALIPUR")+
  labs(color = "Time Period")+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "bottom",              # move legend to bottom
    legend.box = "horizontal"                # arrange items in a row
  )


MA_trend_alipur = df2$ma12
MA_season_alipur = season
MA_pred_alipur = df1$ma12 + df1$sn



### MALDA
station2 = "MALDA           "
station2_ind = which(data_monthly_agg$STATION == station2)
dat_station2 = as.matrix(data_monthly_agg[station2_ind,-c(1:4)])
new_dat_station2 = matrix(0,nrow = 120,ncol= ncol(dat_station2))
#missing years 1912 1913 1914 1920 1922 1926 1976 1977 1978 1980 1983
setdiff(c(1901:2020), dat_station2[,1])
new_dat_station2[1:11,] = dat_station2[1:11,]
new_dat_station2[12,] = new_dat_station2[13,] = new_dat_station2[14,] = new_dat_station2[11,]
new_dat_station2[12,1] = 1912
new_dat_station2[13,1] = 1913
new_dat_station2[14,1] = 1914
new_dat_station2[15:19,] = dat_station2[12:16,]
new_dat_station2[20,] = new_dat_station2[19,]
new_dat_station2[20,1] = 1920
new_dat_station2[21,] = dat_station2[17,]
new_dat_station2[22,] = new_dat_station2[21,]
new_dat_station2[22,1] = 1922
new_dat_station2[23:25,] = dat_station2[18:20,] 
new_dat_station2[26,] = new_dat_station2[25,]
new_dat_station2[26,1] = 1926
new_dat_station2[27:75,] = dat_station2[21:69,]
new_dat_station2[76,] = new_dat_station2[75,] 
new_dat_station2[77,] = new_dat_station2[75,] 
new_dat_station2[78,] = new_dat_station2[75,] 
new_dat_station2[76,1] = 1976
new_dat_station2[77,1] = 1977
new_dat_station2[78,1] = 1978
new_dat_station2[79,] = dat_station2[70,]
new_dat_station2[80,] = new_dat_station2[79,]
new_dat_station2[80,1] = 1980
new_dat_station2[81:82,] = dat_station2[71:72,]
new_dat_station2[83,] = new_dat_station2[82,]
new_dat_station2[83,1] = 1983
new_dat_station2[84:120,] = dat_station2[73:109,] 
dat_station2 = new_dat_station2
station2_rain =na.locf (as.numeric(t(dat_station2[,-1])))
year_station2 = rep(1901:2020,
                    each = 12)
month = rep(1:12, 120)
df2 = cbind.data.frame(year_station2,month,station2_rain)

###Now let's do manually, assuming 30 year seasonality to be same 
df2$ma <- stats::filter(df2$station2_rain, filter = rep(1/360, 360), sides = 2)
trend0 = df2$ma
trend0[is.na(trend0)] = 0
df2$diff <- df2$station2_rain - trend0
df2$diff[is.na(df2$ma)] = NA
season = na.omit(df2$diff)
dat21 = na.omit(df2 %>% 
                  filter(year_station2 %in% c(1901:1930)))
dat22 = na.omit(df2 %>% 
                  filter(year_station2 %in% c(1931:1960)))
dat23 = na.omit(df2 %>% 
                  filter(year_station2 %in% c(1961:1990)))
dat24 = na.omit(df2 %>% 
                  filter(year_station2 %in% c(1991:2020)))
dat21$sn = dat22$sn = dat23$sn = dat24$sn = 0
for(i in 1:12){
  dat21$sn[which(dat21$month == i)] = mean(dat21[which(dat21$month == i),5])
}
for(i in 1:12){
  dat22$sn[which(dat22$month == i)] = mean(dat22[which(dat22$month == i),5])
}
for(i in 1:12){
  dat23$sn[which(dat23$month == i)] = mean(dat23[which(dat23$month == i),5])
}
for(i in 1:12){
  dat24$sn[which(dat24$month == i)] = mean(dat24[which(dat24$month == i),5])
}
df2$sn = c(rep(NA,179),dat21$sn,dat22$sn,dat23$sn,dat24$sn,rep(NA,180))
df2_2 = na.omit(df2)

seasonality1_2 = dat21$sn[2:13] 
seasonality2_2 = dat22$sn[1:12] 
seasonality3_2 = dat23$sn[1:12] 
seasonality4_2 = dat24$sn[1:12] 

season2 = cbind.data.frame(time1 = seasonality1_2,time2 = seasonality2_2,
                           time3 = seasonality3_2,time4 = seasonality4_2)

df2_2 %>% 
  ggplot() +
  aes(x = year_station2, y = ma) +
  geom_line(color = "blue", size = 1.2) +  # Use a teal-like color for the line
  scale_x_continuous(breaks = seq(1901, 2020, by = 10), limits = c(1901, 2020)) +  # Set x-axis limits and breaks
  labs(x = "Year (Monthwise)", y = "Trend", title = "MA Trend Estimate for MALDA") +  # Customize axis labels
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
  )


season2 %>% 
  ggplot()+
  geom_line(aes(x = 1:12,y = time1, colour = "1901-1930"))+
  geom_line(aes(x = 1:12,y = time2, colour = "1931-1960"))+
  geom_line(aes(x = 1:12,y = time3, colour = "1961-1990"))+
  geom_line(aes(x = 1:12,y = time4, colour = "1991-2020"))+
  labs(x = "Months", y = "Seasonality", title = "MA Seasonality Estimate for MALDA")+
  labs(color = "Time Period")+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "bottom",              # move legend to bottom
    legend.box = "horizontal"                # arrange items in a row
  )


MA_trend_malda = df2_2$ma
MA_season_malda = season2
MA_pred_malda = df2_2$ma + df2_2$sn



### MALDA
station3 = "MIDNAPORE       "
station3_ind = which(data_monthly_agg$STATION == station3)
dat_station3 = as.data.frame(data_monthly_agg[station3_ind,-c(1:4)])
new_dat_station3 <- dat_station3 %>%
  fill(`month.name`, .direction = "down")
dat_station3 = new_dat_station3
station3_rain =na.locf (as.numeric(t(dat_station3[,-1])))
year_station3 = rep(1901:2020,
                    each = 12)
month = rep(1:12, 120)
df3 = cbind.data.frame(year_station3,month,station3_rain)
###Now let's do manually, assuming 30 year seasonality to be same 
df3$ma <- stats::filter(df3$station3_rain, filter = rep(1/360, 360), sides = 2)
trend0 = df3$ma
trend0[is.na(trend0)] = 0
df3$diff <- df3$station3_rain - trend0
df3$diff[is.na(df3$ma)] = NA
season = na.omit(df3$diff)
dat31 = na.omit(df3 %>% 
                  filter(year_station3 %in% c(1901:1930)))
dat32 = na.omit(df3 %>% 
                  filter(year_station3 %in% c(1931:1960)))
dat33 = na.omit(df3 %>% 
                  filter(year_station3 %in% c(1961:1990)))
dat34 = na.omit(df3 %>% 
                  filter(year_station3 %in% c(1991:2020)))
dat31$sn = dat32$sn = dat33$sn = dat34$sn = 0
for(i in 1:12){
  dat31$sn[which(dat31$month == i)] = mean(dat31[which(dat31$month == i),5])
}
for(i in 1:12){
  dat32$sn[which(dat32$month == i)] = mean(dat32[which(dat32$month == i),5])
}
for(i in 1:12){
  dat33$sn[which(dat33$month == i)] = mean(dat33[which(dat33$month == i),5])
}
for(i in 1:12){
  dat34$sn[which(dat34$month == i)] = mean(dat34[which(dat34$month == i),5])
}
df3$sn = c(rep(NA,179),dat31$sn,dat32$sn,dat33$sn,dat34$sn,rep(NA,180))
df3_3 = na.omit(df3)

seasonality1_3 = dat31$sn[2:13] 
seasonality2_3 = dat32$sn[1:12] 
seasonality3_3 = dat33$sn[1:12] 
seasonality4_3 = dat34$sn[1:12] 

season3 = cbind.data.frame(time1 = seasonality1_3,time2 = seasonality2_3,
                           time3 = seasonality3_3,time4 = seasonality4_3)

df3_3 %>% 
  ggplot() +
  aes(x = year_station3, y = ma) +
  geom_line(color = "blue", size = 1.2) +  # Use a teal-like color for the line
  scale_x_continuous(breaks = seq(1901, 2020, by = 10), limits = c(1901, 2020)) +  # Set x-axis limits and breaks
  labs(x = "Year (Monthwise)", y = "Trend", title = "MA Trend Estimate for MIDNAPORE") +  # Customize axis labels
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
  )


season3 %>% 
  ggplot()+
  geom_line(aes(x = 1:12,y = time1, colour = "1901-1930"))+
  geom_line(aes(x = 1:12,y = time2, colour = "1931-1960"))+
  geom_line(aes(x = 1:12,y = time3, colour = "1961-1990"))+
  geom_line(aes(x = 1:12,y = time4, colour = "1991-2020"))+
  labs(x = "Months", y = "Seasonality", title = "MA Seasonality Estimate for MIDNAPORE")+
  labs(color = "Time Period")+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "bottom",              # move legend to bottom
    legend.box = "horizontal"                # arrange items in a row
  )

MA_trend_midnapore = df3_3$ma
MA_season_midnapore = season3
MA_pred_midnpore = df3_3$ma + df3_3$sn

### LAT LON spatial dist
spatial_dist = function(i,j){
  s_i = as.numeric(spat_full_data[i,1:2])
  s_j = as.numeric(spat_full_data[j,1:2])
  phi1 = s_i[1]*pi/180
  phi2 = s_j[1]*pi/180
  lambda1 = s_i[2]*pi/180
  lambda2 = s_j[2]*pi/180
  d_ij = sin((phi2-phi1)/2)^2 + cos(phi1)*cos(phi2)*sin((lambda2-lambda1)/2)^2
  d = 2*6371* asin(sqrt(d_ij))
  return(d)
}

colnames(spat_full_data) = c("latitude","longitude","station")
spat_full_data$latitude = as.numeric(spat_full_data$latitude)
spat_full_data$longitude = as.numeric(spat_full_data$longitude)
spat_full_data$region <- ifelse(spat_full_data$latitude > 25, "N",
                                ifelse(spat_full_data$longitude < 87.5, "W", "S"))

### Basis Functions
df = 8
ss_period = 4
tot_x = c(1:length(rep(seq(1901,2020),each=12)))
trend_basis = bSpline(tot_x,df=df,degree=3,intercept = T)
len = length(tot_x)/ss_period
seasonality_basis = matrix(0,nrow=length(tot_x), ncol = 12*ss_period)
for(block in 1:ss_period){
  seasonality_basis[seq(len*(block-1)+1,len*block,1),seq(12*(block-1)+1,12*block,1)] = 
    cbind(cos(pi * 0.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 1.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 2.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 3.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 4.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 5.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 6.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 7.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 8.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 9.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 10.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 11.5 * (c(1:len)%%12+0.5) / 12))
}
seasonality_basis_1 = seasonality_basis
rowsum_ssbs = apply(seasonality_basis[1:12,1:12],2,sum)
rowsum_ssbs = rowsum_ssbs/rowsum_ssbs[12]
rowsum_ssbs = c(rowsum_ssbs[12],rowsum_ssbs[1:11])
for(i in 1:(ncol(seasonality_basis))){
  seasonality_basis_1[,i] = seasonality_basis_1[,i] - rowsum_ssbs[i%%12+1] * seasonality_basis_1[,ncol(seasonality_basis_1)]
}
seasonality_basis_1 = seasonality_basis_1[,-(ss_period*12)]

trend_ss_basis = cbind(trend_basis,seasonality_basis_1)

trend_ss_matrix = function(s,df){
  station =  as.character(spat_full_data[s,3])
  station_month_ind = which(data_monthly_agg$STATION == station)
  dat_station_month = as.data.frame(data_monthly_agg[station_month_ind,-c(1:4)])
  start_year_station = 1901
  end_year_station = 2020
  missing_year = setdiff(c(start_year_station:end_year_station), dat_station_month$YEAR)
  new_dat_station_month = matrix(0,nrow = (end_year_station - start_year_station + 1),ncol= ncol(dat_station_month))
  new_dat_station_month[,1] = c(start_year_station:end_year_station)
  for(i in 1:nrow(new_dat_station_month)){
    if(new_dat_station_month[i,1] %in% dat_station_month$YEAR) new_dat_station_month[i,] = as.numeric(dat_station_month[which(dat_station_month$YEAR == new_dat_station_month[i,1])[1],])
    else new_dat_station_month[i,2:13] = NA 
  }
  dat_station_month = new_dat_station_month
  station_rain = as.numeric(t(dat_station_month[,-1]))
  year_station = rep(c(start_year_station:end_year_station),
                     each = 12)
  month = rep(1:12, (end_year_station-start_year_station+1))
  df_station = cbind.data.frame(year_station,month,station_rain)
  station_data = df_station[,c(1,2,3)] 
  x = c(1:length(rep(seq(start_year_station,end_year_station),each=12)))
  y = station_data$station_rain
  w = ifelse(is.na(y),0,1)
  station_data$indicator = w
  trend_ss_basis_s = trend_ss_basis * w
  return(list(rain = y, basis = trend_ss_basis_s, trend_ss_mat = t(trend_ss_basis_s)%*%trend_ss_basis_s, indicator = w))
}

trend_ss_matrix_train_test = function(s,df,ratio=0.9){
  station =as.character(spat_full_data[s,3])
  station_month_ind = which(data_monthly_agg$STATION == station)
  dat_station_month = as.data.frame(data_monthly_agg[station_month_ind,-c(1:4)])
  start_year_station = 1901
  end_year_station = 2020
  missing_year = setdiff(c(start_year_station:end_year_station), dat_station_month$YEAR)
  new_dat_station_month = matrix(0,nrow = (end_year_station - start_year_station + 1),ncol= ncol(dat_station_month))
  new_dat_station_month[,1] = c(start_year_station:end_year_station)
  for(i in 1:nrow(new_dat_station_month)){
    if(new_dat_station_month[i,1] %in% dat_station_month$YEAR) new_dat_station_month[i,] = as.numeric(dat_station_month[which(dat_station_month$YEAR == new_dat_station_month[i,1])[1],])
    else new_dat_station_month[i,2:13] = NA 
  }
  dat_station_month = new_dat_station_month
  station_rain = as.numeric(t(dat_station_month[,-1]))
  non_na <- which(!is.na(station_rain))
  perm <- sample(non_na, length(non_na))  
  i90 <- perm[1:floor(ratio * length(perm))] #train index
  i10 <- perm[(floor(ratio * length(perm)) + 1):length(perm)] #test index
  train <- replace(station_rain, -i90, NA)
  test <- replace(station_rain, -i10, NA)
  year_station = rep(c(start_year_station:end_year_station),
                     each = 12)
  month = rep(1:12, (end_year_station-start_year_station+1))
  df_station_train = cbind.data.frame(year_station,month,train)
  station_data = df_station_train[,c(1,2,3)] 
  x = c(1:length(rep(seq(start_year_station,end_year_station),each=12)))
  y = station_data$train
  w = ifelse(is.na(y),0,1)
  station_data$indicator = w
  trend_ss_basis_s = trend_ss_basis * w
  return(list(rain = y, basis = trend_ss_basis_s, trend_ss_mat = t(trend_ss_basis_s)%*%trend_ss_basis_s, indicator = w, test = test))
}

tot_x_ext = c(1:length(rep(seq(1891,2030),each=12)))
trend_basis_ext = bSpline(tot_x_ext,df=df,degree=3,intercept = T)
ss_period = 4
len = length(tot_x_ext)/ss_period
seasonality_basis_ext = matrix(0,nrow=length(tot_x_ext), ncol = 12*ss_period)
for(block in 1:ss_period){
  seasonality_basis_ext[seq(len*(block-1)+1,len*block,1),seq(12*(block-1)+1,12*block,1)] = 
    cbind(cos(pi * 0.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 1.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 2.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 3.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 4.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 5.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 6.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 7.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 8.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 9.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 10.5 * (c(1:len)%%12+0.5) / 12),
          cos(pi * 11.5 * (c(1:len)%%12+0.5) / 12))
}
seasonality_basis_1_ext = seasonality_basis_ext
rowsum_ssbs_ext = apply(seasonality_basis_ext[1:12,1:12],2,sum)
rowsum_ssbs_ext = rowsum_ssbs_ext/rowsum_ssbs_ext[12]
rowsum_ssbs_ext = c(rowsum_ssbs_ext[12],rowsum_ssbs_ext[1:11])
for(i in 1:(ncol(seasonality_basis_ext))){
  seasonality_basis_1_ext[,i] = seasonality_basis_1_ext[,i] - rowsum_ssbs_ext[i%%12+1] * seasonality_basis_1_ext[,ncol(seasonality_basis_1_ext)]
}
seasonality_basis_1_ext = seasonality_basis_1_ext[,-(ss_period*12)]

trend_ss_basis_ext = cbind(trend_basis_ext,seasonality_basis_1_ext)

trend_ss_matrix_ext = function(s,df){
  station = as.character(spat_full_data[s,3])
  station_month_ind = which(data_monthly_agg$STATION == station)
  dat_station_month = as.data.frame(data_monthly_agg[station_month_ind,-c(1:4)])
  start_year_station = 1891
  end_year_station = 2030
  missing_year = setdiff(c(start_year_station:end_year_station), dat_station_month$YEAR)
  new_dat_station_month = matrix(0,nrow = (end_year_station - start_year_station + 1),ncol= ncol(dat_station_month))
  new_dat_station_month[,1] = c(start_year_station:end_year_station)
  for(i in 1:nrow(new_dat_station_month)){
    if(new_dat_station_month[i,1] %in% dat_station_month$YEAR) new_dat_station_month[i,] = as.numeric(dat_station_month[which(dat_station_month$YEAR == new_dat_station_month[i,1])[1],])
    else new_dat_station_month[i,2:13] = NA 
  }
  dat_station_month = new_dat_station_month
  dat_station_month[1:10,1:12] = dat_station_month[11:20,1:12]
  dat_station_month[131:140,1:12] = dat_station_month[121:130,1:12]
  station_rain = as.numeric(t(dat_station_month[,-1]))
  year_station = rep(c(start_year_station:end_year_station),
                     each = 12)
  month = rep(1:12, (end_year_station-start_year_station+1))
  df_station = cbind.data.frame(year_station,month,station_rain)
  station_data = df_station[,c(1,2,3)] 
  x = c(1:length(rep(seq(start_year_station,end_year_station),each=12)))
  y = station_data$station_rain
  w = ifelse(is.na(y),0,1)
  station_data$indicator = w
  trend_ss_basis_s = trend_ss_basis_ext * w
  return(list(rain = y, basis = trend_ss_basis_s, trend_ss_mat = t(trend_ss_basis_s)%*%trend_ss_basis_s, indicator = w))
}

trend_ss_matrix_train_test_ext = function(s,df,ratio=0.9){
  station = as.character(spat_full_data[s,3])
  station_month_ind = which(data_monthly_agg$STATION == station)
  dat_station_month = as.data.frame(data_monthly_agg[station_month_ind,-c(1:4)])
  start_year_station = 1891
  end_year_station = 2030
  missing_year = setdiff(c(start_year_station:end_year_station), dat_station_month$YEAR)
  new_dat_station_month = matrix(0,nrow = (end_year_station - start_year_station + 1),ncol= ncol(dat_station_month))
  new_dat_station_month[,1] = c(start_year_station:end_year_station)
  for(i in 1:nrow(new_dat_station_month)){
    if(new_dat_station_month[i,1] %in% dat_station_month$YEAR) new_dat_station_month[i,] = as.numeric(dat_station_month[which(dat_station_month$YEAR == new_dat_station_month[i,1])[1],])
    else new_dat_station_month[i,2:13] = NA 
  }
  dat_station_month = new_dat_station_month
  dat_station_month[1:10,1:12] = dat_station_month[11:20,1:12]
  dat_station_month[131:140,1:12] = dat_station_month[121:130,1:12]
  station_rain = as.numeric(t(dat_station_month[,-1]))
  non_na <- which(!is.na(station_rain))
  perm <- sample(non_na, length(non_na))  
  i90 <- perm[1:floor(ratio * length(perm))] #train index
  i10 <- perm[(floor(ratio * length(perm)) + 1):length(perm)] #test index
  train <- replace(station_rain, -i90, NA)
  test <- replace(station_rain, -i10, NA)
  year_station = rep(c(start_year_station:end_year_station),
                     each = 12)
  month = rep(1:12, (end_year_station-start_year_station+1))
  df_station_train = cbind.data.frame(year_station,month,train)
  station_data = df_station_train[,c(1,2,3)] 
  x = c(1:length(rep(seq(start_year_station,end_year_station),each=12)))
  y = station_data$train
  w = ifelse(is.na(y),0,1)
  station_data$indicator = w
  trend_ss_basis_s = trend_ss_basis_ext * w
  return(list(rain = y, basis = trend_ss_basis_s, trend_ss_mat = t(trend_ss_basis_s)%*%trend_ss_basis_s, indicator = w, test = test))
}

### Kernel Smoothing 
trend_ss_estimate_smoothing = function(station_cur_ind,df,h,knn){
  dist_station_cur = numeric()
  for(i in 1:nrow(spat_full_data)){
    dist_station_cur[i] = spatial_dist(station_cur_ind,i)
  }
  nearest_station_cur = sort(dist_station_cur, decreasing = F, index.return = T)$ix[-1][1:9]
  sorted_distances = sort(dist_station_cur,decreasing = F)
  sorted_distances = sorted_distances/max(sorted_distances)
  t0 = trend_ss_matrix(station_cur_ind,df)
  a0 = (1/h) * t0$trend_ss_mat
  t0_rain = t0$rain
  t0_rain[is.na(t0_rain)] = 0
  b0 = (1/h) * t(t0$basis)%*%as.matrix(t0_rain,1)
  a_list = list(a0)
  b_list = list(b0)
  indices <- 1:knn
  for (i in indices) {
    var_name_1 <- paste0("t", i)
    assign(var_name_1, trend_ss_matrix(nearest_station_cur[i], df))
    var_name_2 <- paste0("a", i)
    var_name_3 <- paste0("b", i)
    t_value <- get(var_name_1)
    assign(var_name_2, (1/h) * 1/(sqrt(2*pi)) * exp(-((sorted_distances[i+1]/h)^2)/2) * t_value$trend_ss_mat)
    t_rain = t_value$rain
    t_rain[is.na(t_rain)] = 0
    assign(var_name_3, (1/h) * 1/(sqrt(2*pi)) * exp(-((sorted_distances[i+1]/h)^2)/2) * t(t_value$basis)%*%as.matrix(t_rain,1))
    a_list[[i+1]] = get(var_name_2)
    b_list[[i+1]] = get(var_name_3)
  }
  A = Reduce('+',a_list)
  b = Reduce('+',b_list)
  coeff = solve(t(A)%*%A)%*%t(A)%*%b
  beta_trend = coeff[1:df]
  beta_ss = coeff[(df+1):length(coeff)]
  trend = as.numeric(as.matrix(trend_basis %*% as.matrix(beta_trend)))
  ss = as.numeric(as.matrix(seasonality_basis_1 %*% as.matrix(beta_ss)))
  ss_df = matrix(ss, ncol = 12, byrow = T)
  seasonality_period = as.data.frame(t(ss_df[!duplicated(ss_df),]))
  random = t0$rain - trend - ss
  pred = trend + ss
  return(list(trend = trend, seasonality_period = seasonality_period, rain = t0$rain, predicted = pred, error = random))
}

#### test train split
trend_ss_estimate_smoothing_train_test = function(station_cur_ind,df,h,knn,ratio=0.9){
  dist_station_cur = numeric()
  for(i in 1:nrow(spat_full_data)){
    dist_station_cur[i] = spatial_dist(station_cur_ind,i)
  }
  nearest_station_cur = sort(dist_station_cur, decreasing = F, index.return = T)$ix[-1][1:9]
  sorted_distances = sort(dist_station_cur,decreasing = F)
  sorted_distances = sorted_distances/max(sorted_distances)
  t0 = trend_ss_matrix_train_test(station_cur_ind,df,ratio)
  a0 = (1/h) * t0$trend_ss_mat
  t0_rain = t0$rain
  t0_rain[is.na(t0_rain)] = 0
  b0 = (1/h) * t(t0$basis)%*%as.matrix(t0_rain,1)
  a_list = list(a0)
  b_list = list(b0)
  indices <- 1:knn
  for (i in indices) {
    var_name_1 <- paste0("t", i)
    assign(var_name_1, trend_ss_matrix(nearest_station_cur[i], df))
    var_name_2 <- paste0("a", i)
    var_name_3 <- paste0("b", i)
    t_value <- get(var_name_1)
    assign(var_name_2, (1/h) * 1/(sqrt(2*pi)) * exp(-((sorted_distances[i+1]/h)^2)/2) * t_value$trend_ss_mat)
    t_rain = t_value$rain
    t_rain[is.na(t_rain)] = 0
    assign(var_name_3, (1/h) * 1/(sqrt(2*pi)) * exp(-((sorted_distances[i+1]/h)^2)/2) * t(t_value$basis)%*%as.matrix(t_rain,1))
    a_list[[i+1]] = get(var_name_2)
    b_list[[i+1]] = get(var_name_3)
  }
  A = Reduce('+',a_list)
  b = Reduce('+',b_list)
  coeff = solve(t(A)%*%A)%*%t(A)%*%b
  beta_trend = coeff[1:df]
  beta_ss = coeff[(df+1):length(coeff)]
  trend = as.numeric(as.matrix(trend_basis %*% as.matrix(beta_trend)))
  ss = as.numeric(as.matrix(seasonality_basis_1 %*% as.matrix(beta_ss)))
  ss_df = matrix(ss, ncol = 12, byrow = T)
  seasonality_period = as.data.frame(t(ss_df[!duplicated(ss_df),]))
  random = t0$rain - trend - ss
  pred = trend + ss
  return(list(trend = trend, seasonality_period = seasonality_period, rain = t0$rain, predicted = pred, error = random, test = t0$test))
}

#### reflecting boundary
trend_ss_estimate_smoothing_ext = function(station_cur_ind,df,h,knn){
  dist_station_cur = numeric()
  for(i in 1:nrow(spat_full_data)){
    dist_station_cur[i] = spatial_dist(station_cur_ind,i)
  }
  nearest_station_cur = sort(dist_station_cur, decreasing = F, index.return = T)$ix[-1][1:9]
  sorted_distances = sort(dist_station_cur,decreasing = F)
  sorted_distances = sorted_distances/max(sorted_distances)
  t0 = trend_ss_matrix_ext(station_cur_ind,df)
  a0 = (1/h) * t0$trend_ss_mat
  t0_rain = t0$rain
  t0_rain[is.na(t0_rain)] = 0
  b0 = (1/h) * t(t0$basis)%*%as.matrix(t0_rain,1)
  a_list = list(a0)
  b_list = list(b0)
  indices <- 1:knn
  for (i in indices) {
    var_name_1 <- paste0("t", i)
    assign(var_name_1, trend_ss_matrix_ext(nearest_station_cur[i], df))
    var_name_2 <- paste0("a", i)
    var_name_3 <- paste0("b", i)
    t_value <- get(var_name_1)
    assign(var_name_2, (1/h) * 1/(sqrt(2*pi)) * exp(-((sorted_distances[i+1]/h)^2)/2) * t_value$trend_ss_mat)
    t_rain = t_value$rain
    t_rain[is.na(t_rain)] = 0
    assign(var_name_3, (1/h) * 1/(sqrt(2*pi)) * exp(-((sorted_distances[i+1]/h)^2)/2) * t(t_value$basis)%*%as.matrix(t_rain,1))
    a_list[[i+1]] = get(var_name_2)
    b_list[[i+1]] = get(var_name_3)
  }
  A = Reduce('+',a_list)
  b = Reduce('+',b_list)
  coeff = solve(t(A)%*%A)%*%t(A)%*%b
  beta_trend = coeff[1:df]
  beta_ss = coeff[(df+1):length(coeff)]
  trend = as.numeric(as.matrix(trend_basis_ext %*% as.matrix(beta_trend)))
  ss = as.numeric(as.matrix(seasonality_basis_1 %*% as.matrix(beta_ss)))
  ss_df = matrix(ss, ncol = 12, byrow = T)
  seasonality_period = as.data.frame(t(ss_df[!duplicated(ss_df),]))
  random = t0$rain - trend - ss
  pred = trend + ss
  return(list(trend = trend, seasonality_period = seasonality_period, rain = t0$rain, predicted = pred, error = random))
}

### train test for reflecting boundary
trend_ss_estimate_smoothing_train_test_ext = function(station_cur_ind,df,h,knn,ratio = 0.9){
  dist_station_cur = numeric()
  for(i in 1:nrow(spat_full_data)){
    dist_station_cur[i] = spatial_dist(station_cur_ind,i)
  }
  nearest_station_cur = sort(dist_station_cur, decreasing = F, index.return = T)$ix[-1][1:9]
  sorted_distances = sort(dist_station_cur,decreasing = F)
  sorted_distances = sorted_distances/max(sorted_distances)
  t0 = trend_ss_matrix_train_test_ext(station_cur_ind,df,ratio)
  a0 = (1/h) * t0$trend_ss_mat
  t0_rain = t0$rain
  t0_rain[is.na(t0_rain)] = 0
  b0 = (1/h) * t(t0$basis)%*%as.matrix(t0_rain,1)
  a_list = list(a0)
  b_list = list(b0)
  indices <- 1:knn
  for (i in indices) {
    var_name_1 <- paste0("t", i)
    assign(var_name_1, trend_ss_matrix_ext(nearest_station_cur[i], df))
    var_name_2 <- paste0("a", i)
    var_name_3 <- paste0("b", i)
    t_value <- get(var_name_1)
    assign(var_name_2, (1/h) * 1/(sqrt(2*pi)) * exp(-((sorted_distances[i+1]/h)^2)/2) * t_value$trend_ss_mat)
    t_rain = t_value$rain
    t_rain[is.na(t_rain)] = 0
    assign(var_name_3, (1/h) * 1/(sqrt(2*pi)) * exp(-((sorted_distances[i+1]/h)^2)/2) * t(t_value$basis)%*%as.matrix(t_rain,1))
    a_list[[i+1]] = get(var_name_2)
    b_list[[i+1]] = get(var_name_3)
  }
  A = Reduce('+',a_list)
  b = Reduce('+',b_list)
  coeff = solve(t(A)%*%A)%*%t(A)%*%b
  beta_trend = coeff[1:df]
  beta_ss = coeff[(df+1):length(coeff)]
  trend = as.numeric(as.matrix(trend_basis_ext %*% as.matrix(beta_trend)))
  ss = as.numeric(as.matrix(seasonality_basis_1 %*% as.matrix(beta_ss)))
  ss_df = matrix(ss, ncol = 12, byrow = T)
  seasonality_period = as.data.frame(t(ss_df[!duplicated(ss_df),]))
  random = t0$rain - trend - ss
  pred = trend + ss
  return(list(trend = trend, seasonality_period = seasonality_period, rain = t0$rain, predicted = pred, error = random, test = t0$test))
}

### choosing h
h_seq = c(0.01,0.05,0.1,0.5,1,5,10,20)
R = 10
mse_h_ts = numeric()
mse_h_val_ts = numeric()
for(i in 1:nrow(spat_full_data)){
  test_error = numeric()
  for(j in 1:length(h_seq)){
    test_error_j = 0
    
    for (r in 1:R) {
      result <- tryCatch({
        trend_ss_estimate_smoothing_train_test(i, 8, h_seq[j], 9)
      }, error = function(e) {
        NULL
      })
      if (!is.null(result)) {
        smooth_data_df <- as.data.frame(na.omit(cbind(pred = result$pred, act = result$test)))
        test_error_j <- test_error_j + mean((smooth_data_df$pred - smooth_data_df$act)^2)
      }
    }
    test_error_j <- test_error_j / R
    test_error[j] = test_error_j
  }
  non_zero_indices <- which(test_error > 0)
  min_non_zero_index <- non_zero_indices[which.min(test_error[non_zero_indices])]
  mse_h_val_ts[i] <- ifelse(sum(test_error)==0, NA, test_error[min_non_zero_index])
  mse_h_ts[i] = ifelse(sum(test_error) == 0, NA, h_seq[which.min(test_error)])
  print(i)
}

write.csv(mse_h_ts, "/Users/srijanch/Downloads/mse_h_ts.csv")
write.csv(mse_h_val_ts, "/Users/srijanch/Downloads/mse_h_val_ts.csv")

mse_h_ts_ext = numeric()
mse_h_val_ts_ext = numeric()
for(i in 1:nrow(spat_full_data)){
  test_error = numeric()
  for(j in 1:length(h_seq)){
    test_error_j = 0
    
    for (r in 1:R) {
      result <- tryCatch({
        trend_ss_estimate_smoothing_train_test_ext(i, 8, h_seq[j], 9)
      }, error = function(e) {
        NULL
      })
      if (!is.null(result)) {
        smooth_data_df <- as.data.frame(na.omit(cbind(pred = result$pred, act = result$test)))
        test_error_j <- test_error_j + mean((smooth_data_df$pred - smooth_data_df$act)^2)
      }
    }
    test_error_j <- test_error_j / R
    test_error[j] = test_error_j
  }
  non_zero_indices <- which(test_error > 0)
  min_non_zero_index <- non_zero_indices[which.min(test_error[non_zero_indices])]
  mse_h_val_ts_ext[i] <- ifelse(sum(test_error)==0, NA, test_error[min_non_zero_index])
  mse_h_ts_ext[i] = ifelse(sum(test_error) == 0, NA, h_seq[which.min(test_error)])
  print(i)
}


write.csv(mse_h_ts_ext, "/Users/srijanch/Downloads/mse_h_ts_ext.csv")
write.csv(mse_h_val_ts_ext, "/Users/srijanch/Downloads/mse_h_val_ts_ext.csv")

#### Penalization
EE1E2_s_diffpenalty = function(s,df,lambda1,lambda2){
  t_s = trend_ss_matrix(s,df)
  E2_s = t_s$trend_ss_mat
  L = diag(c(rep(lambda1,df),rep(lambda2,(nrow(E2_s)-df))))
  E_s = E2_s + L
  rain_s = t_s$rain
  rain_s[is.na(rain_s)] = 0
  E1_s = t(t_s$basis)%*%as.matrix(rain_s,1)
  return(list(E1 = E1_s, Es = E_s, E2 = E2_s, t0 = t_s))
}

FF1_diffpenalty = function(df,lambda1, lambda2, type){
  F1 = 0
  F2 = 0
  check = ifelse(spat_full_data$region == type,1,0)
  check_ind = which(check == 1)
  for(i in 1:length(check_ind)){
    info = EE1E2_s_diffpenalty(check_ind[i],df,lambda1, lambda2)
    F1 = F1 + solve(info$Es)%*%info$E2
    F2 = F2 + solve(info$Es)%*%info$E1
  }
  return(solve(F1)%*%F2)
}

trend_ss_estimate_ind_diffpenalty = function(s,df,lambda1, lambda2, FF1_reg){
  type = spat_full_data[s,4]
  info = EE1E2_s_diffpenalty(s,df,lambda1, lambda2)
  L = diag(c(rep(lambda1,df),rep(lambda2,(nrow(info$Es)-df))))
  coeff = solve(info$Es)%*% (info$E1 +  L %*% FF1_reg)
  beta_trend = coeff[1:df]
  beta_ss = coeff[(df+1):length(coeff)]
  trend = as.numeric(as.matrix(trend_basis %*% as.matrix(beta_trend)))
  ss = as.numeric(as.matrix(seasonality_basis_1 %*% as.matrix(beta_ss)))
  ss_df = matrix(ss, ncol = 12, byrow = T)
  seasonality_period = as.data.frame(t(ss_df[!duplicated(ss_df),]))
  t0 = info$t0
  random = t0$rain - trend - ss
  pred = trend + ss
  return(list(trend = trend, seasonality_period = seasonality_period, predicted = pred, error = random))
}


### test train split
EE1E2_s_diffpenalty_train_test = function(s,df,lambda1, lambda2,ratio = 0.9){
  t_s = trend_ss_matrix_train_test(s,df,ratio)
  E2_s = t_s$trend_ss_mat
  L = diag(c(rep(lambda1,df),rep(lambda2,(nrow(E2_s)-df))))
  E_s = E2_s + L
  rain_s = t_s$rain
  rain_s[is.na(rain_s)] = 0
  E1_s = t(t_s$basis)%*%as.matrix(rain_s,1)
  return(list(E1 = E1_s, Es = E_s, E2 = E2_s, test = t_s$test))
}

trend_ss_estimate_ind_diffpenalty_train_test = function(s,df,lambda1,lambda2,FF1_reg,ratio = 0.9){
  type = spat_full_data[s,4]
  info = EE1E2_s_diffpenalty_train_test(s,df,lambda1, lambda2, ratio)
  L = diag(c(rep(lambda1,df),rep(lambda2,(nrow(info$Es)-df))))
  coeff = solve(info$Es)%*% (info$E1 + L %*% FF1_reg)
  beta_trend = coeff[1:df]
  beta_ss = coeff[(df+1):length(coeff)]
  trend = as.numeric(as.matrix(trend_basis %*% as.matrix(beta_trend)))
  ss = as.numeric(as.matrix(seasonality_basis_1 %*% as.matrix(beta_ss)))
  ss_df = matrix(ss, ncol = 12, byrow = T)
  seasonality_period = as.data.frame(t(ss_df[!duplicated(ss_df),]))
  t0 = info$t0
  random = t0$rain - trend - ss
  pred = trend + ss
  return(list(trend = trend, seasonality_period = seasonality_period, predicted = pred, error = random, test = info$test))
}

### reflecting boundary
EE1E2_s_diffpenalty_ext = function(s,df,lambda1,lambda2){
  t_s = trend_ss_matrix_ext(s,df)
  E2_s = t_s$trend_ss_mat
  L = diag(c(rep(lambda1,df),rep(lambda2,(nrow(E2_s)-df))))
  E_s = E2_s + L
  rain_s = t_s$rain
  rain_s[is.na(rain_s)] = 0
  E1_s = t(t_s$basis)%*%as.matrix(rain_s,1)
  return(list(E1 = E1_s, Es = E_s, E2 = E2_s, t0 = t_s))
}

FF1_diffpenalty_ext = function(df,lambda1, lambda2, type){
  F1 = 0
  F2 = 0
  check = ifelse(spat_full_data$region == type,1,0)
  check_ind = which(check == 1)
  for(i in 1:length(check_ind)){
    info = EE1E2_s_diffpenalty_ext(check_ind[i],df,lambda1, lambda2)
    F1 = F1 + solve(info$Es)%*%info$E2
    F2 = F2 + solve(info$Es)%*%info$E1
  }
  return(solve(F1)%*%F2)
}

trend_ss_estimate_ind_diffpenalty_ext = function(s,df,lambda1, lambda2, FF1_reg){
  type = spat_full_data[s,4]
  info = EE1E2_s_diffpenalty_ext(s,df,lambda1, lambda2)
  L = diag(c(rep(lambda1,df),rep(lambda2,(nrow(info$Es)-df))))
  coeff = solve(info$Es)%*% (info$E1 +  L %*% FF1_reg)
  beta_trend = coeff[1:df]
  beta_ss = coeff[(df+1):length(coeff)]
  trend = as.numeric(as.matrix(trend_basis_ext %*% as.matrix(beta_trend)))
  ss = as.numeric(as.matrix(seasonality_basis_1 %*% as.matrix(beta_ss)))
  ss_df = matrix(ss, ncol = 12, byrow = T)
  seasonality_period = as.data.frame(t(ss_df[!duplicated(ss_df),]))
  t0 = info$t0
  random = t0$rain - trend - ss
  pred = trend + ss
  return(list(trend = trend, seasonality_period = seasonality_period, predicted = pred, error = random))
}


### reflecting boundary train test
EE1E2_s_diffpenalty_train_test_ext = function(s,df,lambda1, lambda2,ratio = 0.9){
  t_s = trend_ss_matrix_train_test_ext(s,df,ratio)
  E2_s = t_s$trend_ss_mat
  L = diag(c(rep(lambda1,df),rep(lambda2,(nrow(E2_s)-df))))
  E_s = E2_s + L
  rain_s = t_s$rain
  rain_s[is.na(rain_s)] = 0
  E1_s = t(t_s$basis)%*%as.matrix(rain_s,1)
  return(list(E1 = E1_s, Es = E_s, E2 = E2_s, test = t_s$test))
}

trend_ss_estimate_ind_diffpenalty_train_test_ext = function(s,df,lambda1,lambda2,FF1_reg,ratio = 0.9){
  type = spat_full_data[s,4]
  info = EE1E2_s_diffpenalty_train_test_ext(s,df,lambda1, lambda2, ratio)
  L = diag(c(rep(lambda1,df),rep(lambda2,(nrow(info$Es)-df))))
  coeff = solve(info$Es)%*% (info$E1 + L %*% FF1_reg)
  beta_trend = coeff[1:df]
  beta_ss = coeff[(df+1):length(coeff)]
  trend = as.numeric(as.matrix(trend_basis_ext %*% as.matrix(beta_trend)))
  ss = as.numeric(as.matrix(seasonality_basis_1 %*% as.matrix(beta_ss)))
  ss_df = matrix(ss, ncol = 12, byrow = T)
  seasonality_period = as.data.frame(t(ss_df[!duplicated(ss_df),]))
  t0 = info$t0
  random = t0$rain - trend - ss
  pred = trend + ss
  return(list(trend = trend, seasonality_period = seasonality_period, predicted = pred, error = random, test = info$test))
}



### choosing lambda1, lambda2
lambda_seq = c(0.01,0.05,0.1,0.5,1,5,10,20)

L       <- length(lambda_seq)
nStn    <- nrow(spat_full_data)
types   <- c("S","N","W")
FF1_cache <- list()
ratio = 0.9
for (t in types) {
  for (j in seq_along(lambda_seq)) {
    for (k in seq_along(lambda_seq)) {
      key <- paste(t, j, k, sep = "_") 
      FF1_cache[[key]] <- FF1_diffpenalty(8, lambda_seq[j], lambda_seq[k], t)
    }
  }
}
R = 10
mse_arr <- array(NA_real_, dim = c(nStn, L, L))
for (i in seq_len(nStn)) {
  st_type <- spat_full_data[i, 4]
  
  for (j in seq_along(lambda_seq)) {
    for (k in seq_along(lambda_seq)) {
      
      key <- paste(st_type, j, k, sep = "_")
      FF1_mat <- FF1_cache[[key]] 
      errs <- numeric(R)
      for (r in seq_len(R)) {
        pd <- trend_ss_estimate_ind_diffpenalty_train_test(i, 8,
                                                           lambda_seq[j],
                                                           lambda_seq[k],
                                                           FF1_mat,ratio)
        df <- na.omit(data.frame(pred = pd$predicted, act = pd$test))
        errs[r] <- mean((df$pred - df$act)^2)
      }
      
      mse_arr[i, j, k] <- mean(errs)
    }
  }
}

saveRDS(mse_arr, "/Users/srijanch/Downloads/mse_arr.Rds")

FF1_cache_ext <- list()
ratio = 0.9
for (t in types) {
  for (j in seq_along(lambda_seq)) {
    for (k in seq_along(lambda_seq)) {
      key <- paste(t, j, k, sep = "_") 
      FF1_cache_ext[[key]] <- FF1_diffpenalty_ext(8, lambda_seq[j], lambda_seq[k], t)
    }
  }
}

R = 10
mse_arr_ext <- array(NA_real_, dim = c(nStn, L, L))
for (i in seq_len(nStn)) {
  st_type <- spat_full_data[i, 4]
  
  for (j in seq_along(lambda_seq)) {
    for (k in seq_along(lambda_seq)) {
      
      key <- paste(st_type, j, k, sep = "_")
      FF1_mat <- FF1_cache_ext[[key]] 
      errs <- numeric(R)
      for (r in seq_len(R)) {
        pd <- trend_ss_estimate_ind_diffpenalty_train_test_ext(i, 8,
                                                               lambda_seq[j],
                                                               lambda_seq[k],
                                                               FF1_mat,ratio)
        df <- na.omit(data.frame(pred = pd$predicted, act = pd$test))
        errs[r] <- mean((df$pred - df$act)^2)
      }
      
      mse_arr_ext[i, j, k] <- mean(errs)
    }
  }
}

saveRDS(mse_arr_ext, "/Users/srijanch/Downloads/mse_arr_ext.Rds")


pdf("~/Downloads/Trend_SS_Pred_for_Smooth_TS.pdf", width = 18, height = 10)
for(i in seq_len(nStn)){
  if (is.na(sum(mse_arr[i,,]))==FALSE && !inherits(try(trend_ss_estimate_smoothing(i, 8, mse_h_ts[i], 9), silent=TRUE), "try-error") && is.na(mse_h_ts[i]) == FALSE ) {
    st_type <- as.character(spat_full_data[i, 4])
    mat <- mse_arr[i,,]
    vec <- as.vector(mat)
    vmin <- min(vec)
    q <- quantile(vec, 0.1)
    which_idx <- which(mat <= q, arr.ind = TRUE)
    var_pred = numeric()
    for(j in 1:nrow(which_idx)){
      key <- paste(st_type, which_idx[j,1], which_idx[j,2], sep = "_")
      FF1_mat <- FF1_cache[[key]]
      ts = trend_ss_estimate_ind_diffpenalty_train_test(i,8,lambda_seq[which_idx[j,1]],lambda_seq[which_idx[j,2]],FF1_mat)
      var_pred[j] = var(ts$predicted)
    }
    j_min = which.min(var_pred)
    key <- paste(st_type, which_idx[j_min,1], which_idx[j_min,2], sep = "_")
    FF1_mat <- FF1_cache[[key]]
    penalty_result_diffpen = trend_ss_estimate_ind_diffpenalty(i,8,lambda_seq[which_idx[j_min,1]],lambda_seq[which_idx[j_min,2]],FF1_mat)
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
    sd1_TS = round(sd(penalty_seasonality_diffpen$V1, na.rm = TRUE), 2)
    sd2_TS = round(sd(penalty_seasonality_diffpen$V2, na.rm = TRUE), 2)
    sd3_TS = round(sd(penalty_seasonality_diffpen$V3, na.rm = TRUE), 2)
    sd4_TS = round(sd(penalty_seasonality_diffpen$V4, na.rm = TRUE), 2)
    sd1_smooth = round(sd(smooth_seasonality$V1,na.rm = TRUE),2)
    sd2_smooth = round(sd(smooth_seasonality$V2,na.rm = TRUE),2)
    sd3_smooth = round(sd(smooth_seasonality$V3,na.rm = TRUE),2)
    sd4_smooth = round(sd(smooth_seasonality$V4,na.rm = TRUE),2)
    p1 = ggplot(penalty_seasonality_diffpen) +
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
        )
        ,
        color = "Time Period"
      ) +
      scale_color_manual(values = c(
        "1901-1930" = "#1f78b4",  # blue
        "1931-1960" = "#33a02c",  # green
        "1961-1990" = "#e31a1c",  # red
        "1991-2020" = "#ff7f00"   # orange
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
    p2 = smooth_seasonality %>%
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
        )
        ,
        color = "Time Period"
      ) +
      scale_color_manual(values = c(
        "1901-1930" = "#1f78b4",  # blue
        "1931-1960" = "#33a02c",  # green
        "1961-1990" = "#e31a1c",  # red
        "1991-2020" = "#ff7f00"   # orange
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
    p3 <- 
      ggplot(plot_data, aes(x = x)) +
      geom_line(aes(y = Actual, color = "Actual"), linewidth = 1.3, alpha = 0.95) +
      geom_line(aes(y = TrendSSPenalty, color = "Predicted"), linewidth = 1, alpha = 0.6) +
      labs(
        title = paste("TS Prediction vs Actual for", spat_full_data[i, 3]),
        subtitle = paste("RMSE:", round(rmse_TS, 2), "| MAE:", round(mae_TS, 2)),
        x = "Index",
        y = "Value",
        color = "Method"
      ) +
      scale_color_manual(values = c("Actual" = "#003f5c",   # navy blue
                                    "Predicted" = "#ffa600")) + # golden orange
      theme_minimal(base_size = 14) +
      theme(
        plot.title   = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle= element_text(size = 14, hjust = 0.5, color = "gray30"),
        axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y  = element_text(size = 12),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text  = element_text(size = 16),
        legend.position = "bottom",
        legend.box = "horizontal"
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
      scale_color_manual(values = c("Actual" = "#003f5c",                # navy blue
                                    "Predicted" = "#ffa600")) +  # golden orange
      theme_minimal(base_size = 14) +
      theme(
        plot.title   = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle= element_text(size = 14, hjust = 0.5, color = "gray30"),
        axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y  = element_text(size = 12),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text  = element_text(size = 16),
        legend.position = "bottom",
        legend.box = "horizontal"
      )
    rmse_TS_smooth = sqrt(mean((plot_data$TrendSSPenalty_trend-plot_data$smooth_trend)^2,na.rm=T))
    mae_TS_smooth = mean(abs(plot_data$TrendSSPenalty_trend-plot_data$smooth_trend),na.rm = T)
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
      scale_color_manual(values = c("Smoothing" = "#4A90E2",  # teal
                                    "TS"        = "#ffa600")) +  # coral red
      theme_minimal()+
      theme(
        plot.title   = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle= element_text(size = 14, hjust = 0.5, color = "gray30"),
        axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y  = element_text(size = 12),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text  = element_text(size = 16),
        legend.position = "bottom",
        legend.box = "horizontal"
      )
    print(p3)
    print(p4)
    print(p5)
    print(p1)
    print(p2)
  }
}
dev.off()





pdf("~/Downloads/Trend_SS_Pred_for_Smooth_TS_reflecting_boundary.pdf", width = 18, height = 10)
for(i in seq_len(nStn)){
  if (is.na(sum(mse_arr_ext[i,,]))==FALSE && !inherits(try(trend_ss_estimate_smoothing_ext(i, 8, mse_h_ts[i], 9), silent=TRUE), "try-error") && is.na(mse_h_ts[i]) == FALSE ) {
    st_type <- as.character(spat_full_data[i, 4])
    mat <- mse_arr_ext[i,,]
    vec <- as.vector(mat)
    vmin <- min(vec)
    q <- quantile(vec, 0.1)
    which_idx <- which(mat <= q, arr.ind = TRUE)
    var_pred = numeric()
    for(j in 1:nrow(which_idx)){
      key <- paste(st_type, which_idx[j,1], which_idx[j,2], sep = "_")
      FF1_mat <- FF1_cache_ext[[key]]
      ts = trend_ss_estimate_ind_diffpenalty_train_test_ext(i,8,lambda_seq[which_idx[j,1]],lambda_seq[which_idx[j,2]],FF1_mat)
      var_pred[j] = var(ts$predicted)
    }
    j_min = which.min(var_pred)
    key <- paste(st_type, which_idx[j_min,1], which_idx[j_min,2], sep = "_")
    FF1_mat <- FF1_cache_ext[[key]]
    penalty_result_diffpen = trend_ss_estimate_ind_diffpenalty_ext(i,8,lambda_seq[which_idx[j_min,1]],lambda_seq[which_idx[j_min,2]],FF1_mat)
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
    sd1_TS = round(sd(penalty_seasonality_diffpen$V1, na.rm = TRUE), 2)
    sd2_TS = round(sd(penalty_seasonality_diffpen$V2, na.rm = TRUE), 2)
    sd3_TS = round(sd(penalty_seasonality_diffpen$V3, na.rm = TRUE), 2)
    sd4_TS = round(sd(penalty_seasonality_diffpen$V4, na.rm = TRUE), 2)
    sd1_smooth = round(sd(smooth_seasonality$V1,na.rm = TRUE),2)
    sd2_smooth = round(sd(smooth_seasonality$V2,na.rm = TRUE),2)
    sd3_smooth = round(sd(smooth_seasonality$V3,na.rm = TRUE),2)
    sd4_smooth = round(sd(smooth_seasonality$V4,na.rm = TRUE),2)
    p1 = ggplot(penalty_seasonality_diffpen) +
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
        )
        ,
        color = "Time Period"
      ) +
      scale_color_manual(values = c(
        "1901-1930" = "#1f78b4",  # blue
        "1931-1960" = "#33a02c",  # green
        "1961-1990" = "#e31a1c",  # red
        "1991-2020" = "#ff7f00"   # orange
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
    p2 = smooth_seasonality %>%
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
        )
        ,
        color = "Time Period"
      ) +
      scale_color_manual(values = c(
        "1901-1930" = "#1f78b4",  # blue
        "1931-1960" = "#33a02c",  # green
        "1961-1990" = "#e31a1c",  # red
        "1991-2020" = "#ff7f00"   # orange
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
    p3 <- 
      ggplot(plot_data, aes(x = x)) +
      geom_line(aes(y = Actual, color = "Actual"), linewidth = 1.3, alpha = 0.95) +
      geom_line(aes(y = TrendSSPenalty, color = "Predicted"), linewidth = 1, alpha = 0.6) +
      geom_vline(xintercept = c(120, 1561), linetype = "dashed", color = "violet",linewidth = 1.2) +
      labs(
        title = paste("TS Prediction vs Actual for", spat_full_data[i, 3]),
        subtitle = paste("RMSE:", round(rmse_TS, 2), "| MAE:", round(mae_TS, 2)),
        x = "Index",
        y = "Value",
        color = "Method"
      ) +
      scale_color_manual(values = c("Actual" = "#003f5c",   # navy blue
                                    "Predicted" = "#ffa600")) + # golden orange
      theme_minimal(base_size = 14) +
      theme(
        plot.title   = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle= element_text(size = 14, hjust = 0.5, color = "gray30"),
        axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y  = element_text(size = 12),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text  = element_text(size = 16),
        legend.position = "bottom",
        legend.box = "horizontal"
      )
    p4 <- ggplot(plot_data, aes(x = x)) +
      geom_line(aes(y = Actual, color = "Actual"), linewidth = 1.3, alpha = 0.95) +
      geom_line(aes(y = smooth_pred, color = "Predicted"), linewidth = 1, alpha = 0.6) + 
      geom_vline(xintercept = c(120, 1561), linetype = "dashed", color = "violet",linewidth = 1.2) +
      labs(
        title = paste("Smoothed Prediction vs Actual for", spat_full_data[i, 3]),
        subtitle = paste("RMSE:", round(rmse_smooth, 2), "| MAE:", round(mae_smooth, 2)),
        x = "Index",
        y = "Value",
        color = "Method"
      ) +
      scale_color_manual(values = c("Actual" = "#003f5c",                # navy blue
                                    "Predicted" = "#ffa600")) +  # golden orange
      theme_minimal(base_size = 14) +
      theme(
        plot.title   = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle= element_text(size = 14, hjust = 0.5, color = "gray30"),
        axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y  = element_text(size = 12),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text  = element_text(size = 16),
        legend.position = "bottom",
        legend.box = "horizontal"
      )
    rmse_TS_smooth = sqrt(mean((plot_data$TrendSSPenalty_trend-plot_data$smooth_trend)^2,na.rm=T))
    mae_TS_smooth = mean(abs(plot_data$TrendSSPenalty_trend-plot_data$smooth_trend),na.rm = T)
    p5 <- ggplot(plot_data, aes(x = x)) +
      geom_line(aes(y = TrendSSPenalty_trend, color = "TS"), linewidth = 1) +
      geom_line(aes(y = smooth_trend, color = "Smoothing"), linewidth = 1) + 
      geom_vline(xintercept = c(120, 1561), linetype = "dashed", color = "violet",linewidth = 1.2) +
      labs(
        title = paste("Trend Estimates for ", spat_full_data[i, 3]),
        subtitle = paste("RMSE:", round(rmse_TS_smooth, 2), "| MAE:", round(mae_TS_smooth, 2)),
        x = "Index",
        y = "Value",
        color = "Method"
      ) +
      scale_color_manual(values = c("Smoothing" = "#4A90E2",  # teal
                                    "TS"        = "#ffa600")) +  # coral red
      theme_minimal()+
      theme(
        plot.title   = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle= element_text(size = 14, hjust = 0.5, color = "gray30"),
        axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y  = element_text(size = 12),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text  = element_text(size = 16),
        legend.position = "bottom",
        legend.box = "horizontal"
      )
    print(p3)
    print(p4)
    print(p5)
    print(p1)
    print(p2)
  }
}
dev.off()


