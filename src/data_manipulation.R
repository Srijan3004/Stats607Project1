# Reading the actual data
data = read.csv("original/Rainfall.csv")
station = sort(unique(data$STATION)) # sorting the stations name wise

data1 = data[,6:372] #just taking the numeric rainfall part!

# Taking lattitude, lomgitude, station name, district, rainfall
full_data = read.csv("original/full_data_wb.csv")[,-1]

full_data = full_data[-which(full_data$YEAR == "2021"),] # 2021 has very less entries, hence removing that

# Disambiguate duplicate STATION names by appending a unique numeric suffix 
# if they have multiple distinct coordinate pairs (LAT/LON).
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

# some stations have missing info for lat and lon, imputing those using information from internet
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

# Some stations have repeated entries for same time, clearing those by taking average observations
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




