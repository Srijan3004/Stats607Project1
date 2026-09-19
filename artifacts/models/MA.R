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

