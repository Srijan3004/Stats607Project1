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
