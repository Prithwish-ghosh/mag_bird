data = read.csv("bird tracking data.csv")
head(data)
dim(data)
summary(data)
tail(data)
data = na.omit(data)

c_l = function(longitude){
  return((longitude + 180) %% 360 - 180)
}

B_data = data

dt <- data.frame(lon = data$longitude,
                 lat = data$latitude)
library(ggOceanMaps)
basemap(data = dt, bathymetry = TRUE, grid.col = "transparent", land.col = "green4") +
  #ggspatial::geom_spatial_point(data = dt, aes(x = lon, y = lat),
   #                             color = "red")+
  geom_point(
    data = B_data,
    aes(B_data$longitude, B_data$latitude, color = B_data$bird_name),
    alpha = 1, size = 1
  ) +    
  scale_color_manual(values = 
                       c("Eric" = "red",
                         "Nico" = "yellow", 
                         "Sanne" = "pink3")) +
  theme(legend.position="left")#+ scale_color_brewer(palette = "Set3")

B_data = B_data[B_data$altitude >0, ]

basemap(data = dt, bathymetry = TRUE, grid.col = "transparent", land.col = "green4") +
  geom_segment(data = dataff,
               aes(x = longitude,
                   y = latitude,
                   xend = longitude + sin(theta * pi/180),
                   yend = latitude + cos(theta * pi/180)),
               arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "last"),
               color = "red")  +
  theme(legend.position = "bottom")

dim(B_data)

library(Directional)
ed = cbind(B_data$latitude, B_data$longitude)

ed


# nOW WE USE 16000 SAMPLES TO PLOT




vmf_density_grid = function(u, ngrid = 100){
  u[,1] <- u[,1] + 90
  u[,2] <- u[,2] +180
  res <- vmf.kerncontour(u, thumb = "none", den.ret = T, full = T,
                         ngrid = ngrid)
  ret <- expand.grid(Lat = res$lat - 90, Long = res$long - 180)
  ret$Density <- c(res$den)
  ret
}

ed1 = ed[sample(nrow(ed), 14000),] 
data1 = B_data[sample(nrow(B_data), 14000),] 

bird.dens = vmf_density_grid(ed1, ngrid = 300)

summary(data)


library(oceanmap)
library(ggOceanMaps)

$latitude = as.numeric(d1$latitude)

d1$longitude = as.numeric(d1$longitude)

dt1 = data.frame(B_data$longitude, B_data$latitude)

library(ggplot2)
library(ggspatial)

basemap(data = dt1, bathymetry = T, land.col = "green4", grid.size = 30, grid.col = "transparent") +
  geom_point(
    data = B_data,
    aes(B_data$longitude, B_data$latitude, color = B_data$bird_name),
    alpha = 1, size = 1
  ) +    
  scale_color_manual(values = 
                       c("Eric" = "red",
                         "Nico" = "yellow", 
                         "Sanne" = "magenta1"))+
  geom_density_2d(data = data1,
                  aes(x = longitude, y = latitude),
                  color = "turquoise2", alpha = 2, size = 1) +
  geom_contour(data = bird.dens, aes(x = Long, y = Lat, z = Density),
               color = "darkorange1", size = 1.5) +   theme(legend.position = "bottom") +
  theme(legend.position="left")
#  coord_map("mercator")


library(openair)
library(wmm)

head(d1)

d1=data.frame(B_data)

d11=d1[1:121,]
d12=d1[122:167,]
d13=d1[168:243,]
d14=d1[244:375,]
d15=d1[376:536,]
d16=d1[537:703,]
d15
d11$dates = as.Date(d11$date, tryFormats = "%d-%m-%Y")
d11$dates
d11$year <- format(d11$dates, "%Y")
d11$year = as.numeric(d11$year)


# Convert the date string to a POSIXlt object
date_obj <- as.POSIXlt(d11$dates)
#date_obj

# Extract the day of the year
d11$yearly_day <- date_obj$yday + 1
#summary(data)
#data$date = as.Date(data$date)
#d16$time = as.Date(d16$date)
#d16$time

d11$yearly_date = d11$yearly_day/365
#d16$yearly_date
d11$yearly_day

d11$Annualized_time = d11$year + d11$yearly_date
d11$height = 0

dim(d11)


summary(d15)
tail(d11)


data_f = rbind(d11, d12, d13, d14, d15, d16)
head(data_f)
dim(data_f)


mag1 = list()
for (i in 1:703) {
  mag1[[i]] = data.frame(GetMagneticFieldWMM(lon = data_f$longitude[i], 
                                             lat =  data_f$latitude[i], 
                                             height =  data_f$height[i], 
                                             time =  data_f$Annualized_time[i], 
                                             wmmVersion = "WMM2015v2"))
}

mage_data = data.frame(t(sapply(mag1,c)))
head(mage_data)
magnetic_dta = mage_data[,c(1:3, 7:10)]
head(magnetic_dta)

combined_f_data = cbind(data_f, magnetic_dta)
head(combined_f_data)

get_initial_bearing <- function(longA, latA, longB, latB) {
  delta_long <- longB - longA
  rad_latA <- latA * (pi / 180)  
  rad_latB <- latB * (pi / 180)  
  
  bearing <- atan2(sin(delta_long) * cos(rad_latB), cos(rad_latA) * sin(rad_latB) - sin(rad_latA) * cos(rad_latB) * cos(delta_long))
  bearing_degrees <- bearing * (180 / pi)  
  
  return(bearing_degrees)
}

summary(combined_f_data)
combined_f_data$i = as.numeric(combined_f_data$i)
combined_f_data$d = as.numeric(combined_f_data$d)

calculate_julian_day <- function(year, month, day) {
  jd <- (1461 * (year + 4800 + (month - 14) / 12)) / 4 +
    (367 * (month - 2 - 12 * ((month - 14) / 12))) / 12 -
    (3 * ((year + 4900 + (month - 14) / 12) / 100)) / 4 + day - 32075
  return(jd)
}

date_str <- combined_f_data$date
date_str
# Convert the date string to a Date object using dmy() function (day-month-year format)
library(lubridate)
date <- dmy(date_str)

# Extract year, month, and day
combined_f_data$year <- year(date)
combined_f_data$month <- month(date)
combined_f_data$day <- day(date)

# Apply the function to calculate Julian Day for each row in the data frame
combined_f_data$julian_day <- calculate_julian_day(combined_f_data$year, combined_f_data$month, combined_f_data$day)


library(solarPos)
x = solarPos::solarPosition(combined_f_data$julian_day , combined_f_data$longitude , combined_f_data$latitude)
combined_f_new_data = data.frame(combined_f_data , x)
head(combined_f_new_data)

library(suncalc)
dataframe = data.frame(date = combined_f_new_data$date , lat = combined_f_new_data$latitude , lon = combined_f_new_data$longitude)
df = suncalc::getMoonPosition(data = dataframe , keep = c("azimuth"))
head(df)

combined_f_new_data$moon_azimuth = df$azimuth
head(combined_f_new_data)



df11=combined_f_new_data[1:121,]
df12=combined_f_new_data[122:167,]
df13=combined_f_new_data[168:243,]
df14=combined_f_new_data[244:375,]
df15=combined_f_new_data[376:536,]
df16=combined_f_new_data[537:703,]

dim(df16)
summary(combined_f_data)


combined_f_data$longitude = c_l(combined_f_data$longitude)

for ( i in 1:121) {
  df11$theta[i] = 
    get_initial_bearing(df11$longitude[i] ,
                        df11$latitude[i],
                        df11$longitude[i+1],
                        df11$latitude[i+1] )
}

for ( i in 1:46) {
  df12$theta[i] = 
    get_initial_bearing(df12$longitude[i] ,
                        df12$latitude[i],
                        df12$longitude[i+1],
                        df12$latitude[i+1] )
}

for ( i in 1:76) {
  df13$theta[i] = 
    get_initial_bearing(df13$longitude[i] ,
                        df13$latitude[i],
                        df13$longitude[i+1],
                        df13$latitude[i+1] )
}

for ( i in 1:132) {
  df14$theta[i] = 
    get_initial_bearing(df14$longitude[i] ,
                        df14$latitude[i],
                        df14$longitude[i+1],
                        df14$latitude[i+1] )
}

for ( i in 1:161) {
  df15$theta[i] = 
    get_initial_bearing(df15$longitude[i] ,
                        df15$latitude[i],
                        df15$longitude[i+1],
                        df15$latitude[i+1] )
}

for ( i in 1:167) {
  df16$theta[i] = 
    get_initial_bearing(df16$longitude[i] ,
                        df16$latitude[i],
                        df16$longitude[i+1],
                        df16$latitude[i+1] )
}

for ( i in 1:121) {
  df11$del_i[i] = 
    (df11$i[i+1] -  
       df11$i[i])
}

for ( i in 1:46) {
  df12$del_i[i] = 
    (df12$i[i+1] -  
       df12$i[i])
}


for ( i in 1:76) {
  df13$del_i[i] = 
    (df13$i[i+1] -  
       df13$i[i])
}

for ( i in 1:132) {
  df14$del_i[i] = 
    (df14$i[i+1] -  
       df14$i[i])
}

for ( i in 1:161) {
  df15$del_i[i] = 
    (df15$i[i+1] -  
       df15$i[i])
}

for ( i in 1:167) {
  df16$del_i[i] = 
    (df16$i[i+1] -  
       df16$i[i])
}

for ( i in 1:121) {
  df11$del_d[i] = 
    (df11$d[i+1] -  
       df11$d[i])
}

for ( i in 1:46) {
  df12$del_d[i] = 
    (df12$d[i+1] -  
       df12$d[i])
}


for ( i in 1:76) {
  df13$del_d[i] = 
    (df13$d[i+1] -  
       df13$d[i])
}

for ( i in 1:132) {
  df14$del_d[i] = 
    (df14$d[i+1] -  
       df14$d[i])
}

for ( i in 1:161) {
  df15$del_d[i] = 
    (df15$d[i+1] -  
       df15$d[i])
}

for ( i in 1:167) {
  df16$del_d[i] = 
    (df16$d[i+1] -  
       df16$d[i])
}

tail(df13)

library(CircStats)

for ( i in 1:121) {
  df11$del_sol_az[i] = 
    (df11$azimuth[i+1] -  
       df11$azimuth[i])
}

for ( i in 1:121) {
  df11$del_sol_zen[i] = 
    (df11$zenith[i+1] -  
       df11$zenith[i])
}

for ( i in 1:121) {
  df11$del_lunar_az[i] = 
    (df11$moon_azimuth[i+1] -  
       df11$moon_azimuth[i])
}

head(df11)

for ( i in 1:46) {
  df12$del_sol_az[i] = 
    (df12$azimuth[i+1] -  
       df12$azimuth[i])
}

for ( i in 1:46) {
  df12$del_sol_zen[i] = 
    (df12$zenith[i+1] -  
       df12$zenith[i])
}

for ( i in 1:46) {
  df12$del_lunar_az[i] = 
    (df12$moon_azimuth[i+1] -  
       df12$moon_azimuth[i])
}

for ( i in 1:76) {
  df13$del_sol_az[i] = 
    (df13$azimuth[i+1] -  
       df13$azimuth[i])
}

for ( i in 1:76) {
  df13$del_sol_zen[i] = 
    (df13$zenith[i+1] -  
       df13$zenith[i])
}

for ( i in 1:76) {
  df13$del_lunar_az[i] = 
    (df13$moon_azimuth[i+1] -  
       df13$moon_azimuth[i])
}

head(df13)

for ( i in 1:132) {
  df14$del_sol_az[i] = 
    (df14$azimuth[i+1] -  
       df14$azimuth[i])
}

for ( i in 1:132) {
  df14$del_sol_zen[i] = 
    (df14$zenith[i+1] -  
       df14$zenith[i])
}

for ( i in 1:132) {
  df14$del_lunar_az[i] = 
    (df14$moon_azimuth[i+1] -  
       df14$moon_azimuth[i])
}

head(df14)

for ( i in 1:161) {
  df15$del_sol_az[i] = 
    (df15$azimuth[i+1] -  
       df15$azimuth[i])
}

for ( i in 1:161) {
  df15$del_sol_zen[i] = 
    (df15$zenith[i+1] -  
       df15$zenith[i])
}

for ( i in 1:161) {
  df15$del_lunar_az[i] = 
    (df15$moon_azimuth[i+1] -  
       df15$moon_azimuth[i])
}

head(df15)

for ( i in 1:167) {
  df16$del_sol_az[i] = 
    (df16$azimuth[i+1] -  
       df16$azimuth[i])
}

for ( i in 1:167) {
  df16$del_sol_zen[i] = 
    (df16$zenith[i+1] -  
       df16$zenith[i])
}

for ( i in 1:167) {
  df16$del_lunar_az[i] = 
    (df16$moon_azimuth[i+1] -  
       df16$moon_azimuth[i])
}

tail(df12)

bird_final = rbind(df11, df12, df13, df14, df15, df16)

head(bird_final)
dim(bird_final)
bird_final$x = as.numeric(bird_final$x)
bird_final$y = as.numeric(bird_final$y)
bird_final$z = as.numeric(bird_final$z)
bird_final$h = as.numeric(bird_final$h)
bird_final$f = as.numeric(bird_final$f)
bird_final$i = as.numeric(bird_final$i)

write.csv(bird_final, file = "bird_migration_final.csv", col.names = FALSE)

dim(combined_f_new_data)

head(combined_f_new_data)
library(circular)

watson.test(df16$longitude, alpha = 0.01, dist = "vonmises")
watson.test(df16$latitude, alpha = 0.01, dist = "vonmises")
watson.test(df16$theta, alpha = 0.01, dist = "vonmises")
watson.test(df16$del_lunar_az, alpha = 0.01, dist = "vonmises")

df16_part1 = df16[c(1:75),]
watson.test(df16_part1$longitude, alpha = 0.01, dist = "vonmises")
watson.test(df16_part1$latitude, alpha = 0.01, dist = "vonmises")
watson.test(df16_part1$theta, alpha = 0.01, dist = "vonmises")
watson.test(df16_part1$del_lunar_az, alpha = 0.01, dist = "vonmises")
watson.test(df16_part1$del_sol_az, alpha =   0.01, dist = "vonmises")
watson.test(df16_part1$del_sol_zen, alpha =   0.01, dist = "vonmises")
watson.test(df16_part1$del_d, alpha =   0.01, dist = "vonmises")

f1 = lm.circular(df16_part22$theta, df16_part22$del_d)
f1$rho
df16_part22 = na.omit(df16_part22)
circ.cor(df16_part22$theta, df16_part22$del_d, T)
f2 = lm.circular(df16_part22$theta, df16_part22$del_sol_az)
f2$rho
circ.cor(df16_part22$theta, df16_part22$del_sol_az, T)

f3 = lm.circular(df16_part22$theta, df16_part22$del_sol_zen)
f3$rho
circ.cor(df16_part22$theta, df16_part22$del_sol_zen, T)

f4 = lm.circular(df16_part22$theta, df16_part22$del_lunar_az)
f4$rho
circ.cor(df16_part22$theta, df16_part22$del_lunar_az, T)

dim(df16_part1)
df16$species

df16_part22 = df16[c(100:167),]
watson.test(df16_part22$longitude, alpha = 0.01, dist = "vonmises")
watson.test(df16_part22$latitude, alpha = 0.01, dist = "vonmises")
watson.test(df16_part22$theta, alpha = 0.01, dist = "vonmises")
watson.test(df16_part22$del_lunar_az, alpha = 0.01, dist = "vonmises")
watson.test(df16_part22$del_sol_az, alpha =   0.01, dist = "vonmises")
watson.test(df16_part22$del_sol_zen, alpha =   0.01, dist = "vonmises")
watson.test(df16_part22$del_d, alpha =   0.01, dist = "vonmises")



df16_part23 = df16[c(88:99),]
watson.test(df16_part23$longitude, alpha = 0.01, dist = "vonmises")
watson.test(df16_part23$latitude, alpha = 0.01, dist = "vonmises")
watson.test(df16_part23$theta, alpha = 0.01, dist = "vonmises")
watson.test(df16_part23$del_lunar_az, alpha = 0.01, dist = "vonmises")
watson.test(df16_part23$del_sol_az, alpha =   0.01, dist = "vonmises")
watson.test(df16_part23$del_sol_zen, alpha =   0.01, dist = "vonmises")
watson.test(df16_part23$del_d, alpha =   0.01, dist = "vonmises")

df16_part3 = df16[c(76:78),]
watson.test(df16_part3$longitude, alpha = 0.01, dist = "vonmises")
watson.test(df16_part3$latitude, alpha = 0.01, dist = "vonmises")
watson.test(df16_part3$theta, alpha = 0.01, dist = "vonmises")
watson.test(df16_part3$del_lunar_az, alpha = 0.01, dist = "vonmises")
watson.test(df16_part3$del_sol_az, alpha =   0.01, dist = "vonmises")
watson.test(df16_part3$del_sol_zen, alpha =   0.01, dist = "vonmises")
watson.test(df16_part3$del_d, alpha =   0.01, dist = "vonmises")


df16_part31 = df16[c(79:82),]
watson.test(df16_part31$longitude, alpha = 0.01, dist = "vonmises")
watson.test(df16_part31$latitude, alpha = 0.01, dist = "vonmises")
watson.test(df16_part31$theta, alpha = 0.01, dist = "vonmises")
watson.test(df16_part31$del_lunar_az, alpha = 0.01, dist = "vonmises")
watson.test(df16_part31$del_sol_az, alpha =   0.01, dist = "vonmises")
watson.test(df16_part31$del_sol_zen, alpha =   0.01, dist = "vonmises")
watson.test(df16_part31$del_d, alpha =   0.01, dist = "vonmises")

df16_part34 = df16[c(83:85),]
watson.test(df16_part34$longitude, alpha = 0.01, dist = "vonmises")
watson.test(df16_part34$latitude, alpha = 0.01, dist = "vonmises")
watson.test(df16_part34$theta, alpha = 0.01, dist = "vonmises")
watson.test(df16_part34$del_lunar_az, alpha = 0.01, dist = "vonmises")
watson.test(df16_part34$del_sol_az, alpha =   0.01, dist = "vonmises")
watson.test(df16_part34$del_sol_zen, alpha =   0.01, dist = "vonmises")
watson.test(df16_part34$del_d, alpha =   0.01, dist = "vonmises")


dim(df12)

df12_part1 = df12[c(2:11),]

watson.test(df12_part1$longitude, alpha = 0.01, dist = "vonmises")
watson.test(df12_part1$latitude, alpha = 0.01, dist = "vonmises")
watson.test(df12_part1$theta, alpha = 0.01, dist = "vonmises")
watson.test(df12_part1$del_lunar_az, alpha = 0.01, dist = "vonmises")
watson.test(df12_part1$del_sol_az, alpha =   0.01, dist = "vonmises")
watson.test(df12_part1$del_sol_zen, alpha =   0.01, dist = "vonmises")
watson.test(df12_part1$del_d, alpha =   0.01, dist = "vonmises")


df12_part2 = df12[c(12:15),]

watson.test(df12_part2$longitude, alpha = 0.01, dist = "vonmises")
watson.test(df12_part2$latitude, alpha = 0.01, dist = "vonmises")
watson.test(df12_part2$theta, alpha = 0.01, dist = "vonmises")
watson.test(df12_part2$del_lunar_az, alpha = 0.01, dist = "vonmises")
watson.test(df12_part2$del_sol_az, alpha =   0.01, dist = "vonmises")
watson.test(df12_part2$del_sol_zen, alpha =   0.01, dist = "vonmises")
watson.test(df12_part2$del_d, alpha =   0.01, dist = "vonmises")

df12_part3 = df12[c(39:46),]

watson.test(df12_part3$longitude, alpha = 0.01, dist = "vonmises")
watson.test(df12_part3$latitude, alpha = 0.01, dist = "vonmises")
watson.test(df12_part3$theta, alpha = 0.01, dist = "vonmises")
watson.test(df12_part3$del_lunar_az, alpha = 0.01, dist = "vonmises")
watson.test(df12_part3$del_sol_az, alpha =   0.01, dist = "vonmises")
watson.test(df12_part3$del_sol_zen, alpha =   0.01, dist = "vonmises")
watson.test(df12_part3$del_d, alpha =   0.01, dist = "vonmises")



library(movMF)

library(Directional)
ed = cbind(d16$latitude, d16$longitude)

Evmf <- function(K){
  movMF(ed, k= K, control = list(nruns = 20))
}

set.seed(123)
Esd = lapply(1:10, Evmf)

Esd
sapply(Esd, BIC)


combined_f_new_data = na.omit(combined_f_new_data)

d1 = combined_f_new_data

d11=d1[1:121,]
d12=d1[122:167,]
d13=d1[168:243,]
d14=d1[244:375,]
d15=d1[376:536,]
d16=d1[537:701,]



dim(combined_f_new_data)

dp1 = d1[,c(20:21,28:30)]
head(dp1)

dp11=dp1[1:121,]
dp12=dp1[122:167,]
dp13=dp1[168:243,]
dp14=dp1[244:375,]
dp15=dp1[376:536,]
dp16=dp1[537:701,]
dp112 = sin(dp12)

library(FactoMineR)
pca_para = PCA(dp112, graph = T)
eig_val = pca_para$eig
eig_val

contri = pca_para$var$contrib
contri

library(circular)
library(Directional)
library(kernlab)


xxxx = kpca(dp11)
xxxx
pcv(xxxx)

watson.test(d11$theta, alpha = 0.01, dist = "vonmises")
watson.test(d12$theta, alpha = 0.01, dist = "vonmises")
watson.test(d13$theta, alpha = 0.01, dist = "vonmises")
watson.test(d14$theta, alpha = 0.01, dist = "vonmises")
watson.test(d15$theta, alpha = 0.01, dist = "vonmises")
watson.test(d16$theta, alpha = 0.01, dist = "vonmises")



circ.cor(combined_f_data$theta , combined_f_data$del_i, T)

head(combined_f_data$species)


library(circular)

f1 = lm.circular(combined_f_data$theta, combined_f_data$del_i)
f2 = lm.circular(combined_f_data$theta, combined_f_data$del_d)


f1$rho
f2$rho
head(d16)



GetMagneticFieldWMM(
  lon = 240,
  lat = -80,
  height = 1e5,
  time = 2005,
  wmmVersion = 'WMM2000'
)



################ European ###############################
library(readr)
data = read_csv("bird_tracking.csv")
data = data.frame(data)
data

dt <- data.frame(lon = data$longitude, lat = data$latitude)

dt

library(ggOceanMaps)
library(geosphere)
library(ggplot2)
library(oceanmap)
library(RColorBrewer)


?RColorBrewer::brewer.pal()
brewer.pal.info


basemap(data = dt, bathymetry = TRUE, grid.col = "transparent", land.col = "green4") +
  geom_point(
    data = data,
    aes(data$longitude, data$latitude, color = data$bird_name),
    alpha = 2, size = 1
  ) + scale_color_brewer(palette = "Set11") + theme_minimal()


#+ scale_color_manual(values = 
c("Eric" = "red",
  "Nicco" = "yellow"
  "Sanne" = "#FF00FF") +
  theme(legend.position="top")#+ scale_color_brewer(palette = "Set3")
















get_initial_bearing <- function(longA, latA, longB, latB) {
  delta_long <- longB - longA
  rad_latA <- latA * (pi / 180)  # Convert latA to radians
  rad_latB <- latB * (pi / 180)  # Convert latB to radians
  
  bearing <- atan2(sin(delta_long) * cos(rad_latB), cos(rad_latA) * sin(rad_latB) - sin(rad_latA) * cos(rad_latB) * cos(delta_long))
  bearing_degrees <- bearing * (180 / pi)  # Convert bearing to degrees
  
  return(bearing_degrees)
}

new_data = data

Black_bellied_plover = new_data[1:121,]
Black_crowned_night_heron = new_data[122:167,]
Brown_pelican = new_data[168:243,]
Long_billed_curlew = new_data[244:375,]
Pacific_loon = new_data[376:536,]
Swainsons_hawk = new_data[537:703,]

Black_bellied_plover$bird = 1
Black_crowned_night_heron$bird = 2
Brown_pelican$bird = 3
Long_billed_curlew$bird = 4
Pacific_loon$bird = 5
Swainsons_hawk$bird = 6


for (
  i in 1:46) {
  #Black_crowned_night_heron$s[i] = Black_crowned_night_heron$G_h[i+1] - Black_crowned_night_heron$G_h[i] 
  Black_crowned_night_heron$theta[i] = get_initial_bearing(Black_crowned_night_heron$longitude[i] , 
                                                           Black_crowned_night_heron$latitude[i],
                                                           Black_crowned_night_heron$longitude[i+1], 
                                                           Black_crowned_night_heron$latitude[i+1] )
}
tail(Black_crowned_night_heron)
p1 = na.omit(Black_crowned_night_heron) 
p1$s = as.numeric(p1$s)
fit_BCNH = lm.circular(p1$theta , p1$s, init = 0.0 , type = "c-l" )
fit_BCNH$p.values
fit_BCNH
watson.test(Black_crowned_night_heron$theta , alpha = 0.05 , dist = "vonmises")
summary(fit_BCNH)


for (i in 1:121) {
 # Black_bellied_plover$s[i] = Black_bellied_plover$G_h[i+1] - Black_bellied_plover$G_h[i] 
  Black_bellied_plover$theta[i] = get_initial_bearing(Black_bellied_plover$longitude[i] , 
                                                      Black_bellied_plover$latitude[i],
                                                      Black_bellied_plover$longitude[i+1], 
                                                      Black_bellied_plover$latitude[i+1] )
}
tail(Black_bellied_plover)
p2 = na.omit(Black_bellied_plover)
p2$s = as.numeric(p2$s)
fit_BBP = lm.circular(p2$theta , p2$s, init = 0.0, type = "c-l" )
fit_BBP$p.values
fit_BBP
watson.test(Black_bellied_plover$theta , alpha = 0.05 , dist = "vonmises")



for (i in 1:76) {
  #Brown_pelican$s[i] = Brown_pelican$G_h[i+1] - Brown_pelican$G_h[i] 
  Brown_pelican$theta[i] = get_initial_bearing(Brown_pelican$longitude[i] , 
                                               Brown_pelican$latitude[i],
                                               Brown_pelican$longitude[i+1], 
                                               Brown_pelican$latitude[i+1] )
}
tail(Brown_pelican)
watson.test(Brown_pelican$theta , alpha = 0.05 , dist = "vonmises")
Brown_pelican$theta = as.numeric(Brown_pelican$theta)
Brown_pelican$s = as.numeric(Brown_pelican$s)
Brown_pelican = na.omit(Brown_pelican)
fit_BP = lm.circular(Brown_pelican$theta , Brown_pelican$s , init = 0.0 , type = "c-l")
fit_BP$p.values
fit_BP
for (i in 1:132) {
  #Long_billed_curlew$s[i] = Long_billed_curlew$G_h[i+1] - Long_billed_curlew$G_h[i] 
  Long_billed_curlew$theta[i] = get_initial_bearing(Long_billed_curlew$longitude[i] , 
                                                    Long_billed_curlew$latitude[i],
                                                    Long_billed_curlew$longitude[i+1], 
                                                    Long_billed_curlew$latitude[i+1] )
}
tail(Long_billed_curlew)
watson.test(Long_billed_curlew$theta , alpha = 0.05 , dist = "vonmises")
Long_billed_curlew$s = as.numeric(Long_billed_curlew$s)
fit_LBC = lm.circular(Long_billed_curlew$theta , Long_billed_curlew$s , init = 0.0 , type = "c-l")
fit_LBC

for (i in 1:161) {
  #Pacific_loon$s[i] = Pacific_loon$G_h[i+1] - Pacific_loon$G_h[i] 
  Pacific_loon$theta[i] = get_initial_bearing(Pacific_loon$longitude[i] , 
                                              Pacific_loon$latitude[i],
                                              Pacific_loon$longitude[i+1], 
                                              Pacific_loon$latitude[i+1] )
}
tail(Pacific_loon)
watson.test(Pacific_loon$theta , alpha = 0.05 , dist = "vonmises")
Pacific_loon$s = as.numeric(Pacific_loon$s)
fit_PL = lm.circular(Pacific_loon$theta , Pacific_loon$s , init = 0.0 , type = "c-l")
fit_PL


for (i in 1:167) {
 # Swainsons_hawk$s[i] = Swainsons_hawk$G_h[i+1] - Swainsons_hawk$G_h[i] 
  Swainsons_hawk$theta[i] = get_initial_bearing(Swainsons_hawk$longitude[i] , 
                                                Swainsons_hawk$latitude[i],
                                                Swainsons_hawk$longitude[i+1], 
                                                Swainsons_hawk$latitude[i+1] )
}
sum(is.na(Swainsons_hawk$s))
tail(Swainsons_hawk)
watson.test(Swainsons_hawk$theta , alpha = 0.05 , dist = "vonmises")
x = c(Swainsons_hawk$s)
Swainsons_hawk = na.omit(Swainsons_hawk)
Swainsons_hawk$s = as.numeric(Swainsons_hawk$s)
sum(is.na(Swainsons_hawk$s))
fit_Sw = lm.circular(Swainsons_hawk$theta , x , init = 0.0 , type = "c-l")
fit_Sw$p.values
fit_Sw
bird_partition = dff$bird
colours =c("blue","red","green","brown","yellow", "pink")

dataf = rbind(Black_bellied_plover
              , Black_crowned_night_heron, Swainsons_hawk, Pacific_loon, Brown_pelican)

head(dataf)
dt = data.frame(dataff$longitude, dataff$latitude)

tail(dataf)

dataff = na.omit(dataf)


library(ggplot2)


summary(dataff)



Black_bellied_plover$longitude = c_l(Black_bellied_plover$longitude)
Black_crowned_night_heron$longitude = c_l(Black_crowned_night_heron$longitude)
Pacific_loon$longitude = c_l(Pacific_loon$longitude)
Brown_pelican$longitude = c_l(Brown_pelican$longitude)
Swainsons_hawk$longitude = c_l(Swainsons_hawk$longitude`)
Long_billed_curlew$longitude = c_l(Long_billed_curlew$longitude)


basemap(data = dt, bathymetry = TRUE, grid.col = "transparent", land.col = "#D5D5D5") +
  geom_segment(data = Pacific_loon,
               aes(x = longitude, y = latitude, xend = longitude + sin(theta * pi/180),
                   yend = latitude + cos(theta * pi/180),
                   color = "brown3"),
               arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "last"),size = 0.9) +
  geom_segment(data = Brown_pelican,
               aes(x = longitude, y = latitude, xend = longitude + sin(theta * pi/180),
                   yend = latitude + cos(theta * pi/180),
                   color = "orange"),
               arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "last"),size = 0.9) +
  geom_segment(data = Black_bellied_plover,
               aes(x = longitude, y = latitude, xend = longitude + sin(theta * pi/180),
                   yend = latitude + cos(theta * pi/180),
                   color = "purple"),
               arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "last"),size = 0.9) +
  geom_segment(data = Black_crowned_night_heron,
               aes(x = longitude, y = latitude, xend = longitude + sin(theta * pi/180),
                   yend = latitude + cos(theta * pi/180),
                   color = "pink3"),
               arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "last"),size = 0.9) +
  geom_segment(data = Long_billed_curlew,
               aes(x = longitude, y = latitude, xend = longitude + sin(theta * pi/180),
                   yend = latitude + cos(theta * pi/180),
                   color = "green2"),
               arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "last"),size = 0.9) +
  geom_segment(data = Swainsons_hawk,
               aes(x = longitude, y = latitude, xend = longitude + sin(theta * pi/180),
                   yend = latitude + cos(theta * pi/180),
                   color = "red"),
               arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "last"),size = 0.9) +
  theme(legend.position = "bottom")





#scale_color_manual(values = 
                     c("Eric" = "red",
                       "Nicco" = "yellow"
                       "Sanne" = "#FF00FF") +
                     theme(legend.position="top")#+ scale_color_brewer(palette = "Set3")
                   
                   geom_point(
                     data = data,
                     aes(data$longitude, data$latitude, color = data$bird_name),
                     alpha = 2, size = 2
                   ) +    
                     scale_color_manual(values = 
                                          c("Eric" = "red",
                                            "Nicco" = "yellow"
                                            "Sanne" = "#FF00FF") +
                                          theme(legend.position="top")#+ scale_color_brewer(palette = "Set3")
                                        
                                        
