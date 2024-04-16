library(vroom)
library(dplyr)

points_mass_b <-vroom("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/A_SUBMISSION_UPDATED/FINAL SUBMISSION/Supplmentary Datafiles for Publication/Data_S1_Bird_Mass.csv")%>%
  select(Binomial, Lat, Lon)
points_length_b <- vroom("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/A_SUBMISSION_UPDATED/FINAL SUBMISSION/Supplmentary Datafiles for Publication/Data_S2_Bird_Length.csv")%>%
  select(Binomial, Lat, Lon)
points_size_b <- vroom("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/A_SUBMISSION_UPDATED/FINAL SUBMISSION/Supplmentary Datafiles for Publication/Data_S3_Bird_Size.csv")%>%
  select(Binomial, Lat, Lon)

points_mass_m <-vroom("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/A_SUBMISSION_UPDATED/FINAL SUBMISSION/Supplmentary Datafiles for Publication/Data_S4_Mammal_Mass.csv")%>%
  select(Binomial, Lat, Lon)
points_length_m <- vroom("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/A_SUBMISSION_UPDATED/FINAL SUBMISSION/Supplmentary Datafiles for Publication/Data_S5_Mammal_Length.csv")%>%
  select(Binomial, Lat, Lon)
points_size_m <- vroom("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/A_SUBMISSION_UPDATED/FINAL SUBMISSION/Supplmentary Datafiles for Publication/Data_S6_Mammal_Size.csv")%>%
  select(Binomial, Lat, Lon)

trends <- vroom("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/A_SUBMISSION_UPDATED/FINAL SUBMISSION/Supplmentary Datafiles for Publication/Data_S9_Body_Size_Time_Trend_Data.csv")

mass_t_m <- trends %>%filter(Metric=="Mass" & Class=="Mammalia")
length_t_m <- trends %>%filter(Metric=="Length" & Class=="Mammalia")
size_t_m  <- trends %>%filter(Metric=="Size" & Class=="Mammalia")

mass_t_b <- trends %>%filter(Metric=="Mass" & Class=="Aves")
length_t_b <- trends %>%filter(Metric=="Length" & Class=="Aves")
size_t_b  <- trends %>%filter(Metric=="Size" & Class=="Aves")

points_mass_b <- left_join(points_mass_b, mass_t_b)
points_length_b <- left_join(points_length_b, length_t_b)
points_size_b <- left_join(points_size_b, size_t_b)

points_mass_m <- left_join(points_mass_m, mass_t_m)
points_length_m <- left_join(points_length_m, length_t_m)
points_size_m <- left_join(points_size_m, size_t_m)

vroom_write(points_mass_b,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_mass_trend.csv")
vroom_write(points_length_b,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_length_trend.csv")
vroom_write(points_size_b,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_size_trend.csv")

vroom_write(points_mass_m,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_mass_trend.csv")
vroom_write(points_length_m,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_length_trend.csv")
vroom_write(points_size_m,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_size_trend.csv")


##test
library(terra)
tmax_rast <- rast("E:/Coding Files/Climate Data/Temperature Files/Tmax/Baseline/tmax_1961_01.tif")
base_rast <- tmax_rast*0
base_rast100 <- aggregate(base_rast,fact=20)


b_m <- points_mass_b
b_l <- points_length_b
b_s <- points_size_b


b_m <- b_m %>% rename(x=Lon,y=Lat, z=lm_estimate)%>% dplyr::select(x,y,z)
xy_df <- as.matrix(b_m%>%dplyr::select(x,y))
z <-b_m$z
b_mr <- rasterize(xy_df, base_rast100, values=z, fun = mean, touches=T)
plot(b_mr)
crs(b_mr) <- crs(tmax_rast)

b_l <- b_l %>% rename(x=Lon,y=Lat, z=lm_estimate)%>% dplyr::select(x,y,z)
xy_dfl <- as.matrix(b_l%>%dplyr::select(x,y))
zl <-b_l$z
b_lr <- rasterize(xy_dfl, base_rast100, values=zl, fun = mean, touches=T)
plot(b_lr)

b_s <- b_s %>% rename(x=Lon,y=Lat, z=lm_estimate)%>% dplyr::select(x,y,z)
xy_dfs <- as.matrix(b_s%>%dplyr::select(x,y))
zs <-b_s$z
b_sr <- rasterize(xy_dfs, base_rast100, values=zs, fun = mean, touches=T)
plot(b_sr)

writeRaster(b_mr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_mass.tif",overwrite=T)
writeRaster(b_lr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_length.tif.tif",overwrite=T)
writeRaster(b_sr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_size.tif.tif",overwrite=T)



m_m <- points_mass_m
m_l <- points_length_m
m_s <- points_size_m

m_m <- m_m %>% rename(x=Lon,y=Lat, z=lm_estimate)%>% dplyr::select(x,y,z)
xy_dfm <- as.matrix(m_m%>%dplyr::select(x,y))
zm <-m_m$z
m_mr <- rasterize(xy_dfm, base_rast100, values=zm, fun = mean, touches=T)
plot(m_mr)

m_l <- m_l %>% rename(x=Lon,y=Lat, z=lm_estimate)%>% dplyr::select(x,y,z)
xy_dfml <- as.matrix(m_l%>%dplyr::select(x,y))
zml <-m_l$z
m_lr <- rasterize(xy_dfml, base_rast100, values=zml, fun = mean, touches=T)
plot(m_lr)

m_s <- m_s %>% rename(x=Lon,y=Lat, z=lm_estimate)%>% dplyr::select(x,y,z)
xy_dfms <- as.matrix(m_s%>%dplyr::select(x,y))
zms <-m_s$z
m_sr <- rasterize(xy_dfms, base_rast100, values=zms, fun = mean, touches=T)
plot(m_sr)

writeRaster(m_mr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_mass.tif",overwrite=T)
writeRaster(m_lr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_length.tif.tif",overwrite=T)
writeRaster(m_sr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_size.tif.tif",overwrite=T)



#Positives


b_m <- points_mass_b %>% filter(lm_estimate >= 0)
b_l <- points_length_b%>% filter(lm_estimate >= 0)
b_s <- points_size_b%>% filter(lm_estimate >= 0)


b_m <- b_m %>% rename(x=Lon,y=Lat, z=lm_estimate)%>% dplyr::select(x,y,z)
xy_df <- as.matrix(b_m%>%dplyr::select(x,y))
z <-b_m$z
b_mr <- rasterize(xy_df, base_rast100, values=z, fun = mean, touches=T)
plot(b_mr)
crs(b_mr) <- crs(tmax_rast)

b_l <- b_l %>% rename(x=Lon,y=Lat, z=lm_estimate)%>% dplyr::select(x,y,z)
xy_dfl <- as.matrix(b_l%>%dplyr::select(x,y))
zl <-b_l$z
b_lr <- rasterize(xy_dfl, base_rast100, values=zl, fun = mean, touches=T)
plot(b_lr)

b_s <- b_s %>% rename(x=Lon,y=Lat, z=lm_estimate)%>% dplyr::select(x,y,z)
xy_dfs <- as.matrix(b_s%>%dplyr::select(x,y))
zs <-b_s$z
b_sr <- rasterize(xy_dfs, base_rast100, values=zs, fun = mean, touches=T)
plot(b_sr)

writeRaster(b_mr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_mass_positive.tif",overwrite=T)
writeRaster(b_lr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_length_positive.tif",overwrite=T)
writeRaster(b_sr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_size_positive.tif",overwrite=T)



m_m <- points_mass_m%>% filter(lm_estimate >= 0)
m_l <- points_length_m%>% filter(lm_estimate >= 0)
m_s <- points_size_m%>% filter(lm_estimate >= 0)

m_m <- m_m %>% rename(x=Lon,y=Lat, z=lm_estimate)%>% dplyr::select(x,y,z)
xy_dfm <- as.matrix(m_m%>%dplyr::select(x,y))
zm <-m_m$z
m_mr <- rasterize(xy_dfm, base_rast100, values=zm, fun = mean, touches=T)
plot(m_mr)

m_l <- m_l %>% rename(x=Lon,y=Lat, z=lm_estimate)%>% dplyr::select(x,y,z)
xy_dfml <- as.matrix(m_l%>%dplyr::select(x,y))
zml <-m_l$z
m_lr <- rasterize(xy_dfml, base_rast100, values=zml, fun = mean, touches=T)
plot(m_lr)

m_s <- m_s %>% rename(x=Lon,y=Lat, z=lm_estimate)%>% dplyr::select(x,y,z)
xy_dfms <- as.matrix(m_s%>%dplyr::select(x,y))
zms <-m_s$z
m_sr <- rasterize(xy_dfms, base_rast100, values=zms, fun = mean, touches=T)
plot(m_sr)

writeRaster(m_mr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_mass_positive.tif",overwrite=T)
writeRaster(m_lr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_length_positive.tif",overwrite=T)
writeRaster(m_sr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_size_positive.tif",overwrite=T)

#Positives


b_m <- points_mass_b %>% filter(lm_estimate <= 0)
b_l <- points_length_b%>% filter(lm_estimate <= 0)
b_s <- points_size_b%>% filter(lm_estimate <= 0)


b_m <- b_m %>% rename(x=Lon,y=Lat, z=lm_estimate)%>% dplyr::select(x,y,z)
xy_df <- as.matrix(b_m%>%dplyr::select(x,y))
z <-b_m$z
b_mr <- rasterize(xy_df, base_rast100, values=z, fun = mean, touches=T)
plot(b_mr)
crs(b_mr) <- crs(tmax_rast)

b_l <- b_l %>% rename(x=Lon,y=Lat, z=lm_estimate)%>% dplyr::select(x,y,z)
xy_dfl <- as.matrix(b_l%>%dplyr::select(x,y))
zl <-b_l$z
b_lr <- rasterize(xy_dfl, base_rast100, values=zl, fun = mean, touches=T)
plot(b_lr)

b_s <- b_s %>% rename(x=Lon,y=Lat, z=lm_estimate)%>% dplyr::select(x,y,z)
xy_dfs <- as.matrix(b_s%>%dplyr::select(x,y))
zs <-b_s$z
b_sr <- rasterize(xy_dfs, base_rast100, values=zs, fun = mean, touches=T)
plot(b_sr)

writeRaster(b_mr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_mass_negative.tif",overwrite=T)
writeRaster(b_lr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_length_negative.tif",overwrite=T)
writeRaster(b_sr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_size_negative.tif",overwrite=T)



m_m <- points_mass_m%>% filter(lm_estimate <= 0)
m_l <- points_length_m%>% filter(lm_estimate <= 0)
m_s <- points_size_m%>% filter(lm_estimate <= 0)

m_m <- m_m %>% rename(x=Lon,y=Lat, z=lm_estimate)%>% dplyr::select(x,y,z)
xy_dfm <- as.matrix(m_m%>%dplyr::select(x,y))
zm <-m_m$z
m_mr <- rasterize(xy_dfm, base_rast100, values=zm, fun = mean, touches=T)
plot(m_mr)

m_l <- m_l %>% rename(x=Lon,y=Lat, z=lm_estimate)%>% dplyr::select(x,y,z)
xy_dfml <- as.matrix(m_l%>%dplyr::select(x,y))
zml <-m_l$z
m_lr <- rasterize(xy_dfml, base_rast100, values=zml, fun = mean, touches=T)
plot(m_lr)

m_s <- m_s %>% rename(x=Lon,y=Lat, z=lm_estimate)%>% dplyr::select(x,y,z)
xy_dfms <- as.matrix(m_s%>%dplyr::select(x,y))
zms <-m_s$z
m_sr <- rasterize(xy_dfms, base_rast100, values=zms, fun = mean, touches=T)
plot(m_sr)

writeRaster(m_mr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_mass_negative.tif",overwrite=T)
writeRaster(m_lr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_length_negative.tif",overwrite=T)
writeRaster(m_sr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_size_negative.tif",overwrite=T)


##summs

tmax_rast <- rast("E:/Coding Files/Climate Data/Temperature Files/Tmax/Baseline/tmax_1961_01.tif")
base_rast <- tmax_rast*0
base_rast1000 <- aggregate(base_rast,fact=50)

b_m <- points_mass_b %>% mutate(lm_estimate = ifelse(lm_estimate<0, -1,ifelse(lm_estimate==0,0,1)),
                                TPI_trend = ifelse(TPI_trend<0, -1,ifelse(TPI_trend==0,0,1)),
                                AI_trend = ifelse(AI_trend<0, -1,ifelse(AI_trend==0,0,1)),
                                HLU_trend = ifelse(HLU_trend<0, -1,ifelse(HLU_trend==0,0,1)))
b_l <- points_length_b %>% mutate(lm_estimate = ifelse(lm_estimate<0, -1,ifelse(lm_estimate==0,0,1)),
                                  TPI_trend = ifelse(TPI_trend<0, -1,ifelse(TPI_trend==0,0,1)),
                                  AI_trend = ifelse(AI_trend<0, -1,ifelse(AI_trend==0,0,1)),
                                  HLU_trend = ifelse(HLU_trend<0, -1,ifelse(HLU_trend==0,0,1)))
b_s <- points_size_b %>% mutate(lm_estimate = ifelse(lm_estimate<0, -1,ifelse(lm_estimate==0,0,1)),
                                TPI_trend = ifelse(TPI_trend<0, -1,ifelse(TPI_trend==0,0,1)),
                                AI_trend = ifelse(AI_trend<0, -1,ifelse(AI_trend==0,0,1)),
                                HLU_trend = ifelse(HLU_trend<0, -1,ifelse(HLU_trend==0,0,1)))


b_m <- b_m %>% rename(x=Lon, y=Lat, z=lm_estimate,zz=TPI_trend)
xy_df <- as.matrix(b_m%>%dplyr::select(x,y))
z <-b_m$z
zz <- b_m$zz
zzz <- b_m$AI_trend
zzzz <- b_m$HLU_trend
b_mr <- rasterize(xy_df, base_rast1000, values=z, fun = mean, touches=T)
b_mrt <- rasterize(xy_df, base_rast1000, values=zz, fun = mean, touches=T)
b_mra <- rasterize(xy_df, base_rast1000, values=zzz, fun = mean, touches=T)
b_mrh <- rasterize(xy_df, base_rast1000, values=zzzz, fun = mean, touches=T)

b_l <- b_l %>% rename(x=Lon,y=Lat, z=lm_estimate,zz=TPI_trend)
xy_dfl <- as.matrix(b_l%>%dplyr::select(x,y))
zl <-b_l$z
zlz <- b_l$zz
zlza <- b_l$AI_trend
zlzh <- b_l$HLU_trend

b_lr <- rasterize(xy_dfl, base_rast1000, values=zl, fun = mean, touches=T)
b_lrt <- rasterize(xy_dfl, base_rast1000, values=zlz, fun = mean, touches=T)
b_lra <- rasterize(xy_dfl, base_rast1000, values=zlza, fun = mean, touches=T)
b_lrh <- rasterize(xy_dfl, base_rast1000, values=zlzh, fun = mean, touches=T)

b_s <- b_s %>% rename(x=Lon,y=Lat, z=lm_estimate,zz=TPI_trend)
xy_dfs <- as.matrix(b_s%>%dplyr::select(x,y))
zs <-b_s$z
zsz <- b_s$zz
zsza <- b_s$AI_trend
zszh <- b_s$AI_trend

b_sr <- rasterize(xy_dfs, base_rast1000, values=zs, fun = mean, touches=T)
b_srt <- rasterize(xy_dfs, base_rast1000, values=zsz, fun = mean, touches=T)
b_sra <- rasterize(xy_dfs, base_rast1000, values=zsza, fun = mean, touches=T)
b_srh <- rasterize(xy_dfs, base_rast1000, values=zszh, fun = mean, touches=T)
plot(b_mr)

writeRaster(b_mr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_mass_sum.tif",overwrite=T)
writeRaster(b_lr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_length_sum.tif.tif",overwrite=T)
writeRaster(b_sr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_size_sum.tif.tif",overwrite=T)
writeRaster(b_mrt,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_mass_tpi.tif",overwrite=T)
writeRaster(b_lrt,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_length_tpi.tif.tif",overwrite=T)
writeRaster(b_srt,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_size_tpi.tif.tif",overwrite=T)
writeRaster(b_mra,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_mass_ai.tif",overwrite=T)
writeRaster(b_lra,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_length_ai.tif",overwrite=T)
writeRaster(b_sra,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_size_ai.tif",overwrite=T)
writeRaster(b_mrh,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_mass_hlu.tif",overwrite=T)
writeRaster(b_lrh,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_length_hlu.tif",overwrite=T)
writeRaster(b_srh,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/bird_size_hlu.tif",overwrite=T)

m_m <- points_mass_m%>% mutate(lm_estimate = ifelse(lm_estimate<0, -1,ifelse(lm_estimate==0,0,1)),
                               TPI_trend = ifelse(TPI_trend<0, -1,ifelse(TPI_trend==0,0,1)),
                               AI_trend = ifelse(AI_trend<0, -1,ifelse(AI_trend==0,0,1)),
                               HLU_trend = ifelse(HLU_trend<0, -1,ifelse(HLU_trend==0,0,1)))
m_l <- points_length_m%>% mutate(lm_estimate = ifelse(lm_estimate<0, -1,ifelse(lm_estimate==0,0,1)),
                                 TPI_trend = ifelse(TPI_trend<0, -1,ifelse(TPI_trend==0,0,1)),
                                 AI_trend = ifelse(AI_trend<0, -1,ifelse(AI_trend==0,0,1)),
                                 HLU_trend = ifelse(HLU_trend<0, -1,ifelse(HLU_trend==0,0,1)))
m_s <- points_size_m%>% mutate(lm_estimate = ifelse(lm_estimate<0, -1,ifelse(lm_estimate==0,0,1)),
                               TPI_trend = ifelse(TPI_trend<0, -1,ifelse(TPI_trend==0,0,1)),
                               AI_trend = ifelse(AI_trend<0, -1,ifelse(AI_trend==0,0,1)),
                               HLU_trend = ifelse(HLU_trend<0, -1,ifelse(HLU_trend==0,0,1)))
                               
m_m <- m_m %>% rename(x=Lon,y=Lat, z=lm_estimate,zz = TPI_trend)
xy_dfm <- as.matrix(m_m%>%dplyr::select(x,y))
zm <-m_m$z
zmw <-m_m$zz
zma <-m_m$AI_trend
zmh <-m_m$HLU_trend

m_mr <- rasterize(xy_dfm, base_rast1000, values=zm, fun = mean, touches=T)
m_mrt <- rasterize(xy_dfm, base_rast1000, values=zmw, fun = mean, touches=T)
m_mra <- rasterize(xy_dfm, base_rast1000, values=zma, fun = mean, touches=T)
m_mrh <- rasterize(xy_dfm, base_rast1000, values=zmh, fun = mean, touches=T)

m_l <- m_l %>% rename(x=Lon,y=Lat, z=lm_estimate,zz=TPI_trend)
xy_dfml <- as.matrix(m_l%>%dplyr::select(x,y))
zml <-m_l$z
zmlz<-m_l$zz
zmla<-m_l$AI_trend
zmlh<-m_l$HLU_trend


m_lr <- rasterize(xy_dfml, base_rast1000, values=zml, fun = mean, touches=T)
m_lrz <- rasterize(xy_dfml, base_rast1000, values=zmlz, fun = mean, touches=T)
m_lra <- rasterize(xy_dfml, base_rast1000, values=zmla, fun = mean, touches=T)
m_lrh <- rasterize(xy_dfml, base_rast1000, values=zmlh, fun = mean, touches=T)


m_s <- m_s %>% rename(x=Lon,y=Lat, z=lm_estimate,zz=TPI_trend)
xy_dfms <- as.matrix(m_s%>%dplyr::select(x,y))
zms <-m_s$z
zmsz <- m_s$zz
zmsa <- m_s$AI_trend
zmsh <- m_s$HLU_trend

m_sr <- rasterize(xy_dfms, base_rast1000, values=zms, fun = mean, touches=T)
m_srz <- rasterize(xy_dfms, base_rast1000, values=zmsz, fun = mean, touches=T)
m_sra <- rasterize(xy_dfms, base_rast1000, values=zmsa, fun = mean, touches=T)
m_srh <- rasterize(xy_dfms, base_rast1000, values=zmsh, fun = mean, touches=T)

plot(m_sra)

writeRaster(m_mr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_mass_sum.tif",overwrite=T)
writeRaster(m_lr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_length_sum.tif",overwrite=T)
writeRaster(m_sr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_size_sum.tif",overwrite=T)

writeRaster(m_mrt,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_mass_tpi.tif",overwrite=T)
writeRaster(m_lrz,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_length_tpi.tif",overwrite=T)
writeRaster(m_srz,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_size_tpi.tif",overwrite=T)

writeRaster(m_mra,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_mass_ai.tif",overwrite=T)
writeRaster(m_lra,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_length_ai.tif",overwrite=T)
writeRaster(m_sra,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_size_ai.tif",overwrite=T)

writeRaster(m_mrh,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_mass_hlu.tif",overwrite=T)
writeRaster(m_lrh,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_length_hlu.tif",overwrite=T)
writeRaster(m_srh,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/mammal_size_hlu.tif",overwrite=T)


####values

b_m <- points_mass_b %>% rename(x=Lon, y=Lat, z=lm_estimate,zz=TPI_trend)
xy_df <- as.matrix(b_m%>%dplyr::select(x,y))
z <-b_m$z
zz <- b_m$zz
zzz <- b_m$AI_trend
zzzz <- b_m$HLU_trend
b_mr <- rasterize(xy_df, base_rast1000, values=z, fun = mean, touches=T)
b_mrt <- rasterize(xy_df, base_rast1000, values=zz, fun = mean, touches=T)
b_mra <- rasterize(xy_df, base_rast1000, values=zzz, fun = mean, touches=T)
b_mrh <- rasterize(xy_df, base_rast1000, values=zzzz, fun = mean, touches=T)

b_l <- points_length_b %>% rename(x=Lon,y=Lat, z=lm_estimate,zz=TPI_trend)
xy_dfl <- as.matrix(b_l%>%dplyr::select(x,y))
zl <-b_l$z
zlz <- b_l$zz
zlza <- b_l$AI_trend
zlzh <- b_l$HLU_trend

b_lr <- rasterize(xy_dfl, base_rast1000, values=zl, fun = mean, touches=T)
b_lrt <- rasterize(xy_dfl, base_rast1000, values=zlz, fun = mean, touches=T)
b_lra <- rasterize(xy_dfl, base_rast1000, values=zlza, fun = mean, touches=T)
b_lrh <- rasterize(xy_dfl, base_rast1000, values=zlzh, fun = mean, touches=T)

b_s <- points_size_b %>% rename(x=Lon,y=Lat, z=lm_estimate,zz=TPI_trend)
xy_dfs <- as.matrix(b_s%>%dplyr::select(x,y))
zs <-b_s$z
zsz <- b_s$zz
zsza <- b_s$AI_trend
zszh <- b_s$AI_trend

b_sr <- rasterize(xy_dfs, base_rast1000, values=zs, fun = mean, touches=T)
b_srt <- rasterize(xy_dfs, base_rast1000, values=zsz, fun = mean, touches=T)
b_sra <- rasterize(xy_dfs, base_rast1000, values=zsza, fun = mean, touches=T)
b_srh <- rasterize(xy_dfs, base_rast1000, values=zszh, fun = mean, touches=T)

writeRaster(b_mr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/bird_mass_sum_values.tif",overwrite=T)
writeRaster(b_lr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/bird_length_sum_values.tif.tif",overwrite=T)
writeRaster(b_sr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/bird_size_sum_values.tif.tif",overwrite=T)
writeRaster(b_mrt,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/bird_mass_tpi_values.tif",overwrite=T)
writeRaster(b_lrt,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/bird_length_tpi_values.tif.tif",overwrite=T)
writeRaster(b_srt,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/bird_size_tpi_values.tif.tif",overwrite=T)
writeRaster(b_mra,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/bird_mass_ai_values.tif",overwrite=T)
writeRaster(b_lra,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/bird_length_ai_values.tif",overwrite=T)
writeRaster(b_sra,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/bird_size_ai_values.tif",overwrite=T)
writeRaster(b_mrh,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/bird_mass_hlu_values.tif",overwrite=T)
writeRaster(b_lrh,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/bird_length_hlu_values.tif",overwrite=T)
writeRaster(b_srh,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/bird_size_hlu_values.tif",overwrite=T)


m_m <- points_size_m %>% rename(x=Lon,y=Lat, z=lm_estimate,zz = TPI_trend)
xy_dfm <- as.matrix(m_m%>%dplyr::select(x,y))
zm <-m_m$z
zmw <-m_m$zz
zma <-m_m$AI_trend
zmh <-m_m$HLU_trend

m_mr <- rasterize(xy_dfm, base_rast1000, values=zm, fun = mean, touches=T)
m_mrt <- rasterize(xy_dfm, base_rast1000, values=zmw, fun = mean, touches=T)
m_mra <- rasterize(xy_dfm, base_rast1000, values=zma, fun = mean, touches=T)
m_mrh <- rasterize(xy_dfm, base_rast1000, values=zmh, fun = mean, touches=T)

m_l <- points_length_m %>% rename(x=Lon,y=Lat, z=lm_estimate,zz=TPI_trend)
xy_dfml <- as.matrix(m_l%>%dplyr::select(x,y))
zml <-m_l$z
zmlz<-m_l$zz
zmla<-m_l$AI_trend
zmlh<-m_l$HLU_trend


m_lr <- rasterize(xy_dfml, base_rast1000, values=zml, fun = mean, touches=T)
m_lrz <- rasterize(xy_dfml, base_rast1000, values=zmlz, fun = mean, touches=T)
m_lra <- rasterize(xy_dfml, base_rast1000, values=zmla, fun = mean, touches=T)
m_lrh <- rasterize(xy_dfml, base_rast1000, values=zmlh, fun = mean, touches=T)


m_s <- points_mass_m %>% rename(x=Lon,y=Lat, z=lm_estimate,zz=TPI_trend)
xy_dfms <- as.matrix(m_s%>%dplyr::select(x,y))
zms <-m_s$z
zmsz <- m_s$zz
zmsa <- m_s$AI_trend
zmsh <- m_s$HLU_trend

m_sr <- rasterize(xy_dfms, base_rast1000, values=zms, fun = mean, touches=T)
m_srz <- rasterize(xy_dfms, base_rast1000, values=zmsz, fun = mean, touches=T)
m_sra <- rasterize(xy_dfms, base_rast1000, values=zmsa, fun = mean, touches=T)
m_srh <- rasterize(xy_dfms, base_rast1000, values=zmsh, fun = mean, touches=T)

plot(m_sra)

writeRaster(m_mr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/mammal_mass_sum_values.tif",overwrite=T)
writeRaster(m_lr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/mammal_length_sum_values.tif",overwrite=T)
writeRaster(m_sr,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/mammal_size_sum_values.tif",overwrite=T)

writeRaster(m_mrt,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/mammal_mass_tpi_values.tif",overwrite=T)
writeRaster(m_lrz,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/mammal_length_tpi_values.tif",overwrite=T)
writeRaster(m_srz,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/mammal_size_tpi_values.tif",overwrite=T)

writeRaster(m_mra,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/mammal_mass_ai_values.tif",overwrite=T)
writeRaster(m_lra,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/mammal_length_ai_values.tif",overwrite=T)
writeRaster(m_sra,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/mammal_size_ai_values.tif",overwrite=T)

writeRaster(m_mrh,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/mammal_mass_hlu_values.tif",overwrite=T)
writeRaster(m_lrh,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/mammal_length_hlu_values.tif",overwrite=T)
writeRaster(m_srh,"C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/values/mammal_size_hlu_values.tif",overwrite=T)
