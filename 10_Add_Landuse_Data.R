library(vroom)
library(dplyr)
library(terra)

mm <- vroom("D:/PhD/Thesis/Body Size Chapter/A_SUBMISSION_UPDATED/FINAL SUBMISSION/Nature CC submission/Extended Datafiles/Data_4_Mammal_Mass.csv")
ml <- vroom("D:/PhD/Thesis/Body Size Chapter/A_SUBMISSION_UPDATED/FINAL SUBMISSION/Nature CC submission/Extended Datafiles/Data_5_Mammal_Length.csv")
bm <- vroom("D:/PhD/Thesis/Body Size Chapter/A_SUBMISSION_UPDATED/FINAL SUBMISSION/Nature CC submission/Extended Datafiles/Data_1_Bird_Mass.csv")
bl <- vroom("D:/PhD/Thesis/Body Size Chapter/A_SUBMISSION_UPDATED/FINAL SUBMISSION/Nature CC submission/Extended Datafiles/Data_2_Bird_Length.csv")

xy <- data.frame(Lon = c(mm$Lon,ml$Lon,bm$Lon,bl$Lon),
                 Lat = c(mm$Lat,ml$Lat,bm$Lat,bl$Lat),
                 Year = c(mm$Year,ml$Year,bm$Year,bl$Year),
                 Month = c(mm$Month,ml$Month,bm$Month,bl$Month))%>%
  distinct()

xy_shape <-  vect(xy, geom=c("Lon", "Lat"), 
                  crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

dates <- data.frame(Year = c(mm$Year,ml$Year,bm$Year,bl$Year),
                    Month = c(mm$Month,ml$Month,bm$Month,bl$Month))%>%
  distinct()%>%mutate(Month_char = ifelse(Month>9,paste0(Month),paste0(0,Month)),
                   Month_let = ifelse(Month==1,"Jan",
                                      ifelse(Month==2,"Feb",
                                             ifelse(Month==3,"Mar",
                                                    ifelse(Month==4,"Apr",
                                                           ifelse(Month==5,"May",
                                                                  ifelse(Month==6,"June",
                                                                         ifelse(Month==7,"July",
                                                                                ifelse(Month==8,"Aug",
                                                                                       ifelse(Month==9,"Sep",
                                                                                              ifelse(Month==10,"Oct",
                                                                                                     ifelse(Month==11,"Nov","Dec"))))))))))))


library(progress)
pbsp <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                         total = nrow(dates),
                         complete = "=",   # Completion bar character
                         incomplete = "-", # Incomplete bar character
                         current = ">",    # Current bar character
                         clear = FALSE,    # If TRUE, clears the bar when finish
                         width = 100)

ALL_LAT_LONG_CLIMATE <- data.frame(NULL)

for(i in 1:nrow(dates)){
  pbsp$tick()
  
  HLU_Per <- rast(paste0("D:/HILDA/HLU Rasters/HLU_Percent_",dates$Year[i],".tif"))
  
  hc <- (dates$Year[i] - 1960)
  HLU_class <- rast("D:/HILDA/hildaplus_states.nc")[[(62+hc)]]
  
  hluvalue <- as.data.frame(as.data.frame(extract(HLU_Per, xy_shape))[,-1])
  colnames(hluvalue) <- "HLU"
  hluvalue$HLU_Class <- (as.data.frame(extract(HLU_class, xy_shape)))[,-1]
  hluvalue$Lat <- xy$Lat
  hluvalue$Lon <- xy$Lon
  
  hluvalue <- hluvalue %>% mutate(Year= dates$Year[i], Month = dates$Month[i])
  
  filter_lats <- xy%>%filter(Year==dates$Year[i] & Month==dates$Month[i])%>%
    dplyr::select(Lat,Lon)%>%mutate(Keep_val = "Yes")
  
  suppressWarnings(suppressMessages(hluvalue <- left_join(hluvalue,filter_lats)%>%filter(Keep_val=="Yes")%>%dplyr::select(-Keep_val)))
  
  ALL_LAT_LONG_CLIMATE <- rbind(ALL_LAT_LONG_CLIMATE,hluvalue)
  
}

vroom_write(ALL_LAT_LONG_CLIMATE, "D:/Thesis Projects/Body Size Reviewer Analyses and data/HLU_Lat_Lon_Dates.csv",delim=",")

ALL_LAT_LONG_CLIMATE2 <- ALL_LAT_LONG_CLIMATE %>% distinct()%>%rename(HLU_5 = HLU)

mm2 <- left_join(mm,ALL_LAT_LONG_CLIMATE2)
ml2 <- left_join(ml,ALL_LAT_LONG_CLIMATE2)
bm2 <- left_join(bm,ALL_LAT_LONG_CLIMATE2)
bl2 <- left_join(bl,ALL_LAT_LONG_CLIMATE2)

#realm
realm <- rast("E:/Coding Files/Ecosystem Maps/Ecoregions/Eco_Realms.tif")

xy <- data.frame(Lon = c(mm$Lon,ml$Lon,bm$Lon,bl$Lon),
                 Lat = c(mm$Lat,ml$Lat,bm$Lat,bl$Lat)
                 )%>%
  distinct()


points <- vect(xy, geom=c("Lon", "Lat"), crs="+proj=longlat +datum=WGS84")

#ecoregion
realm_points <- extract(realm, points)

realm_points <- realm_points %>% mutate(WWF_REALM2 = as.character(WWF_REALM2))%>%
  rename(Realm=WWF_REALM2)

xy$Realm <- realm_points[,2]
xy_with_points <- xy %>% filter(!is.na(Realm))

xy_nas <- xy %>% filter(is.na(Realm))

library(rSDM)

locs.sf <- vect(xy_nas, geom=c("Lon", "Lat"), crs="+proj=longlat +datum=WGS84")

xy_nas_moved <- points2nearestcell(locs.sf, realm,move=T,distance=NULL,table=F,map="none")

nas_moved_data <- extract(realm, xy_nas_moved)


xy_nas <- xy_nas %>% dplyr::select(1,2)

xy_nas$Realm <- as.character(nas_moved_data[,2])

realm_all <- rbind(xy_with_points,xy_nas)

mm2 <- left_join(mm2,realm_all)
ml2 <- left_join(ml2,realm_all)
bm2 <- left_join(bm2,realm_all)
bl2 <- left_join(bl2,realm_all)

vroom_write(mm2, "D:/Thesis Projects/Body Size Reviewer Analyses and data/Mammal_Mass_HLU.csv",delim=",")
vroom_write(ml2, "D:/Thesis Projects/Body Size Reviewer Analyses and data/Mammal_Length_HLU.csv",delim=",")
vroom_write(bm2, "D:/Thesis Projects/Body Size Reviewer Analyses and data/Bird_Mass_HLU.csv",delim=",")
vroom_write(bl2, "D:/Thesis Projects/Body Size Reviewer Analyses and data/Bird_Length_HLU.csv",delim=",")

##Add Urban and Agricultural Data Seperately
#HILDA DATASET

#1km resolution - resample to percent human land use in 5x5 km 

#get average of urban, cropland, paasture land, in each 5km cell

library(terra)
library(progress)
library(stringr)


hilda <- rast("D:/HILDA/hildaplus_states.nc")[[63:120]]
#convert cells to values of 1 for human land use type
#aggregate by a function of 5 and calculate sum
#divide cells by 25 to get % human land use
y <- c(1961:2018)
pbsp <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                         total = 58,
                         complete = "=",   # Completion bar character
                         incomplete = "-", # Incomplete bar character
                         current = ">",    # Current bar character
                         clear = FALSE,    # If TRUE, clears the bar when finish
                         width = 100)
#11 urban
#22 cropland
#33 pasture

#urban
for(i in 1:58){
  pbsp$tick()
  h <- hilda[[i]]
  h[h==11] <- 1
  h[h==22] <- 0
  h[h==33] <- 0
  
  h[h==40] <- 0
  h[h==41] <- 0
  h[h==42] <- 0
  h[h==43] <- 0
  h[h==44] <- 0
  h[h==45] <- 0
  h[h==55] <- 0
  h[h==66] <- 0
  h[h==77] <- 0
  
  a <- aggregate(h, fact=5, fun="sum", cores=6)
  a <- a/25
  writeRaster(a, paste0("D:/HILDA/HLU Rasters/Urban_Percent_",y[i],".tif"))
}
rm(hilda,h,a,y)
gc()

#agriculture
for(i in 1:58){
  pbsp$tick()
  h <- hilda[[i]]
  h[h==11] <- 0
  h[h==22] <- 1
  h[h==33] <- 1
  
  h[h==40] <- 0
  h[h==41] <- 0
  h[h==42] <- 0
  h[h==43] <- 0
  h[h==44] <- 0
  h[h==45] <- 0
  h[h==55] <- 0
  h[h==66] <- 0
  h[h==77] <- 0
  
  a <- aggregate(h, fact=5, fun="sum", cores=6)
  a <- a/25
  writeRaster(a, paste0("D:/HILDA/HLU Rasters/Agriculture_Percent_",y[i],".tif"))
}
rm(hilda,h,a,y)
gc()

###
library(vroom)
library(dplyr)
library(terra)
setwd("D:/Thesis Projects/Body Size Reviewer Analyses and data")
tpi <- vroom("D:/Thesis Projects/Climate Change Indicators/Niche Limits/TPI_API_Original/Month_Limits_Baseline_Final.csv")[,c(6:12)]%>%
  rename(TMax_lim = TMax, AMax_lim =AMax, TMin_lim = TMin, AMin_lim =AMin,Month=MonthNumber)
syns <- vroom("D:/PhD/Thesis/TPI and API/Master_Synonym_List_All.csv")%>%distinct(IUCN_Binomial,.keep_all = T)
##

M_Mass <- vroom("D:/Thesis Projects/Body Size Reviewer Analyses and data/ALL_DATA_CLIMATE.csv")%>%
  filter(Class=="Mammalia")%>%filter(!is.na(Mass))%>% filter(!Age=="Juv")%>%
  group_by(Binomial)%>%filter(n()>99)%>%ungroup()%>%
  mutate(LMass = log10(Mass),AI = ifelse(Aridity>100,100,Aridity))

M_Mass$Binomial <- str_squish(M_Mass$Binomial)  

M_Mass<-M_Mass %>%left_join(tpi)%>%
  mutate(TPI_Max = (Tx_max - TMin_lim)/(TMax_lim-TMin_lim),
         API = (AI - AMin_lim)/(AMax_lim-AMin_lim),
         Year_Fact = as.character(Year),
         Lat2 = Lat^2,
         HLU_Class = ifelse(HLU_Class==11,"Urban",
                            ifelse(HLU_Class==22,"Cropland",
                                   ifelse(HLU_Class==33,"Pasture",
                                          ifelse(HLU_Class==55,"Grassland",
                                                 ifelse(HLU_Class==66,"Other",
                                                        ifelse(HLU_Class==77,"Water",
                                                               ifelse(is.na(HLU_Class),NA,
                                                                      ifelse(HLU_Class==0,"Other","Forest")))))))))
msite <- M_Mass %>% group_by(Lat,Lon)%>%summarise(N_per_site=n())%>%ungroup()%>%mutate(Site = as.character(row_number()))
M_Mass <- left_join(M_Mass,msite)%>%filter(!is.na(TPI_Max))%>%filter(n()>99)%>%ungroup()

B_Mass <- vroom("D:/Thesis Projects/Body Size Reviewer Analyses and data/ALL_DATA_CLIMATE.csv")%>%
  filter(Class=="Aves")%>%filter(!is.na(Mass))%>%filter(!Age=="Juv")

B_Mass$Binomial <- str_squish(B_Mass$Binomial)  

syns_bmass <- syns %>% filter(Synonym %in% B_Mass$Binomial)%>%rename(Binomial = Synonym)

B_Mass <- B_Mass %>% group_by(Binomial)%>%filter(n()>99)%>%ungroup()%>%
  mutate(LMass = log10(Mass),AI = ifelse(Aridity>100,100,Aridity))%>%
  left_join(syns_bmass)%>%
  mutate(Binomial = ifelse(is.na(IUCN_Binomial),Binomial,IUCN_Binomial))%>%
  dplyr::select(-IUCN_Binomial)%>%
  left_join(tpi)%>%
  mutate(TPI_Max = (Tx_max - TMin_lim)/(TMax_lim-TMin_lim),
         API = (AI - AMin_lim)/(AMax_lim-AMin_lim),
         HLU_Class = ifelse(HLU_Class==11,"Urban",
                            ifelse(HLU_Class==22,"Cropland",
                                   ifelse(HLU_Class==33,"Pasture",
                                          ifelse(HLU_Class==55,"Grassland",
                                                 ifelse(HLU_Class==66,"Other",
                                                        ifelse(HLU_Class==77,"Water",
                                                               ifelse(is.na(HLU_Class),NA,
                                                                      ifelse(HLU_Class==0,"Other","Forest")))))))))
bsite <- B_Mass %>% group_by(Lat,Lon)%>%summarise(N_per_site=n())%>%ungroup()%>%mutate(Site = as.character(row_number()))
B_Mass <- left_join(B_Mass,bsite)%>%filter(!is.na(TPI_Max))%>%group_by(Binomial)%>%filter(n()>99)%>%ungroup()

M_Length <- vroom("D:/Thesis Projects/Body Size Reviewer Analyses and data/ALL_DATA_CLIMATE.csv")%>%
  filter(Class=="Mammalia")%>%filter(!is.na(Body_Length))%>%filter(!Age=="Juv")%>%
  group_by(Binomial)%>%filter(n()>99)%>%ungroup()%>%
  mutate(LLength = log10(Body_Length),AI = ifelse(Aridity>100,100,Aridity))
M_Length$Binomial <- str_squish(M_Length$Binomial)  

syns_mlength <- syns %>% filter(Synonym %in% M_Length$Binomial)%>%rename(Binomial = Synonym)

M_Length <- M_Length %>% left_join(syns_mlength)%>%
  mutate(Binomial = ifelse(is.na(IUCN_Binomial),Binomial,IUCN_Binomial))%>%
  dplyr::select(-IUCN_Binomial)%>%
  left_join(tpi)%>%
  mutate(TPI_Max = (Tx_max - TMin_lim)/(TMax_lim-TMin_lim),
         API = (AI - AMin_lim)/(AMax_lim-AMin_lim),
         Year_Fact = as.character(Year),
         HLU_Class = ifelse(HLU_Class==11,"Urban",
                            ifelse(HLU_Class==22,"Cropland",
                                   ifelse(HLU_Class==33,"Pasture",
                                          ifelse(HLU_Class==55,"Grassland",
                                                 ifelse(HLU_Class==66,"Other",
                                                        ifelse(HLU_Class==77,"Water",
                                                               ifelse(is.na(HLU_Class),NA,
                                                                      ifelse(HLU_Class==0,"Other","Forest")))))))))

msite2 <- M_Length %>% group_by(Lat,Lon)%>%summarise(N_per_site=n())%>%ungroup()%>%mutate(Site = as.character(row_number()))
M_Length <- left_join(M_Length,msite2)%>%filter(!is.na(TPI_Max))%>%group_by(Binomial)%>%filter(n()>99)%>%ungroup()                   

#
B_Length <- vroom("D:/Thesis Projects/Body Size Reviewer Analyses and data/ALL_DATA_CLIMATE.csv")%>%
  filter(Class=="Aves")%>%filter(!is.na(Body_Length))%>%filter(!Age=="Juv")%>%
  group_by(Binomial)%>%filter(n()>99)%>%ungroup()%>%
  mutate(LLength = log10(Body_Length), AI = ifelse(Aridity>100,100,Aridity))
B_Length$Binomial <- str_squish(B_Length$Binomial)  

syns_blength <- syns %>% filter(Synonym %in% B_Length$Binomial)%>%rename(Binomial = Synonym)

B_Length <- B_Length %>% left_join(syns_blength)%>%
  mutate(Binomial = ifelse(is.na(IUCN_Binomial),Binomial,IUCN_Binomial))%>%
  dplyr::select(-IUCN_Binomial)%>%
  left_join(tpi)%>%
  mutate(TPI_Max = (Tx_max - TMin_lim)/(TMax_lim-TMin_lim),
         API = (AI - AMin_lim)/(AMax_lim-AMin_lim),
         Year_Fact = as.character(Year),
         HLU_Class = ifelse(HLU_Class==11,"Urban",
                            ifelse(HLU_Class==22,"Cropland",
                                   ifelse(HLU_Class==33,"Pasture",
                                          ifelse(HLU_Class==55,"Grassland",
                                                 ifelse(HLU_Class==66,"Other",
                                                        ifelse(HLU_Class==77,"Water",
                                                               ifelse(is.na(HLU_Class),NA,
                                                                      ifelse(HLU_Class==0,"Other","Forest")))))))))

bsite2 <- B_Length %>% group_by(Lat,Lon)%>%summarise(N_per_site=n())%>%ungroup()%>%mutate(Site = as.character(row_number()))
B_Length <- left_join(B_Length,bsite2)%>%filter(!is.na(TPI_Max))%>%group_by(Binomial)%>%filter(n()>99)%>%ungroup()

mm<- M_Mass
ml<- M_Length
bm<- B_Mass
bl<- B_Length

xy <- data.frame(Lon = c(mm$Lon,ml$Lon,bm$Lon,bl$Lon),
                 Lat = c(mm$Lat,ml$Lat,bm$Lat,bl$Lat),
                 Year = c(mm$Year,ml$Year,bm$Year,bl$Year),
                 Month = c(mm$Month,ml$Month,bm$Month,bl$Month))%>%
  distinct()

xy_shape <-  vect(xy, geom=c("Lon", "Lat"), 
                  crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

dates <- data.frame(Year = c(mm$Year,ml$Year,bm$Year,bl$Year),
                    Month = c(mm$Month,ml$Month,bm$Month,bl$Month))%>%
  distinct()%>%mutate(Month_char = ifelse(Month>9,paste0(Month),paste0(0,Month)),
                      Month_let = ifelse(Month==1,"Jan",
                                         ifelse(Month==2,"Feb",
                                                ifelse(Month==3,"Mar",
                                                       ifelse(Month==4,"Apr",
                                                              ifelse(Month==5,"May",
                                                                     ifelse(Month==6,"June",
                                                                            ifelse(Month==7,"July",
                                                                                   ifelse(Month==8,"Aug",
                                                                                          ifelse(Month==9,"Sep",
                                                                                                 ifelse(Month==10,"Oct",
                                                                                                        ifelse(Month==11,"Nov","Dec"))))))))))))


library(progress)
pbsp <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                         total = nrow(dates),
                         complete = "=",   # Completion bar character
                         incomplete = "-", # Incomplete bar character
                         current = ">",    # Current bar character
                         clear = FALSE,    # If TRUE, clears the bar when finish
                         width = 100)

ALL_LAT_LONG_CLIMATE <- data.frame(NULL)

for(i in 1:nrow(dates)){
  pbsp$tick()
  
  Urb_Per <- rast(paste0("D:/HILDA/HLU Rasters/Urban_Percent_",dates$Year[i],".tif"))
  Agriculture_Per <- rast(paste0("D:/HILDA/HLU Rasters/Agriculture_Percent_",dates$Year[i],".tif"))
  
  hc <- (dates$Year[i] - 1960)
  
  urbvalue <- as.data.frame(as.data.frame(extract(Urb_Per, xy_shape))[,-1])
  colnames(urbvalue) <- "ULU"
  urbvalue$ALU <- (as.data.frame(extract(Agriculture_Per, xy_shape)))[,-1]
  urbvalue$Lat <- xy$Lat
  urbvalue$Lon <- xy$Lon
  
  urbvalue <- urbvalue %>% mutate(Year= dates$Year[i], Month = dates$Month[i])
  
  filter_lats <- xy%>%filter(Year==dates$Year[i] & Month==dates$Month[i])%>%
    dplyr::select(Lat,Lon)%>%mutate(Keep_val = "Yes")
  
  suppressWarnings(suppressMessages(urbvalue <- left_join(urbvalue,filter_lats)%>%filter(Keep_val=="Yes")%>%dplyr::select(-Keep_val)))
  
  ALL_LAT_LONG_CLIMATE <- rbind(ALL_LAT_LONG_CLIMATE,urbvalue)
  
}

vroom_write(ALL_LAT_LONG_CLIMATE, "D:/Thesis Projects/Body Size Reviewer Analyses and data/HLU_Lat_Lon_Dates.csv",delim=",")

ALL_LAT_LONG_CLIMATE2 <- ALL_LAT_LONG_CLIMATE %>% distinct()

mm2 <- left_join(mm,ALL_LAT_LONG_CLIMATE2)
ml2 <- left_join(ml,ALL_LAT_LONG_CLIMATE2)
bm2 <- left_join(bm,ALL_LAT_LONG_CLIMATE2)
bl2 <- left_join(bl,ALL_LAT_LONG_CLIMATE2)

vroom_write(mm2, "D:/Thesis Projects/GCB Revised Manuscript/Datasets/Data_4_Mammal_Mass.csv",delim=",")
vroom_write(ml2, "D:/Thesis Projects/GCB Revised Manuscript/Datasets/Data_5_Mammal_Length.csv",delim=",")
vroom_write(bm2, "D:/Thesis Projects/GCB Revised Manuscript/Datasets/Data_1_Bird_Mass.csv",delim=",")
vroom_write(bl2, "D:/Thesis Projects/GCB Revised Manuscript/Datasets/Data_2_Bird_Length.csv",delim=",")

##generate Size datasets
M_Mass_Length <- M_Mass_tree %>% filter(!is.na(Body_Length) & !Dataset=="Neon")%>%
  mutate(LLength = log10(Body_Length),
         Body_Size = (Mass^0.333)/Body_Length)%>%
  mutate(TPI_Max= ((TPI_Max - mean(TPI_Max,na.rm=T))/sd(TPI_Max, na.rm=T)),
         API= ((API - mean(API,na.rm=T))/sd(API, na.rm=T)),
         HLU= ((HLU - mean(HLU,na.rm=T))/sd(HLU, na.rm=T)),
         ULU= ((ULU - mean(ULU,na.rm=T))/sd(ULU, na.rm=T)),
         ALU= ((ALU - mean(ALU,na.rm=T))/sd(ALU, na.rm=T)),
         Year = ((Year - mean(Year,na.rm=T))/sd(Year, na.rm=T)),
         LMass= ((LMass - mean(LMass,na.rm=T))/sd(LMass, na.rm=T)),
         LLength= ((LLength - mean(LLength,na.rm=T))/sd(LLength, na.rm=T)),
         Body_Size = ((Body_Size - mean(Body_Size,na.rm=T))/sd(Body_Size, na.rm=T)),
         Body_Size_w = Winsorize(Body_Size, quantile(Body_Size, probs=c(0.001,0.999),na.rm=T)),
         LLength_w = Winsorize(LLength, quantile(LLength, probs=c(0.001,0.999),na.rm=T)))


B_Mass_Length <- B_Mass_tree %>% filter(!is.na(Body_Length))%>%
  mutate(LLength = log10(Body_Length),
         Body_Size = (Mass^0.333)/Body_Length,
         Body_Size_w = Winsorize(Body_Size, quantile(Body_Size, probs=c(0.01,0.99),na.rm=T)))%>%
  mutate(TPI_Max= ((TPI_Max - mean(TPI_Max,na.rm=T))/sd(TPI_Max, na.rm=T)),
         API= ((API - mean(API,na.rm=T))/sd(API, na.rm=T)),
         HLU= ((HLU - mean(HLU,na.rm=T))/sd(HLU, na.rm=T)),
         ULU= ((ULU - mean(ULU,na.rm=T))/sd(ULU, na.rm=T)),
         ALU= ((ALU - mean(ALU,na.rm=T))/sd(ALU, na.rm=T)),
         Year = ((Year - mean(Year,na.rm=T))/sd(Year, na.rm=T)),
         LMass= ((LMass - mean(LMass,na.rm=T))/sd(LMass, na.rm=T)),
         LLength= ((LLength - mean(LLength,na.rm=T))/sd(LLength, na.rm=T)),
         Body_Size = ((Body_Size - mean(Body_Size,na.rm=T))/sd(Body_Size, na.rm=T)),
         Body_Size_w = ((Body_Size_w - mean(Body_Size_w,na.rm=T))/sd(Body_Size_w, na.rm=T)))

vroom_write(M_Mass_Length,"D:/Thesis Projects/GCB Revised Manuscript/Datasets/Data_6_Mammal_Size.csv",delim=",")
vroom_write(B_Mass_Length,"D:/Thesis Projects/GCB Revised Manuscript/Datasets/Data_3_Bird_Size.csv",delim=",")
