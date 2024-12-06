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

