library(vroom)
library(dplyr)
#create dataset variable
setwd("E:/Coding Files/Chapter 2 Validation/Second filter")

a <- vroom("VertNet_Filtered_Final_v2.csv")%>%mutate(Dataset = "VertNet")
b <- vroom("Atl_Mammals_Filtered_Final_v2.csv")%>%mutate(Dataset = "Atlantic")
c <- vroom("Atl_Birds_Filtered_Final_v2.csv")%>%mutate(Dataset = "Atlantic")
d <- vroom("NEON_Filtered_Final_v2.csv")%>%mutate(Dataset = "Neon")
e <- vroom("DOetal_Filtered_Final_v2.csv")%>%mutate(Dataset = "DOetal")
Template <- rbind(a,b,c,d,e)%>%mutate(Day=as.numeric(Day),Month=as.numeric(Month))

lifes <- vroom("D:/Repositories/Thesis-Chapter-2/Final Code/Datafiles/agesort.csv")%>%
  rename(Age=class)%>%distinct(Age,.keep_all = T)
sexs <- vroom("D:/Repositories/Thesis-Chapter-2/Final Code/Datafiles/sexes_filter.csv")%>%
  rename(Sex=class)


Updated_Data <- left_join(Template,sexs)%>%select(-Sex)%>%rename(Sex=replace)%>%left_join(lifes)%>%
  select(-Age)%>%rename(Age=replace)%>%mutate(Age= ifelse(is.na(Age), "Unknown",Age))%>%
  filter(Month %in% c(1:12))

##
###
library(terra)

xy <- Updated_Data%>%select(Lat,Lon,Year,Month)%>%distinct() 

xy_shape <-  vect(xy, geom=c("Lon", "Lat"), 
                  crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

dates <- Updated_Data%>%select(Year,Month)%>%distinct()%>%
  mutate(Month_char = ifelse(Month>9,paste0(Month),paste0(0,Month)),
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



eco_relms <- rast("E:/Coding Files/Ecosystem Maps/Ecoregions/Eco_Realms.tif")
eco_reg <- rast("E:/Coding Files/Ecosystem Maps/Ecoregions/Ecoregions.tif")

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
  
  tmax <- rast(paste0("E:/Coding Files/Climate Data/Temperature Files/Tmax/tmax_",dates$Year[i],"_",dates$Month_char[i],".tif"))
  ar <- rast(paste0("E:/Coding Files/Climate Data/Environmental Maps/",dates$Year[i],"/Aritity_",dates$Month_let[i],"_",dates$Year[i],".tif"))
  HLU_Per <- rast(paste0("D:/HILDA/HLU Rasters/HLU_Percent_",dates$Year[i],".tif"))
  
  hc <- (dates$Year[i] - 1960)
  HLU_class <- rast("D:/HILDA/hildaplus_states.nc")[[(62+hc)]]
  
  tmxvalue <- as.data.frame(as.data.frame(extract(tmax, xy_shape))[,-1])
  colnames(tmxvalue) <- "Tx_max"
  tmxvalue$Aridity <- (as.data.frame(extract(ar, xy_shape)))[,-1]
  tmxvalue$HLU <- (as.data.frame(extract(HLU_Per, xy_shape)))[,-1]
  tmxvalue$HLU_Class <- (as.data.frame(extract(HLU_class, xy_shape)))[,-1]
  tmxvalue$Realm <- (as.data.frame(extract(eco_relms, xy_shape)))[,-1]
  tmxvalue$Ecoregion <- (as.data.frame(extract(eco_reg, xy_shape)))[,-1]
  tmxvalue$Lat <- xy$Lat
  tmxvalue$Lon <- xy$Lon
  
  tmxvalue <- tmxvalue %>% mutate(Year= dates$Year[i], Month = dates$Month[i])
  
  filter_lats <- xy%>%filter(Year==dates$Year[i] & Month==dates$Month[i])%>%
    dplyr::select(Lat,Lon)%>%mutate(Keep_val = "Yes")
  
  suppressWarnings(suppressMessages(tmxvalue <- left_join(tmxvalue,filter_lats)%>%filter(Keep_val=="Yes")%>%dplyr::select(-Keep_val)))
  
  ALL_LAT_LONG_CLIMATE <- rbind(ALL_LAT_LONG_CLIMATE,tmxvalue)
  
}
ALL_LAT_LONG_CLIMATE2 <- ALL_LAT_LONG_CLIMATE %>% distinct()
ALL_WITH_CLIMATE <- left_join(Updated_Data,ALL_LAT_LONG_CLIMATE2)%>%filter(!is.na(Tx_max))
vroom_write(ALL_WITH_CLIMATE,"D:/Thesis Projects/Body Size Reviewer Analyses and data/ALL_DATA_CLIMATE.csv")


## generate Datasets with less stringent number of occurances 
Final_Data <- vroom("E:/Coding Files/Chapter 2 Validation/Final Datafiles/Final_Master_Data_Tlims.csv")%>%
  mutate(Month=as.numeric(Month))

Final_Data_2 <- left_join(Final_Data,Template2)

missings <- Final_Data_2 %>% filter(is.na(Dataset))%>%dplyr::select(-Dataset)%>%
  mutate(Lon=round(Lon,digits=5),
         Lat=round(Lat,digits=5))

Template3 <- Template2 %>%
  mutate(Lon=round(Lon,digits=5),
         Lat=round(Lat,digits=5))

missings2 <- left_join(missings,Template3)%>%mutate(Dataset = ifelse(is.na(Dataset),"VertNet", Dataset))%>%distinct()

Final_Data_3 <- Final_Data_2 %>% filter(!is.na(Dataset))%>%rbind(missings2)

vroom_write(Final_Data_3, "E:/Coding Files/Chapter 2 Validation/Final Datafiles/ALL_FINAL_DATA_REVISION.csv")
##mass data

Adult_Mass <- Final_Data_3 %>% filter(!is.na(Mass))%>%filter(Age=="Adult")%>%mutate(logmass = log(Mass))%>%
  group_by(Binomial)%>% filter(n()>1)%>%summarise(Median_Mass = median(logmass),
                                                  MAD_Mass = mad(logmass))%>%ungroup()

Mass <- Final_Data_3 %>% filter(!is.na(Mass))%>%mutate(logmass = log(Mass))

Mass2 <- left_join(Mass,Adult_Mass)

Mass_All <- Mass2 %>% filter(!is.na(Median_Mass))%>%
  filter(logmass > Median_Mass-(5*MAD_Mass) & logmass<Median_Mass+(5*MAD_Mass))%>%
  left_join(Template2)

vroom_write(Mass_All,"E:/Coding Files/Chapter 2 Validation/Final Datafiles/Final_Master_MASS_DATA.csv" )

##length data

Adult_Length <- Final_Data_3%>%filter(!Dataset=="Neon")%>%filter(!is.na(Body_Length))%>%filter(Age=="Adult")%>%mutate(loglength= log(Body_Length))%>%
  group_by(Binomial)%>% filter(n()>1)%>%summarise(Median_Length = median(loglength),
                                                  MAD_Length = mad(loglength))

Length1 <- Final_Data_3 %>%filter(!Dataset=="Neon")%>% filter(!is.na(Body_Length))%>%mutate(loglength = log(Body_Length))

Length2 <- left_join(Length1,Adult_Length)

Length3 <- Length2 %>% filter(!is.na(Median_Length))%>%
  filter(loglength > Median_Length-(5*MAD_Length)&loglength<Median_Length+(5*MAD_Length))

vroom_write(Length3, "E:/Coding Files/Chapter 2 Validation/Final_Master_LENGTH_DATA.csv", delim=",")

###
library(terra)
Length3 <- vroom("E:/Coding Files/Chapter 2 Validation/Final_Master_LENGTH_DATA.csv", delim=",")%>%filter(Month<13 & Month >0)
Mass_All <- vroom("E:/Coding Files/Chapter 2 Validation/Final Datafiles/Final_Master_MASS_DATA.csv")%>%filter(Month<13 & Month >0)

xy_mass_d <- Mass_All%>%select(Lat,Lon,Year,Month)%>%distinct() 
xy_length_d <- Length3%>%select(Lat,Lon,Year,Month)%>%distinct()
xy_dates <- rbind(xy_mass_d,xy_length_d)%>%distinct()

xy_mass <- Mass_All%>%select(Lat,Lon)%>%distinct() 
xy_length <- Length3%>%select(Lat,Lon)%>%distinct()
xy <- rbind(xy_mass,xy_length)%>%distinct()

xy_shape <-  vect(xy, geom=c("Lon", "Lat"), 
                  crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

date_mass <- Mass_All%>%select(Year,Month)%>%distinct() 
date_length <- Length3%>%select(Year,Month)%>%distinct()
dates <- rbind(date_mass,date_length)%>%distinct()%>%mutate(Month_char = ifelse(Month>9,paste0(Month),paste0(0,Month)),
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



eco_relms <- rast("E:/Coding Files/Ecosystem Maps/Ecoregions/Eco_Realms.tif")
eco_reg <- rast("E:/Coding Files/Ecosystem Maps/Ecoregions/Ecoregions.tif")

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
  
  tmax <- rast(paste0("E:/Coding Files/Climate Data/Temperature Files/Tmax/tmax_",dates$Year[i],"_",dates$Month_char[i],".tif"))
  tmin <- rast(paste0("E:/Coding Files/Climate Data/Temperature Files/Tmin/tmin_",dates$Year[i],"_",dates$Month_char[i],".tif"))
  tmean <- rast(paste0("E:/Coding Files/Climate Data/Temperature Files/Tmean/tmp_",dates$Year[i],"__",dates$Month_char[i],".tif"))
  ar <- rast(paste0("E:/Coding Files/Climate Data/Environmental Maps/",dates$Year[i],"/Aritity_",dates$Month_let[i],"_",dates$Year[i],".tif"))
  HLU_Per <- rast(paste0("D:/HILDA/HLU Rasters/HLU_Percent_",dates$Year[i],".tif"))
  
  hc <- (dates$Year[i] - 1960)
  HLU_class <- rast("D:/HILDA/hildaplus_states.nc")[[(62+hc)]]
  
  tmxvalue <- as.data.frame(as.data.frame(extract(tmax, xy_shape))[,-1])
  colnames(tmxvalue) <- "Tx_max"
  tmxvalue$Tx_min <- (as.data.frame(extract(tmin, xy_shape)))[,-1]
  tmxvalue$Tx_mean <- (as.data.frame(extract(tmean, xy_shape)))[,-1]
  tmxvalue$Aridity <- (as.data.frame(extract(ar, xy_shape)))[,-1]
  tmxvalue$HLU <- (as.data.frame(extract(HLU_Per, xy_shape)))[,-1]
  tmxvalue$HLU_Class <- (as.data.frame(extract(HLU_class, xy_shape)))[,-1]
  tmxvalue$Realm <- (as.data.frame(extract(eco_relms, xy_shape)))[,-1]
  tmxvalue$Ecoregion <- (as.data.frame(extract(eco_reg, xy_shape)))[,-1]
  tmxvalue$Lat <- xy$Lat
  tmxvalue$Lon <- xy$Lon
  
  tmxvalue <- tmxvalue %>% mutate(Year= dates$Year[i], Month = dates$Month[i])
  
  filter_lats <- xy_dates%>%filter(Year==dates$Year[i] & Month==dates$Month[i])%>%
    dplyr::select(Lat,Lon)%>%mutate(Keep_val = "Yes")
  
  suppressWarnings(suppressMessages(tmxvalue <- left_join(tmxvalue,filter_lats)%>%filter(Keep_val=="Yes")%>%dplyr::select(-Keep_val)))
  
  ALL_LAT_LONG_CLIMATE <- rbind(ALL_LAT_LONG_CLIMATE,tmxvalue)
  
}

ALL_MASS_WITH_CLIMATE <- left_join(Mass_All,ALL_LAT_LONG_CLIMATE)
vroom_write(ALL_MASS_WITH_CLIMATE,"D:/Thesis Projects/Body Size Reviewer Analyses and data/Final_Master_MASS_DATA_CLIMATE.csv")
ALL_LENGTH_WITH_CLIMATE <- left_join(Length3,ALL_LAT_LONG_CLIMATE)
vroom_write(ALL_LENGTH_WITH_CLIMATE,"D:/Thesis Projects/Body Size Reviewer Analyses and data/Final_Master_LENGTH_DATA_CLIMATE.csv")


##After extracting climate and land use data
Mass_10 <- ALL_MASS_WITH_CLIMATE %>% filter(!is.na(Median_Mass))%>%
  filter(logmass > Median_Mass-(5*MAD_Mass) & logmass<Median_Mass+(5*MAD_Mass))%>%
  group_by(Binomial)%>%filter(n()>9)%>%ungroup()


vroom_write(Mass_10, "D:/Thesis Projects/Body Size Reviewer Analyses and data/Final_Master_MASS_DATA_CLIMATE_10.csv")

Length_10 <- ALL_LENGTH_WITH_CLIMATE %>% filter(!is.na(Median_Mass))%>%
  filter(logmass > Median_Mass-(5*MAD_Mass) & logmass<Median_Mass+(5*MAD_Mass))%>%
  group_by(Binomial)%>%filter(n()>9)%>%ungroup()


vroom_write(Length_10, "D:/Thesis Projects/Body Size Reviewer Analyses and data/Final_Master_LENGTH_DATA_CLIMATE_10.csv")
