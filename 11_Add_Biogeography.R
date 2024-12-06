setwd("D:/Thesis Projects/Body Size Reviewer Analyses and data")
tpi <- vroom("D:/Thesis Projects/Climate Change Indicators/Niche Limits/TPI_API_Original/Month_Limits_Baseline_Final.csv")[,c(6:12)]%>%
  rename(TMax_lim = TMax, AMax_lim =AMax, TMin_lim = TMin, AMin_lim =AMin,Month=MonthNumber)
syns <- vroom("D:/PhD/Thesis/TPI and API/Master_Synonym_List_All.csv")%>%distinct(IUCN_Binomial,.keep_all = T)
##

#MASS DATASETS
M_Mass2 <- vroom("Final_Master_MASS_DATA_CLIMATE.csv")%>%
  filter(Class=="Mammalia")%>%
  mutate(LMass = log10(Mass),AI = ifelse(Aridity>100,100,Aridity))%>%
  left_join(tpi)%>%
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
msite <- M_Mass2 %>% group_by(Lat,Lon)%>%summarise(N_per_site=n())%>%ungroup()%>%mutate(Site = as.character(row_number()))
M_Mass2 <- left_join(M_Mass2,msite)                           


B_Mass2 <- vroom("Final_Master_MASS_DATA_CLIMATE.csv")%>%
  filter(Class=="Aves")

syns_bmass <- syns %>% filter(Synonym %in% B_Mass2$Binomial)%>%rename(Binomial = Synonym)

B_Mass2 <- B_Mass2 %>% 
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
bsite <- B_Mass2 %>% group_by(Lat,Lon)%>%summarise(N_per_site=n())%>%ungroup()%>%mutate(Site = as.character(row_number()))
B_Mass2 <- left_join(B_Mass2,bsite)   
#LENGTH DATASETS
M_Length2 <- vroom("Final_Master_LENGTH_DATA_CLIMATE.csv")%>%
  filter(Class=="Mammalia")%>%
  mutate(LMass = log10(Mass),AI = ifelse(Aridity>100,100,Aridity))

syns_mlength <- syns %>% filter(Synonym %in% M_Length2$Binomial)%>%rename(Binomial = Synonym)

M_Length2 <- M_Length2 %>% left_join(syns_mlength)%>%
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

msite2 <- M_Length2 %>% group_by(Lat,Lon)%>%summarise(N_per_site=n())%>%ungroup()%>%mutate(Site = as.character(row_number()))
M_Length2 <- left_join(M_Length2,msite2)                           

#
B_Length2 <- vroom("Final_Master_LENGTH_DATA_CLIMATE.csv")%>%
  filter(Class=="Aves")%>%
  mutate(LLength = log10(Body_Length), AI = ifelse(Aridity>100,100,Aridity))

syns_blength <- syns %>% filter(Synonym %in% B_Length2$Binomial)%>%rename(Binomial = Synonym)

B_Length2 <- B_Length2 %>% left_join(syns_blength)%>%
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

bsite2 <- B_Length2 %>% group_by(Lat,Lon)%>%summarise(N_per_site=n())%>%ungroup()%>%mutate(Site = as.character(row_number()))
B_Length2 <- left_join(B_Length2,bsite2)

#####

xy <- data.frame(Lon = c(M_Mass2$Lon,M_Length2$Lon,B_Mass2$Lon,B_Length2$Lon),
                 Lat = c(M_Mass2$Lat,M_Length2$Lat,B_Mass2$Lat,B_Length2$Lat)
)%>%
  distinct()


realm <- rast("E:/Coding Files/Ecosystem Maps/Ecoregions/Eco_Realms.tif")

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

M_Mass2 <- M_Mass2 %>% dplyr::select(-Realm) %>% left_join(realm_all)
M_Length2 <- M_Length2%>% dplyr::select(-Realm) %>%left_join(realm_all)
B_Mass2 <- B_Mass2 %>% dplyr::select(-Realm) %>%left_join(realm_all)
B_Length2  <- B_Length2%>% dplyr::select(-Realm) %>%left_join(realm_all)

vroom_write(M_Mass2, "D:/Thesis Projects/Body Size Reviewer Analyses and data/Mammal_Mass_HLU_Realm_ALLOBS.csv",delim=",")
vroom_write(M_Length2, "D:/Thesis Projects/Body Size Reviewer Analyses and data/Mammal_Length_HLU_ALLOBS.csv",delim=",")
vroom_write(B_Mass2, "D:/Thesis Projects/Body Size Reviewer Analyses and data/Bird_Mass_HLU_ALLOBS.csv",delim=",")
vroom_write(B_Length2, "D:/Thesis Projects/Body Size Reviewer Analyses and data/Bird_Length_HLU_ALLOBS.csv",delim=",")
