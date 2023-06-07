#' ---
#' Step 5: Organize and clean datafiles
#' Matthew Watson
#' correts sp names, updates lifestage and sex classification and selects approriate data
#' ---
library(vroom)
library(dplyr)
library(readr)

#### VertNet Data

setwd("E:/Coding Files/Chapter 2 Validation/Raw Datafiles")

#Vertnet files were renamed for ease of coding
b <- vroom("Bird_BS.csv")
m <- vroom("Mammal_BS.csv")

all_classes <- rbind(b,m)

rm(b,m)
gc()

all_classes <- all_classes %>%
  filter(between(decimallatitude, -90, 90)) %>%
  filter(between(decimallongitude, -180, 180)) %>%
  filter(!is.na(specificepithet)) %>%
  filter(year > 1960 & year < 2019) %>%
  filter(!decimallatitude == decimallongitude)%>%
  filter(!is.na(massing) | !is.na(lengthinmm))%>%
  mutate(Data_Source = "VertNet")

vroom_write(all_classes, "Filtered_vertnet.csv")
gc()
#### DO et al Data
# raw data was first opened in excel and deliminated using ; and date was separated into 3 columns year,month,day 
setwd("D:/Matt/Thesis Files/Chapter 2/Body Size Data/Official Files/Do etal")

do <- vroom("DOetal_sep.csv")
do_filter <- do %>%
  filter(between(Lat, -90, 90)) %>%
  filter(between(Lon, -180, 180)) %>%
  filter(year > 1960 & year<2019) %>%
  filter(!is.na(Weight..gr.))%>%
  rename(Weigth=Weight..gr.)%>%
  mutate(Data_Source = "DO et al")
vroom_write(do_filter, "Filtered_DOetal_v2.csv", delim = ",")
rm(do)
####Atlantic data
setwd("D:/Matt/Thesis Files/Chapter 2/Body Size Data/Official Files/South America Atlantic")

b <- vroom("birds.csv")

b_filter <- b %>%
  filter(between(Latitude_decimal_degrees, -90, 90)) %>%
  filter(between(Longitude_decimal_degrees, -180, 180)) %>%
  filter(Year > 1960 & Year < 2019) %>%
  filter(!is.na(Body_mass.g.)|!is.na(Body_length.mm.))%>%
  mutate(Data_Source = "Atlantic")

vroom_write(b_filter, "Filtered_Atl_Birds_v2.csv", delim = ",")

m <- vroom("mammals.csv")
m_filter <- m %>%
  filter(between(latitude, -90, 90)) %>%
  filter(between(longitude, -180, 180)) %>%
  filter(year > 1960 & year < 2019) %>%
  filter(!is.na(body_mass)|!is.na(body_length))%>%
  mutate(Data_Source = "Atlantic")
vroom_write(m_filter, "Filtered_Atl_Mammals.csv", delim = ",")

####Neon data

setwd('D:/Matt/Thesis Files/Chapter 2/Body Size Data/Mammals/RAW mammal data/NEON/RAW')
df <- list.files(path='.')
l <- vroom("NEON_mammal_alldata.csv", delim = ",")

out <- l%>%
  filter(taxonRank=="Species"|taxonRank=="species")%>%
  filter(!is.na(decimalLatitude))%>%
  filter(!is.na(decimalLongitude))%>%
  filter(recapture=="N")%>%
  filter(!is.na(totalLength)|!is.na(weight))%>%
  mutate(Data_Source = "Neon")
write.csv(out, "Filtered_NEON_v2.csv")

####COMBINE FILES
setwd("E:/Coding Files/Chapter 2 Validation/First filter")

vert <- vroom("Filtered_vertnet.csv")
vertsub <- vert[c(35,36,37,83,98,116,130,160,161,162,163,165,189,190)]
vertsub <- vertsub %>%
  mutate(Binomial = paste(genus, " ", specificepithet))%>%
  relocate(class:specificepithet, .before = day)%>%
  relocate(Binomial, .before=day)%>%
  relocate(massing, .before=day)%>%
  relocate(lengthinmm, .before=day)%>%
  relocate(decimallongitude, .before = day)%>%
  relocate(decimallatitude, .before=day)%>%
  relocate(year, .before = day)%>%
  relocate(month, .before=day)%>%
  relocate(lifestage, .after=day)%>%
  relocate(sex, .after=lifestage)%>%%>%
  relocate(Data_Source)%>%
  rename(Mass=massing,
         Year=year,
         Month=month,
         Day=day,
         Lat=decimallatitude,
         Lon=decimallongitude,
         Class=class,
         Order=order,
         Family=family,
         Genus=genus,
         Species=specificepithet,
         Age=lifestage,
         Sex = sex,
         Body_Length = lengthinmm)
vertsub <- vertsub[1:16]
vroom_write(vertsub, "VertNet_Filtered_Final_v2.csv", delim = ",")


Atl <- vroom("Filtered_Atl_Mammals.csv")
Atlsub <- Atl[c(4,5,6,7,8,9,13,14,16,17,18,19)]
Atlsub <- Atlsub %>%
  mutate(Month = NA)%>%
  mutate(Day = NA)%>%
  mutate(Class="Mammalia")%>%
  relocate(Class, .before=order)%>%
  relocate(longitude, .before = reproductive_stage)%>%
  relocate(latitude, .before=reproductive_stage)%>%
  relocate(year, .before = reproductive_stage)%>%
  relocate(Month, .before=reproductive_stage)%>%
  relocate(Day, .before=reproductive_stage)%>%
  relocate(Data_Source)%>%
  rename(Mass=body_mass,
         Body_Length=body_length,
         Year=year,
         Lat=latitude,
         Lon=longitude,
         Order=order,
         Family=family,
         Genus=genus,
         Species=binomial,
         Binomial=binomial,
         Age=reproductive_stage,
         Sex=sex)
Atlsub <- Atlsub[,-16]
Atlsub <- Atlsub %>% mutate(Sp = Binomial)%>%
  tidyr::separate(Sp, c("G", "Species"), sep = " ")%>%
  select(-G)%>% relocate(Species, .before=Binomial)

vroom_write(Atlsub, "Atl_Mammals_Filtered_Final_v2.csv", delim = ",")

Atlb <- vroom("Filtered_Atl_Birds_v2.csv")
Atlbsub <- Atlb[c(5,6,7,8,9,10,11,25,27,30,33,35,37,59,60,76,77)]
colnames(Atlbsub)
Atlbsub <- Atlbsub %>%
  tidyr::separate(Date, c("yr", "Month", "Day"))%>%
  select(-yr)%>%
  mutate(Class="Aves")%>%
  relocate(Class, .before=Order)%>%
  relocate(Longitude_decimal_degrees:Latitude_decimal_degrees, .before = Age)%>%
  relocate(Year, .before=Age)%>%
  relocate(Month, .before=Age)%>%
  relocate(Day, .before=Age)%>%
  relocate(Data_Source)%>%
  rename(Mass=Body_mass.g.,
         Body_Length=Body_length.mm.,
         B_Length=Bill_length.mm.,
         B_Width=Bill_width.mm.,
         B_Depth=Bill_depth.mm.,
         Head_Length=Head_length_total.mm.,
         Lon = Longitude_decimal_degrees,
         Lat = Latitude_decimal_degrees)
Altbsub2 <- Atlbsub[,-c(10:13)]
vroom_write(Altbsub2, "Atl_Birds_Filtered_Final_v2.csv", delim = ",")


Neon <- vroom("Filtered_NEON_v2.csv")
Neonsub <- Neon[,c("decimalLatitude", "decimalLongitude", "collectDate", "scientificName", "sex", "recapture",  "lifeStage", "totalLength", "weight")]
Neonsub <- Neonsub %>%
  mutate(Order = NA)%>%
  mutate(Family = NA)%>%
  mutate(GS = scientificName)%>%
  tidyr::separate(GS,c("Genus","Species"), sep=" ")%>%
  mutate(Class="Mammalia")%>%
  rename(Binomial=scientificName,Sex=sex,Age=lifeStage,Lat=decimalLatitude, Lon=decimalLongitude,
         Mass=weight, Body_Length=totalLength)%>%
  relocate(Class, .before=Order)%>%
  relocate(Class:Species, .before = Lat)%>%
  relocate(Binomial, .before=Lat)%>%
  relocate(Mass, .before=Lat)%>%
  relocate(Body_Length, .before=Lat)%>%
  relocate(Lon, .before=Lat)%>%
  relocate(collectDate, .before=Sex)%>%
  relocate(Age, .before=Sex)%>%
  relocate(Data_Source)%>%
  tidyr::separate(collectDate, c("Year","Month","Day"), sep="-")%>%
  filter(recapture=="N")%>%
  select(-recapture)

vroom_write(Neonsub, "NEON_Filtered_Final_v2.csv")

do <- vroom("Filtered_DOetal_v2.csv")
dosub <- do[,c(3,4,5,6,7,8,10,14,21,22,23,25,26)]
dosub <- do%>%
  mutate(Body_Length = NA)%>%
  mutate(Age = NA)%>%
  relocate(Body_Length, .before = Sex)%>%
  relocate(Lon, .before = year)%>%
  relocate(Lat, .before=year)%>%
  relocate(Age, .after = day)%>%
  relocate(Sex, .after=Age)%>%
  relocate(Data_Source)%>%
  rename(Mass=Weigth,
         Year=year,
         Month=month,
         Day=day,
         Binomial=Species)%>%
  mutate(G = Binomial)%>%
  relocate(G, .before = Binomial)%>%
  tidyr::separate(G, c("Genus","Species"),sep=" ")

vroom_write(dosub, "DOetal_Filtered_Final_v2.csv")

setwd("E:/Coding Files/Chapter 2 Validation/Second filter")

a <- vroom("VertNet_Filtered_Final_v2.csv")
b <- vroom("Atl_Mammals_Filtered_Final_v2.csv")
c <- vroom("Atl_Birds_Filtered_Final_v2.csv")
d <- vroom("NEON_Filtered_Final_v2.csv")
e <- vroom("DOetal_Filtered_Final_v2.csv")
Template <- rbind(a,b,c,d,e)
vroom_write(Template, "Full_Dataset_Filtered.csv", delim = ",")

################################################################################################
# add variables and correct sex,age and binomial names
# filter based on body size measurement

library(vroom)
library(dplyr)

#call in masterdataset
All_Data <- vroom("E:/Coding Files/Chapter 2 Validation/Full_Dataset_Filtered.csv", delim = ",")

#adds fields for season and decade
All_Data <- All_Data%>%
  mutate(Decade= ifelse(Year<1970, 1,
                        ifelse(Year<1980,2,
                               ifelse(Year<1990,3,
                                      ifelse(Year<2000,4,
                                             ifelse(Year<2010,5,
                                                    ifelse(Year<2019,6,
                                                           0)))))))%>%
  mutate(Range = ifelse(Decade == 1, "<1970",
                        ifelse(Decade == 2, "1970-1979",
                               ifelse(Decade == 3, "1980-1989",
                                      ifelse(Decade == 4, "1990-1999",
                                             ifelse(Decade == 5, "2000-2009",
                                                    ifelse(Decade == 6, "2010-2018",
                                                           NA)))))))%>%
  mutate(Season = ifelse(Month == "01", "Winter",
                         ifelse(Month == "02", "Winter",
                                ifelse(Month == "03", "Spring",
                                       ifelse(Month == "04", "Spring",
                                              ifelse(Month == "05", "Spring",
                                                     ifelse(Month == "06", "Summer",
                                                            ifelse(Month == "07", "Summer",
                                                                   ifelse(Month == "08", "Summer",
                                                                          ifelse(Month == "09", "Fall",
                                                                                 ifelse(Month == "10", "Fall",
                                                                                        ifelse(Month == "11", "Fall",
                                                                                               ifelse(Month == "12", "Winter",NA)))))))))))))%>%
  mutate(Mnth = ifelse(Month == "01", "Jan",
                       ifelse(Month == "02", "Feb",
                              ifelse(Month == "03", "Mar",
                                     ifelse(Month == "04", "Apr",
                                            ifelse(Month == "05", "May",
                                                   ifelse(Month == "06", "June",
                                                          ifelse(Month == "07", "July",
                                                                 ifelse(Month == "08", "Aug",
                                                                        ifelse(Month == "09", "Sep",
                                                                               ifelse(Month == "10", "Oct",
                                                                                      ifelse(Month == "11", "Nov",
                                                                                             ifelse(Month == "12", "Dec",NA)))))))))))))
N <- All_Data %>% filter(Lat >= 0)
S <- All_Data %>% filter(Lat < 0)%>%
  mutate(ss = if_else(Season=="Winter", "Summer",
                      if_else(Season=="Spring","Fall",
                              if_else(Season=="Summer","Winter",
                                      if_else(Season=="Fall", "Spring", NA)))))
  
S$Season <- S$ss
S <- S[,-20]
All_Data_Season<- rbind(N,S)

All_Data_2 <- All_Data_Season[with(All_Data_Season, order(Class, Binomial)), ]

vroom_write(All_Data_2, "Full_Dataset_Filtered_Seasons.csv", delim=",")
################# Lifestage and Sex class corrections#################################
lifes <- vroom("E:/Coding Files/Repositories/Thesis-Chapter-2/Final Code/Datafiles/agesort.csv")
sexs <- vroom("E:/Coding Files/Repositories/Thesis-Chapter-2/Final Code/Datafiles/sexes_filter.csv")

for (id in 1:nrow(lifes)){
  All_Data_2$Age[All_Data_2$Age %in% lifes$class[id]]<-lifes$replace[id]
}
for (id in 1:nrow(sexs)){
  All_Data_2$Sex[All_Data_2$Sex %in% sexs$class[id]]<-sexs$replace[id]
}

All_Data_3 <- All_Data_2 %>% mutate(Age = ifelse(Age=="pregnant", "Adult",
                                                 if_else(Age=="scroted","Unknown", Age)))

vroom_write(All_Data_3, "E:/Coding Files/Chapter 2 Validation/Full_Dataset_Filtered_Seasons_age_sex.csv", delim=",")
################# Name Correction ########################################

All_Data_3 <- vroom("E:/Coding Files/Chapter 2 Validation/Full_Dataset_Filtered_Seasons_age_sex.csv", delim=",")
update_binomial <- stringr::str_squish(All_Data_3$Binomial) #removes extra spaces in names
All_Data_3$Binomial <- update_binomial #updates bionomial column
#remove jueveniles and unowkn sex
All_Data_4 <- All_Data_3 %>% filter(!Sex=="Unknown")%>%filter(!Age=="Juv")%>%
  filter(!is.na(Year))%>%filter(!is.na(Lon))%>%filter(!is.na(Lat))%>%filter(!is.na(Month))

#get unique names then compare between IUCN list and datalist
Data_Names <- unique(All_Data_4$Binomial)

IUCN_Names <- vroom("E:/Coding Files/Chapter 2 Validation/Extra files for process/All_Species_Thermal_Limits.csv")
IUCN_binom <- unique(IUCN_Names$Binomial) #gets species names for thermal limit data

missing_names <- setdiff(Data_Names,IUCN_binom)
missing_names2 <- as.data.frame(missing_names) %>% rename(Binomial = missing_names) #saves this list for manual procvessing

vroom_write(missing_names2, "E:/Coding Files/Chapter 2 Validation/Extra files for process/Missing_names_data.csv", delim = ",")

#bring in list of names that need to be removed from the datafile
remove_names <- vroom("E:/Coding Files/Chapter 2 Validation/Extra files for process/Remove_Faulty_Names.csv", delim=",")
All_Data_5 <- All_Data_4 %>% filter(!Binomial %in% remove_names$Binomial)%>%
  mutate(Binomial= ifelse(Binomial=="Pterodroma? hasitata?", "Pterodroma hasitata",
                          ifelse(Binomial=="Sporophila luctuosa?","Sporophila luctuosa",
                                  ifelse(Binomial=="Sturnella neglecta?","Sturnella neglecta",
                                         ifelse(Binomial=="Trogon comptus?","Trogon comptus",Binomial)))))
vroom_write(All_Data_5, "E:/Coding Files/Chapter 2 Validation/AFull_Dataset_Filtered_Seasons_age_sex_faultyNamesrm.csv", delim=",")

## after faulty names removed, check for faulty spelling

Sp_list <- vroom("E:/Coding Files/Chapter 2 Validation/Extra files for process/Missing_names_data_Check_Spelling.csv", delim=",")
sp <- Sp_list$Binomial
library(taxize)
Cor_spelling <- gnr_resolve(sp, data_source_ids = c(1,2,3,163), canonical = T)
Update_Binom_spelling <- Cor_spelling[,c(1,5)]
Update_Binom_spelling <- Update_Binom_spelling%>%rename(Binomial=1,Cor_Binom=2)
Update_Binom_spelling_dup <- Update_Binom_spelling[!duplicated(Update_Binom_spelling$Cor_Binom), ]
Update_Binom_spelling_dup <- Update_Binom_spelling_dup[Update_Binom_spelling_dup$Binomial != Update_Binom_spelling_dup$Cor_Binom,]
Update_Binom_spelling_dup2 <- Update_Binom_spelling_dup[grepl(" ",Update_Binom_spelling_dup$Cor_Binom),]

All_Data_6 <- left_join(All_Data_5, Update_Binom_spelling_dup2)

All_Data_6 <- All_Data_6 %>%
  mutate(Binomial=ifelse(is.na(Cor_Binom), Binomial, Cor_Binom))%>%select(-"Cor_Binom")

Missing_Binoms <- unique(All_Data_6$Binomial)
Missing_Binoms <- setdiff(Missing_Binoms,IUCN_binom)
Missing_Binoms <- as.data.frame(Missing_Binoms)
vroom_write(Missing_Binoms,"E:/Coding Files/Chapter 2 Validation/Extra files for process/Cor_spelling_still_missing.csv", delim="," )
vroom_write(All_Data_6, "E:/Coding Files/Chapter 2 Validation/Full_Dataset_Filtered_Seasons_age_sex_faultyNamesrm_corSpelling.csv", delim=",")

#### get correct Taxonomic Names

cor_binom <- read.csv("E:/Coding Files/Chapter 2 Validation/Extra files for process/Cor_spelling_still_missing.csv")

library(rredlist)
library(progress)

Master_sp_list <- cor_binom[,1]
oldlist <- c()
newlist <- data.frame(accepted_id=integer(),
                      accepted_name = character(),
                      authority = character(),
                      synonym = character(),
                      syn_authority= character())
remove_list <- Master_sp_list[c(529,531,1941)]
remove_list <- as.data.frame(remove_list)
vroom_write(remove_list, "E:/Coding Files/Chapter 2 Validation/Extra files for process/remove_list.csv",delim=",")

Master_sp_list[-c(529,531,1941)]
pbsp2 <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                          total = length(Master_sp_list),
                          complete = "=",   # Completion bar character
                          incomplete = "-", # Incomplete bar character
                          current = ">",    # Current bar character
                          clear = FALSE,    # If TRUE, clears the bar when finish
                          width = 100)
for (i in 1:length(Master_sp_list)){
  pbsp2$tick()
  
  rr <- rl_synonyms(Master_sp_list[i] , key='ec423a9dfc40f6d5e7d11fc59fb5c817974e78390b9a3e7ca379ccc642e3029c')
  
  if (rr$count > 0){
    newlist[nrow(newlist)+1,] = rr$result[1,]
    oldlist <- c(oldlist, rr$name)
  } else {
    newlist[nrow(newlist)+1,] <- c("NA","NA", "NA", "NA", "NA")
    oldlist <- c(oldlist, rr$name)
  }
}

Sp_list_Full <- cbind(newlist,oldlist)
Sp_list_Full <- Sp_list_Full %>% mutate(IUCN = ifelse(oldlist==accepted_name,synonym,
                                                      ifelse(oldlist==synonym, accepted_name, NA)))%>%
  select(oldlist,IUCN)%>%
  rename(Binomial=oldlist)

Matches <- Sp_list_Full %>% filter(!is.na(IUCN))
matches_iucncheck <- Matches$IUCN
check3 <- setdiff(matches_iucncheck, IUCN_binom)

Update_Master <- Matches%>%filter(!IUCN %in% check3)

vroom_write(Update_Master, "E:/Coding Files/Chapter 2 Validation/Extra files for process/Master_Update_List",delim=",")

Missing_still <- Sp_list_Full %>% filter(is.na(IUCN))%>%select(Binomial)
missing_in_iucn <- Matches%>%filter(IUCN %in% check3)%>%select(Binomial)
Missing_still <- rbind(Missing_still,missing_in_iucn)

cor_binom2 <- Missing_still$Binomial

### get TSN number for name

syn <- taxize::get_tsn(cor_binom2)

Missing_still$TSN_Number <- syn

no_tsn <- Missing_still %>% filter(is.na(TSN_Number))
with_tsn <- Missing_still %>% filter(!is.na(TSN_Number))
### get accepted name for TSN number
DF <- data.frame(TSN_Number=character(),Syn_name=character())
syn2 <- as.numeric(with_tsn$TSN_Number)

for (i in 1:length(syn2)){
  sn <- taxize::synonyms(syn2[i], db="itis")
  snn <- as.data.frame(sn[1])
  if (ncol(snn)==7){
    sn1 <- snn[,c(1,6)]
    sn2 <- snn[,c(1,2)]
    
    names(sn1)[1]<-"TSN_Number"
    names(sn1)[2]<-"Syn_name"
    names(sn2)[1]<-"TSN_Number"
    names(sn2)[2]<-"Syn_name"
    
    comb_names <- rbind(sn1,sn2)
    DF <- rbind(DF,comb_names)
  }else if(ncol(snn)==5){
    sn1 <- snn[,c(1,4)]
    names(sn1)[1]<-"TSN_Number"
    names(sn1)[2]<-"Syn_name"
    DF <- rbind(DF,sn1)
  }else{
    next
  }
}
with_tsn$TSN_Number <- as.character(with_tsn$TSN_Number)
Update_list_TSN <- left_join(DF,with_tsn)[,c(2,3)]
iucn_tsn_list <- Update_list_TSN$Syn_name
check4 <- setdiff(iucn_tsn_list,IUCN_binom)
Update_list_TSN2 <- Update_list_TSN%>%filter(!Syn_name %in% check4)
Update_list_TSN2 <- unique(Update_list_TSN2)%>% rename(IUCN = Syn_name)%>%
  relocate(IUCN, .after = Binomial)

Update_Master <- rbind(Update_Master, Update_list_TSN2)
um_binom <- Update_Master$Binomial
manual_check <- setdiff(Master_sp_list,um_binom)

manual_list <- vroom("E:/Coding Files/Chapter 2 Validation/Extra files for process/Manual_sp_list.csv",delim=",")

manual_add <- manual_list %>% filter(Binomial %in% manual_check)

Update_Master <- rbind(Update_Master, manual_add)

#add correct spelling, tsn, and Updated binomial to the original list

vroom_write(Update_Master, "E:/Coding Files/Chapter 2 Validation/Extra files for process/Master_Update_List.csv",delim=",")

###Update original datafile

All_Data <- vroom("E:/Coding Files/Chapter 2 Validation/Full_Dataset_Filtered_Seasons_age_sex_faultyNamesrm_corSpelling.csv", delim=",")

All_Data_updatenames <- left_join(All_Data, Update_Master)

All_Data_updatenames <- All_Data_updatenames%>% mutate(Binomial=ifelse(is.na(IUCN), Binomial, IUCN))%>%
  select(-IUCN)

vroom_write(All_Data_updatenames, "E:/Coding Files/Chapter 2 Validation/Final_Master_Data.csv", delim=",")
#####################################################################################################
#Add thermal Limits then remove any species without data

Tlimits <- vroom("E:/Coding Files/Chapter 2 Validation/Extra files for process/All_Species_Thermal_Limits.csv")
Tlimits <- Tlimits[,2:5]
t <- Tlimits %>% filter(Month=="NONE") %>% rename(YR_TMin = TMin, YR_TMax=TMax)%>%select(-Month)
Final_Data <- vroom("E:/Coding Files/Chapter 2 Validation/Final_Master_Data.csv", delim=",")

Final_Data <- All_Data_updatenames %>% left_join(Tlimits) %>% left_join(t) %>% 
  filter(!is.na(YR_TMax))%>%filter(!is.na(YR_TMin))

vroom_write(Final_Data, "E:/Coding Files/Chapter 2 Validation/Final_Master_Data_Tlims.csv")

#### sort by body measurement####

Adult_Mass <- Final_Data %>% filter(!is.na(Mass))%>%filter(Age=="Adult")%>%mutate(logmass = log(Mass))%>%
  group_by(Binomial)%>% filter(n()>1)%>%summarise(Median_Mass = median(logmass),
                              MAD_Mass = mad(logmass))
Mass <- Final_Data %>% filter(!is.na(Mass))%>%mutate(logmass = log(Mass))
Mass2 <- left_join(Mass,Adult_Mass)
Mass3 <- Mass2 %>% filter(!is.na(Median_Mass))%>%
  filter(logmass > Median_Mass-(5*MAD_Mass)&logmass<Median_Mass+(5*MAD_Mass))%>%
  group_by(Binomial)%>%filter(n()>99)
vroom_write(Mass3, "E:/Coding Files/Chapter 2 Validation/Master_Mass.csv", delim=",")

Adult_Length <- Final_Data %>%filter(!is.na(Body_Length))%>%filter(Age=="Adult")%>%mutate(loglength= log(Body_Length))%>%
  group_by(Binomial)%>% filter(n()>1)%>%summarise(Median_Length = median(loglength),
                                                  MAD_Length = mad(loglength))
Length1 <- Final_Data %>% filter(!is.na(Body_Length))%>%mutate(loglength = log(Body_Length))
Length2 <- left_join(Length1,Adult_Length)
Length3 <- Length2 %>% filter(!is.na(Median_Length))%>%
  filter(loglength > Median_Length-(5*MAD_Length)&loglength<Median_Length+(5*MAD_Length))%>%
  group_by(Binomial)%>%filter(n()>99)
vroom_write(Length3, "E:/Coding Files/Chapter 2 Validation/Master_Length.csv", delim=",")

Mass4 <- Mass3 %>% select(-c("logmass","Median_Mass","MAD_Mass"))
Length4 <- Length3 %>% select(-c("loglength","Median_Length","MAD_Length"))
Mass_Length <- inner_join(Mass4,Length4) %>% mutate(Size_Ratio = (log(Mass))/(log(Body_Length)))
vroom_write(Mass_Length, "E:/Coding Files/Chapter 2 Validation/Master_Length.csv", delim=",")
