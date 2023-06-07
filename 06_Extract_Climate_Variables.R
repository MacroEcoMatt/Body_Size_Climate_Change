#' ---
#' Step 6: Get Environmental Variables
#' Matthew Watson
#' collects environmental variables from datfiles using species coordinate location
#' ---
library(vroom)
library(terra)
library(sp)
library(dplyr)
library(progress)
library(DescTools)

max_temp_files_m <- "E:/Environmental Files/Temperature/TMax/"
max_temp_files_y <- "E:/Environmental Files/Temperature/TMax/Yearly Average/"
min_temp_files_m <- "E:/Environmental Files/Temperature/TMin current/"
min_temp_files_y <- "E:/Environmental Files/Temperature/TMin current/Yearly Average/"
mean_temp_files_m <- "E:/Environmental Files/Temperature/Tmean/"
mean_temp_files_y <- "E:/Environmental Files/Temperature/Tmean/Yearly Average/"
arid_files <- "E:/Environmental Files/Aridity/"
HLU <- "D:/Matt/Thesis Files/Universal Maps and Files/Historical Land Use/Human Land Use Percent/"
habitat <- rast("E:/Environmental Files/Habitat/BIOMES.tif")

Bodyfiles <- vroom("C:/Users/mwats041/OneDrive/Chapter 2 results/verify code/Master_Size_Ratio.csv", delim=",") #change for each body metric (Mass,Length,Mass:Length)
Bodyfiles$Month <- as.numeric(Bodyfiles$Month)
Bodyfiles <- Bodyfiles %>% filter(Month < 13)
Bodyfiles$Month <- as.character(Bodyfiles$Month)
datafile <- Bodyfiles
datafile$Month <- stringr::str_pad(Bodyfiles$Month, 2, pad="0")

pbsp <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                         total = nrow(datafile),
                         complete = "=",   # Completion bar character
                         incomplete = "-", # Incomplete bar character
                         current = ">",    # Current bar character
                         clear = FALSE,    # If TRUE, clears the bar when finish
                         width = 100)

for (row in 1:nrow(datafile)){
  pbsp$tick()  
  yr <- datafile[row, "Year"]
  
  if (!is.na(datafile[row, "Month"])){
    
    mn <- datafile[row, "Month"]
    mt <- datafile[row, "Mnth"]
    
    tmax<- paste0(max_temp_files_m,"tmax_", yr,"_", mn,".tif")
    tmin <- paste0(min_temp_files_m,"tmin_", yr,"_", mn,".tif")
    tmean <- paste0(mean_temp_files_m,"tmp_", yr,"__", mn,".tif")
    tx <- rast(tmax)
    tn <- rast(tmin)
    tp <- rast(tmean)
    
    aridname <- paste0(arid_files, yr,"/", "Aritity_",mt,"_",yr,".tif")
    arid <- rast(aridname)
    
  } else {
    
    break
    
  }
  
  spdfv <- vect(datafile[row,], geom=c("Lon", "Lat"), 
                crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  tmax2 <- paste0(max_temp_files_y,"tmax_", yr,".tif")
  tmin2 <- paste0(min_temp_files_y,"tmin_", yr,".tif")
  tmean2 <- paste0(mean_temp_files_y,"tmp_", yr,".tif")
  tx2 <- rast(tmax2)
  tn2 <- rast(tmin2)
  tp2 <- rast(tmean2)
  
  aridname2 <- paste0(arid_files,"Yearly Average/","Aridity_",yr,".tif")
  arid2 <- rast(aridname2)
  
  HLUname <- paste0(HLU, "HLU_Perc_",yr,".tif")
  HLUyear <- rast(HLUname)
  
  tmxvalue2 <- (as.data.frame(extract(tx2, spdfv)))[,-1]
  tmnvalue2 <- (as.data.frame(extract(tn2, spdfv)))[,-1]
  tmpvalue2 <- (as.data.frame(extract(tp2, spdfv)))[,-1]
  aridvalue2 <- (as.data.frame(extract(arid2, spdfv)))[,-1]
  tmxvalue <- (as.data.frame(extract(tx, spdfv)))[,-1]
  tmnvalue <- (as.data.frame(extract(tn, spdfv)))[,-1]
  tmpvalue <- (as.data.frame(extract(tp, spdfv)))[,-1]
  aridvalue <- (as.data.frame(extract(arid, spdfv)))[,-1]
  habvalue <- (as.data.frame(extract(habitat, spdfv)))[,-1]
  HLUvalue <- (as.data.frame(extract(HLUyear, spdfv)))[,-1]
  
  datafile[row, "Tx_max"] <- tmxvalue
  datafile[row, "Tx_min"] <- tmnvalue
  datafile[row, "Tx_mean"] <- tmpvalue
  datafile[row, "YR_Tx_max"] <- tmxvalue2
  datafile[row, "YR_Tx_min"] <- tmnvalue2
  datafile[row, "YR_Tx_mean"] <- tmpvalue2
  datafile[row, "Aridity"] <- aridvalue
  datafile[row, "YR_Aridity"] <- aridvalue2
  datafile[row, "Habitat"] <- habvalue
  datafile[row, "HLU"] <- HLUvalue    
}

DF <- datafile %>% 
  mutate(TPI_month_min = ((Tx_min-TMin)/(TMax-TMin))) %>%
  mutate(TPI_month_mean = ((Tx_mean-TMin)/(TMax-TMin))) %>%
  mutate(TPI_month_max = ((Tx_max-TMin)/(TMax-TMin))) %>%
  mutate(TPI_month_range = TPI_month_max-TPI_month_min)%>%
  mutate(TPI_yr_min = ((YR_Tx_min-YR_TMin)/(YR_TMax-YR_TMin))) %>%
  mutate(TPI_yr_mean = ((YR_Tx_mean-YR_TMin)/(YR_TMax-YR_TMin))) %>%
  mutate(TPI_yr_max = ((YR_Tx_max-YR_TMin)/(YR_TMax-YR_TMin)))%>%
  mutate(TPI_yr_range = TPI_yr_max-TPI_yr_min)%>%
  mutate(Aridity = ifelse(Aridity>100,100,Aridity))%>% 
  group_by(Binomial) %>%
  mutate(Year_sc = Year - mean(Year))

#change mutate LMass to LLength for length file, and LSize for mass:length file
DF <- DF %>% mutate(LMass = log10(Mass)
                    
vroom_write(DF, "E:/Final_Master_Bodysize.csv") #change for each body metric
                    
