#' ---
#' step 4: Extract Temperature and Aridity limits
#' Matthew Watson
#' This code will extract monthly Thermal limits using worldclim data and IUCN range maps
#' Additionally a yearly average upper and lower limit will be calculated
#' due to the different range maps for birds ensure that the proper section is used
#' ---

library(terra)
library(progress)
library(dplyr)

####TPI for mammals #####
## change sp_list to the file location with the target species range maps

sp_list <- list.files("C:/Coding Files/Species Maps", pattern = "\\.shp$", full.names = T)

DF <- data.frame(Binomial= character(),
                 TMin = numeric(),
                 TMax = numeric(),
                 Month = character())

Maxpath <- "C:/Coding Files/Climate Data/MaxBase"
Minpath <- "C:/Coding Files/Climate Data/Min"

mnth <- list("_01.tif", "_02.tif", "_03.tif", "_04.tif", "_05.tif", "_06.tif",
             "_07.tif", "_08.tif", "_09.tif","_10.tif", "_11.tif", "_12.tif")

m <- list("01", "02", "03", "04", "05", "06","07", "08", "09","10", "11", "12")

###Extracts Monthly Thermal Limits

for(i in 1:length(mnth)){
  rmax <- rast(list.files(Maxpath, pattern = mnth[[i]], full.names = T))
  rmin <- rast(list.files(Minpath, pattern = mnth[[i]], full.names = T))
  pbsp <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                           total = length(sp_list),
                           complete = "=",   # Completion bar character
                           incomplete = "-", # Incomplete bar character
                           current = ">",    # Current bar character
                           clear = FALSE,    # If TRUE, clears the bar when finish
                           width = 100)
  for (sp in sp_list){
    pbsp$tick()
    polyg <- vect(sp)
    maxmean <- as.data.frame(extract(rmax, polyg, fun=max, na.rm=T))
    maxmean <- t(maxmean[is.finite(rowSums(maxmean)),])
    if (ncol(maxmean) < 2){
      mxm <- mean(maxmean[-1,])
    } else {
      mxm <- mean(colMeans(t(maxmean[-1,])))
    }
    minmean <- as.data.frame(extract(rmin, polyg, fun=min, na.rm=T))
    minmean <- t(minmean[is.finite(rowSums(minmean)),])
    if (ncol(minmean) < 2){
      mnm <- mean(minmean[-1,])
    } else {
      mnm <- mean(colMeans(t(minmean[-1,])))
    }
    allspecies <- basename(sp)
    sp_sub <- sub("_", " ", allspecies)
    bn <- sub(".shp", "", sp_sub) 
    DF[nrow(DF)+1,] = c(bn,mnm,mxm,m[[i]])
    rm(polyg, maskmax, maskmin, maxv,minv, meanmax, meanmin, allspecies, sp_sub, bn,mnm,mxm)
    gc()
  }
  rm(pbsp)
  gc()
}

## Save file ordered by species

DF_ordered <- DF[with(DF, order(Binomial, Month)), ]
write.csv(DF, "D:/Matt/Thesis Files/Universal Maps and Files/Min and Max Temp Limits/Baseline_TPI_Temperautre_Limits_Mammals.csv")

###Creates row for each species for yearly average upper and lower limit

DF_ordered$TMin <- as.numeric(DF_ordered$TMin)
DF_ordered$TMax <- as.numeric(DF_ordered$TMax)
ag <- aggregate(DF_ordered[, 2:3], list(DF_ordered$Binomial), mean)
ag<- ag %>% mutate(Month="NONE")%>% rename(Binomial = Group.1)
final <- rbind(DF_ordered,ag)
final <- final[with(final, order(Binomial, Month)),]

write.csv(final, "D:/Matt/Thesis Files/Universal Maps and Files/Min and Max Temp Limits/Baseline_TPI_Temperautre_Limits_Mammals_with_Average.csv")

#####TPI for BIRDS#####

library(rgdal)
library(terra)
library(progress)
library(dplyr)

####TPI NORTHERN#####
#Months are set to norhtern hemisphere seasons (Dec-Feb winter, Mar-May spring, June-Aug summer, Sep-Nov fall) to match range map extents

sp_list_N <- list("D:/Shapefiles/Birds/Northern/Breeding",
                 "D:/Shapefiles/Birds/Northern/Non Breeding",
                 "D:/Shapefiles/Birds/Northern/Migration Non Breeding",
                 "D:/Shapefiles/Birds/Northern/Migration Breeding")

DFN <- data.frame(Binomial= character(),
                 TMin = numeric(),
                 TMax = numeric(),
                 Month = character())

Maxpath <- "C:/Coding Files/Climate Data/MaxBase"
Minpath <- "C:/Coding Files/Climate Data/Min"

#month list Norther species
N_B_mnth <- list("_05.tif", "_06.tif","_07.tif", "_08.tif", "_09.tif")

N_NB_mnth <-list("_01.tif", "_02.tif","_12.tif")

N_NBM_mnth <-list("_03.tif","_11.tif")

N_BM_mnth <-list("_04.tif","_10.tif")

monthslistN <- (N_B_mnth, N_NB_mnth, N_NBM_mnth, N_BM_mnth)
cn=1

while (cn < 5){
  mn <- monthlistN[[cn]]
  splist <- list.files(sp_list_N[[cn]], pattern = "\\.shp$", full.name=T)
  for(i in 1:length(mn)){
    rmax <- rast(list.files(Maxpath, pattern = mn[[i]], full.names = T))
    rmin <- rast(list.files(Minpath, pattern = mn[[i]], full.names = T))
    pbsp <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                            total = length(sp_list),
                            complete = "=",   # Completion bar character
                            incomplete = "-", # Incomplete bar character
                            current = ">",    # Current bar character
                            clear = FALSE,    # If TRUE, clears the bar when finish
                            width = 100)
    for (sp in splist){
      pbsp$tick()
      polyg <- vect(sp)
      maxmean <- as.data.frame(extract(rmax, polyg, fun=max, na.rm=T))
      maxmean <- t(maxmean[!is.infinite(rowSums(maxmean)),])
      if (ncol(maxmean) < 2){
        mxm <- mean(maxmean[-1,])
      } else {
        mxm <- mean(colMeans(t(maxmean[-1,])))
      }
      minmean <- as.data.frame(extract(rmin, polyg, fun=min, na.rm=T))
      minmean <- t(minmean[!is.infinite(rowSums(minmean)),])
      if (ncol(minmean) < 2){
        mnm <- mean(minmean[-1,])
      } else {
        mnm <- mean(colMeans(t(minmean[-1,])))
      }
      allspecies <- basename(sp)
      sp_sub <- sub("_", " ", allspecies)
      bn <- sub(".shp", "", sp_sub)
      m <- sub("_","",mn[[i]])
      m <- sub(".tif","", m)
      DFN[nrow(DFN)+1,] = c(bn,mnm,mxm,m)
      rm(polyg, maskmax, maskmin, maxv,minv, meanmax, meanmin, allspecies, sp_sub, bn,mnm,mxm)
      gc()
    }
    rm(pbsp)
    gc()
  }
  cn=cn+1
}  

DFN_ordered <- DFN[with(DFN, order(Binomial, Month)), ]
write.csv(DFN_ordered, "D:/Matt/Thesis Files/Universal Maps and Files/Min and Max Temp Limits/Baseline_TPI_Temperautre_Limits_Birds_north.csv")
DFN_ordered$TMin <- as.numeric(DFN_ordered$TMin)
DFN_ordered$TMax <- as.numeric(DFN_ordered$TMax)

ag <- aggregate(DFN_ordered[, 2:3], list(DFN_ordered$Binomial), mean)
ag<- ag %>% mutate(Month="NONE")%>% rename(Binomial = Group.1)
final <- rbind(DFN_ordered,ag)
final <- final[with(final, order(Binomial, Month)),]

write.csv(final, "D:/Matt/Thesis Files/Universal Maps and Files/Min and Max Temp Limits/Baseline_TPI_Temperautre_Limits_Birds_North_with_Average.csv")

#month list Southern species
#Months are set to southern hemisphere seasons (Dec-Feb summer, Mar-May fall, June-Aug winter, Sep-Nov spring) to match range map extents

sp_list_S <- list("D:/Shapefiles/Birds/Southern/Breeding",
                 "D:/Shapefiles/Birds/Southern/Non Breeding",
                 "D:/Shapefiles/Birds/Southern/Migration Non Breeding",
                 "D:/Shapefiles/Birds/Southern/Migration Breeding")

S_B_mnth <- list("_11.tif", "_12.tif","_01.tif", "_02.tif", "_03.tif")

S_NB_mnth <-list("_06.tif", "_07.tif","_08.tif")

S_NBM_mnth <-list("_05.tif","_09.tif")

S_BM_mnth <-list("_04.tif","_10.tif")

DFS <- data.frame(Binomial= character(),
                 TMin = numeric(),
                 TMax = numeric(),
                 Month = character())

monthslistS <- (S_B_mnth, S_NB_mnth, S_NBM_mnth, S_BM_mnth)
cn=1

while (cn < 5){
  mn <- monthlistS[[cn]]
  splist <- list.files(sp_list_S[[cn]], pattern = "\\.shp$", full.name=T)
  for(i in 1:length(mn)){
    rmax <- rast(list.files(Maxpath, pattern = mn[[i]], full.names = T))
    rmin <- rast(list.files(Minpath, pattern = mn[[i]], full.names = T))
    pbsp <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                            total = length(sp_list),
                            complete = "=",   # Completion bar character
                            incomplete = "-", # Incomplete bar character
                            current = ">",    # Current bar character
                            clear = FALSE,    # If TRUE, clears the bar when finish
                            width = 100)
    for (sp in splist){
      pbsp$tick()
      polyg <- vect(sp)
      maxmean <- as.data.frame(extract(rmax, polyg, fun=max, na.rm=T))
      maxmean <- t(maxmean[!is.infinite(rowSums(maxmean)),])
      if (ncol(maxmean) < 2){
        mxm <- mean(maxmean[-1,])
      } else {
        mxm <- mean(colMeans(t(maxmean[-1,])))
      }
      minmean <- as.data.frame(extract(rmin, polyg, fun=min, na.rm=T))
      minmean <- t(minmean[!is.infinite(rowSums(minmean)),])
      if (ncol(minmean) < 2){
        mnm <- mean(minmean[-1,])
      } else {
        mnm <- mean(colMeans(t(minmean[-1,])))
      }
      allspecies <- basename(sp)
      sp_sub <- sub("_", " ", allspecies)
      bn <- sub(".shp", "", sp_sub)
      m <- sub("_","",mn[[i]])
      m <- sub(".tif","", m)
      DFS[nrow(DFS)+1,] = c(bn,mnm,mxm,m)
      rm(polyg, maskmax, maskmin, maxv,minv, meanmax, meanmin, allspecies, sp_sub, bn,mnm,mxm)
      gc()
    }
    rm(pbsp)
    gc()
  }
  cn=cn+1
}  

DFS_ordered <- DFS[with(DFS, order(Binomial, Month)), ]
write.csv(DFS_ordered, "D:/Matt/Thesis Files/Universal Maps and Files/Min and Max Temp Limits/Baseline_TPI_Temperautre_Limits_Birds_south.csv")
DFS_ordered$TMin <- as.numeric(DFS_ordered$TMin)
DFS_ordered$TMax <- as.numeric(DFS_ordered$TMax)

ags <- aggregate(DFS_ordered[, 2:3], list(DFS_ordered$Binomial), mean)
ags<- ags %>% mutate(Month="NONE")%>% rename(Binomial = Group.1)
finals <- rbind(DFS_ordered,ags)
finals <- final[with(final, order(Binomial, Month)),]

write.csv(finals, "D:/Matt/Thesis Files/Universal Maps and Files/Min and Max Temp Limits/Baseline_TPI_Temperautre_Limits_Birds_South_with_Average.csv")

