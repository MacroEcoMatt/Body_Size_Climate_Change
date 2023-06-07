#' ---
#' step 3: generate maps of human land use
#' Matthew Watson
#' data is downloaded from LUH2-GCB2019
#' only change needed in out <- location and all_clim <- location
#' ---
library(ncdf4)
library(raster)
library(rgdal)
library(filenamer)

out <- "D:/Matt/Thesis Files/Universal Maps and Files/Historical Land Use/New Version/" #Folder path to save location

all_clim <- nc_open("D:/Matt/Thesis Files/Universal Maps and Files/Historical Land Use/New Version/RAW/LUH2_GCB2019_states.nc4") #call in NC files

lon <- ncvar_get(all_clim, "lon")
lat <- ncvar_get(all_clim, "lat", verbose = F)
time <-ncvar_get(all_clim, "time")

##Creates Percent Land use maps for each year 1961-2018 for each land use type

#urban maps:
urb <- ncvar_get(all_clim, "urban")
fillurb <- ncatt_get(all_clim, "urban", "_FillValue")
urb[urb == fillurb$value] <- NA
urbs <- urb[1112:1169, ,]
for (i in 1:length(urbs)){
  urbanrast <- flip((raster(t(urbs[i,,]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
                            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))),direction='y')
  outname = paste0(out,"Urban/","Urban_",i,".tif")
  writeRaster(urbanrast, outname)
}

# crop maps:

c3a<- ncvar_get(all_clim, varid="c3ann")
fillc3a <- ncatt_get(all_clim, "c3ann", "_FillValue")
c3a[c3a == fillc3a$value] <- NA
c3aa <- c3a[1112:1169, ,]
for (i in 1:length(c3aa)){
  pastrast <- flip((raster(t(c3aa[i,,]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
                           crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))),direction='y')
  outname = paste0(out,"Crops/","C3a_",i,".tif")
  writeRaster(pastrast, outname)
}

c3n<- ncvar_get(all_clim, varid="c3nfx")
fillc3n <- ncatt_get(all_clim, "c3nfx", "_FillValue")
c3n[c3n == fillc3n$value] <- NA
c3nn <- c3n[1112:1169, ,]
for (i in 1:length(c3nn)){
  pastrast <- flip((raster(t(c3nn[i,,]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
                           crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))),direction='y')
  outname = paste0(out,"Crops/","C3n_",i,".tif")
  writeRaster(pastrast, outname)
}

c3p<- ncvar_get(all_clim, varid="c3per")
fillc3p <- ncatt_get(all_clim, "c3per", "_FillValue")
c3p[c3p == fillc3p$value] <- NA
c3pp <- c3p[1112:1169, ,]
for (i in 1:length(c3pp)){
  pastrast <- flip((raster(t(c3pp[i,,]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
                           crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))),direction='y')
  outname = paste0(out,"Crops/","C3p_",i,".tif")
  writeRaster(pastrast, outname)
}

c4a<- ncvar_get(all_clim, varid="c4ann")
fillc4a <- ncatt_get(all_clim, "c4ann", "_FillValue")
c4a[c4a == fillc4a$value] <- NA
c4aa <- c4a[1112:1169, ,]
for (i in 1:length(c4aa)){
  pastrast <- flip((raster(t(c4aa[i,,]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
                           crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))),direction='y')
  outname = paste0(out,"Crops/","C4a_",i,".tif")
  writeRaster(pastrast, outname)
}

c4p<- ncvar_get(all_clim, varid="c4per")
fillc4p <- ncatt_get(all_clim, "c4per", "_FillValue")
c4p[c4p == fillc4p$value] <- NA
c4pp <- c4p[1112:1169, ,]
for (i in 1:length(c4pp)){
  pastrast <- flip((raster(t(c4pp[i,,]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
                           crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))),direction='y')
  outname = paste0(out,"Crops/","C4p_",i,".tif")
  writeRaster(pastrast, outname)
}

#pasturemaps: gets variables, replaces empty values, selects years of interest, extracts maps

past<- ncvar_get(all_clim, varid="pastr")
fillpast <- ncatt_get(all_clim, "pastr", "_FillValue")
past[past == fillpast$value] <- NA
pastrr <- past[1112:1169, ,]
for (i in 1:length(pastrr)){
  pastrast <- flip((raster(t(pastrr[i,,]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
                            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))),direction='y')
  outname = paste0(out,"Pasture/","Pasture_",i,".tif")
  writeRaster(pastrast, outname)
}


c <- list.files("D:/Matt/Thesis Files/Universal Maps and Files/Historical Land Use/New Version/Crops", patter=".tif", full.names=T)
p <- list.files("D:/Matt/Thesis Files/Universal Maps and Files/Historical Land Use/New Version/Pasture", patter=".tif", full.names=T)
u <- list.files("D:/Matt/Thesis Files/Universal Maps and Files/Historical Land Use/New Version/Urban", patter=".tif", full.names=T)


### TOTALS land use percentage for each year for all land use types

z<-1
y<-59
x<- 117
w<- 175
v<- 233
year <- 1961
while (z<59){
  crop <- raster(c[z])
  crop2 <- raster(c[y])
  crop3 <-  raster(c[x])
  crop4 <-  raster(c[w])
  crop5 <- raster(c[v])
  past <- raster(p[z])
  urban <- raster(u[z])
  Human <- overlay(crop,crop2,crop3,crop4,crop5,past,urban, fun=sum, na.rm=TRUE)
  namefinal <- paste0("D:/Matt/Thesis Files/Universal Maps and Files/Historical Land Use/Human Land Use Percent/","HLU_Perc_",year,".tif")
  writeRaster(Human, namefinal)
  z=z+1
  y=y+1
  x=x+1
  w=w+1
  v=v+1
  year=year+1
}

