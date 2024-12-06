library(vroom)
library(dplyr)
library(sjmisc)
library(sjPlot)
library(nlme)
library(corrplot)
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggthemes)
library(performance)
library(DHARMa)
library(stringr)
library(ape)
library(phyr)

##

#DATA####
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
#Phylogeny####
mammaltree <- read.tree("F:/Phylogenetic Trees/mammalTree.newick")

oldname_m <- mammaltree$tip.label
newname_m <- mammaltree$tip.label
for (n in 1:length(newname_m)){
  newname_m[n] <- sub("([A-Za-z]+_[A-Za-z]+).*", "\\1", newname_m[n])
  newname_m[n] <- sub("_"," ", newname_m[n])
}
DF_names_m <- as.data.frame(cbind(oldname_m,newname_m))
mammaltree$tip.label<-DF_names_m[[2]][match(mammaltree$tip.label, DF_names_m[[1]])]

binom_mass <- unique(M_Mass$Binomial)
mam_tree_mass <-drop.tip(mammaltree, mammaltree$tip.label[-na.omit(match(binom_mass,mammaltree$tip.label))])
mt <- unique(mam_tree_mass$tip.label)
mamdif <- setdiff(binom_mass,mt)
M_Mass_tree <- M_Mass %>% filter(!Binomial %in% mamdif)

binom_length <- unique(M_Length$Binomial)
mam_tree_length <-drop.tip(mammaltree, mammaltree$tip.label[-na.omit(match(binom_length,mammaltree$tip.label))])
ml <- unique(mam_tree_length$tip.label)
mamdif_l <- setdiff(binom_length,ml)

M_Length$Binomial[M_Length$Binomial=="Neotamias amoenus"] <- "Tamias amoenus"
M_Length$Binomial[M_Length$Binomial=="Neotamias siskiyou"] <- "Tamias siskiyou"
M_Length$Binomial[M_Length$Binomial=="Neotamias minimus"] <- "Tamias minimus"
M_Length$Binomial[M_Length$Binomial=="Neotamias dorsalis"] <- "Tamias dorsalis"
M_Length$Binomial[M_Length$Binomial=="Neotamias speciosus"] <- "Tamias speciosus"
M_Length$Binomial[M_Length$Binomial=="Neotamias quadrivittatus"] <- "Tamias quadrivittatus"
M_Length$Binomial[M_Length$Binomial=="Neotamias senex"] <- "Tamias senex"
M_Length$Binomial[M_Length$Binomial=="Neotamias townsendii"] <- "Tamias townsendii"
M_Length$Binomial[M_Length$Binomial=="Neotamias umbrinus"] <- "Tamias umbrinus"
M_Length_tree <- M_Length %>% filter(Binomial %in% ml)

####Birds##
Birdtree <- read.tree("F:/Phylogenetic Trees/FinalBirdTree_analysis.tre")

oldname_m <- Birdtree$tip.label
newname_m <- Birdtree$tip.label
for (n in 1:length(newname_m)){
  newname_m[n] <- sub("([A-Za-z]+_[A-Za-z]+).*", "\\1", newname_m[n])
  newname_m[n] <- sub("_"," ", newname_m[n])
}
DF_names_m <- as.data.frame(cbind(oldname_m,newname_m))
Birdtree$tip.label<-DF_names_m[[2]][match(Birdtree$tip.label, DF_names_m[[1]])]

m100 <- unique(B_Mass$Binomial)
l100 <- unique(B_Length$Binomial)

bird_tree_mass <-drop.tip(Birdtree, Birdtree$tip.label[-na.omit(match(m100,Birdtree$tip.label))])
bird_tree_length <-drop.tip(Birdtree, Birdtree$tip.label[-na.omit(match(l100,Birdtree$tip.label))])

treem <- unique(bird_tree_mass$tip.label)
treel <- unique(bird_tree_length$tip.label)

diff_m <- setdiff(m100,treem)
diff_l<- setdiff(l100,treel)

B_Mass_tree <- B_Mass %>% filter(Binomial %in% treem)
B_Length_tree <- B_Length %>% filter(Binomial %in% treel)

#2 slopes and random terms and phylogeny####
#Mammal Mass

M_Mass_tree2 <- M_Mass_tree%>%filter(is.finite(LMass))%>%
  mutate(TPI_Max= ((TPI_Max - mean(TPI_Max,na.rm=T))/sd(TPI_Max, na.rm=T)),
                                     API= ((API - mean(API,na.rm=T))/sd(API, na.rm=T)),
                                     HLU= ((HLU - mean(HLU,na.rm=T))/sd(HLU, na.rm=T)),
                                     Year = ((Year - mean(Year,na.rm=T))/sd(Year, na.rm=T)),
                                     LMass = ((LMass - mean(LMass,na.rm=T))/sd(LMass, na.rm=T)))

phylo_m <- pglmm(LMass ~ TPI_Max + API + HLU
                 + TPI_Max:API + TPI_Max:HLU+ Year
                 + (TPI_Max|Binomial__)
                 + (API|Binomial__)
                 + (HLU|Binomial__)
                 + (Year|Binomial__)
                 + (1|Binomial__)
                 +  (1|Realm) + (1|Site), 
                 data=M_Mass_tree2, cov_ranef = list(Binomial = mam_tree_mass), 
                 bayes = TRUE)

M_Length_tree2 <- M_Length_tree%>%filter(is.finite(LLength))%>%
  mutate(TPI_Max= ((TPI_Max - mean(TPI_Max,na.rm=T))/sd(TPI_Max, na.rm=T)),
                                         API= ((API - mean(API,na.rm=T))/sd(API, na.rm=T)),
                                         HLU= ((HLU - mean(HLU,na.rm=T))/sd(HLU, na.rm=T)),
                                         Year = ((Year - mean(Year,na.rm=T))/sd(Year, na.rm=T)),
                                         LLength= ((LLength - mean(LLength,na.rm=T))/sd(LLength, na.rm=T)))%>%
  filter(!Dataset=="Neon")%>%filter(LLength < 10 & LLength > -10)

phylo_ml <- pglmm(LLength~ TPI_Max + API + HLU
                  + TPI_Max:API + TPI_Max:HLU+ Year
                  + (TPI_Max|Binomial__)
                  + (API|Binomial__)
                  + (HLU|Binomial__)
                  + (Year|Binomial__)
                  + (1|Binomial__)
                  +  (1|Realm) + (1|Site), 
                  data=M_Length_tree2, cov_ranef = list(Binomial = mam_tree_length), 
                  bayes = TRUE)

B_Mass_tree2 <- B_Mass_tree%>%filter(is.finite(LMass) & is.finite(API))%>%
  mutate(TPI_Max= ((TPI_Max - mean(TPI_Max,na.rm=T))/sd(TPI_Max, na.rm=T)),
         API= ((API - mean(API,na.rm=T))/sd(API, na.rm=T)),
         HLU= ((HLU - mean(HLU,na.rm=T))/sd(HLU, na.rm=T)),
         Year = ((Year - mean(Year,na.rm=T))/sd(Year, na.rm=T)),
         LMass= ((LMass - mean(LMass,na.rm=T))/sd(LMass, na.rm=T)),
         API_w = DescTools::Winsorize(API, val = quantile(API, probs = c(0.005, 0.995), na.rm = T)))

phylo_b <- pglmm(LMass ~ TPI_Max + API_w + HLU
                 + TPI_Max:API_w + TPI_Max:HLU+ Year
                 + (TPI_Max|Binomial__)
                 + (API_w|Binomial__)
                 + (HLU|Binomial__)
                 + (Year|Binomial__)
                 + (1|Binomial__)
                 +  (1|Realm) + (1|Site), 
                 data=B_Mass_tree2, cov_ranef = list(Binomial = bird_tree_mass), 
                 bayes = TRUE)

B_Length_tree2 <- B_Length_tree%>%filter(is.finite(LLength) & is.finite(API))%>%
  mutate(TPI_Max= ((TPI_Max - mean(TPI_Max,na.rm=T))/sd(TPI_Max, na.rm=T)),
                                         API= ((API - mean(API,na.rm=T))/sd(API, na.rm=T)),
                                         HLU= ((HLU - mean(HLU,na.rm=T))/sd(HLU, na.rm=T)),
                                         Year = ((Year - mean(Year,na.rm=T))/sd(Year, na.rm=T)),
                                         LLength= ((LLength - mean(LLength,na.rm=T))/sd(LLength, na.rm=T)))

phylo_b_l <- pglmm(LLength ~ TPI_Max + API + HLU
                   + TPI_Max:API + TPI_Max:HLU+ Year
                   + (TPI_Max|Binomial__)
                   + (API|Binomial__)
                   + (HLU|Binomial__)
                   + (Year|Binomial__)
                   + (1|Binomial__)
                   +  (1|Realm) + (1|Site), 
                   data=B_Length_tree2, cov_ranef = list(Binomial = bird_tree_length), 
                   bayes = TRUE)

#Summaries#
#Mammal Mass
###vif

mm_vif <- lmer(LMass ~ TPI_Max + API + HLU
               + TPI_Max:API + TPI_Max:HLU+ Year
               + (TPI_Max + API + HLU+Year||Binomial)
               +(1|Realm)
               +(1|Site)
               , data=M_Mass_tree2,
               control = lmerControl(optimizer = "optimx", 
                                     calc.derivs = FALSE, 
                                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
ml_vif <- lmer(LLength ~ TPI_Max + API + HLU
               + TPI_Max:API + TPI_Max:HLU+ Year
               + (TPI_Max + API + HLU+Year||Binomial)
               +(1|Realm)
               +(1|Site)
               , data=M_Length_tree2,
               control = lmerControl(optimizer = "optimx", 
                                     calc.derivs = FALSE, 
                                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

bm_vif <- lmer(LMass ~ TPI_Max + API + HLU
               + TPI_Max:API + TPI_Max:HLU+ Year
               + (TPI_Max + API + HLU+Year||Binomial)
               +(1|Realm)
               +(1|Site)
               , data=B_Mass_tree2,
               control = lmerControl(optimizer = "optimx", 
                                     calc.derivs = FALSE, 
                                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
bl_vif <- lmer(LLength ~ TPI_Max + API + HLU
               + TPI_Max:API + TPI_Max:HLU+ Year
               + (TPI_Max + API + HLU+Year||Binomial)
               +(1|Realm)
               +(1|Site)
               , data=B_Length_tree2,
               control = lmerControl(optimizer = "optimx", 
                                     calc.derivs = FALSE, 
                                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


car::vif(mm_vif)
car::vif(ml_vif)
car::vif(bm_vif)
car::vif(bl_vif)

##
rr2::R2_pred(phylo_m)

mm.sampl <- sample_n(residuals(phylo_m) %>% as.data.frame() %>% tibble::rownames_to_column(), 3000)%>%
  mutate(rowname = as.numeric(rowname))
sp.corel <- ncf::spline.correlog(x = M_Mass_tree2[mm.sampl$rowname,"Lon"]$Lon,
                                 y = M_Mass_tree2[mm.sampl$rowname,"Lat"]$Lat,                                   
                                 z = mm.sampl[,2], 
                                 resamp = 1000, latlon = T, xmax = 1000)
plot(sp.corel ,main = paste("Mammal Mass Spatial Correlation"))


M_Mass_m <- M_Mass_tree2 

M_Mass_m$Resid <- residuals(phylo_m)

ggplot(M_Mass_m, aes(x=Year, y=Resid))+geom_point(alpha=0.1, size=1)+geom_smooth(color="red")+
  theme_bw()+xlab("Year")+ylab("Residual")

D_W_df <- data.frame(NULL)

for(i in 1:1000){
  if(i %in% c(100,200,300,400,500,600,700,800,900,1000)){print(paste0(i))}
  
  bin <- M_Mass_m%>% group_by(Year)%>%slice_sample(n=1)%>%ungroup()%>%
    tidyr::complete(Year = tidyr::full_seq(1961:2018, 1))
  
  dw <- lmtest::dwtest(bin$Resid ~ bin$Year)
  
  df <- data.frame(dw_est = as.numeric(dw[[1]]),
                   dw_p = as.numeric(dw[[4]]))
  
  D_W_df <- rbind(D_W_df,df)
  
}
D_W_df <- D_W_df %>% mutate(Sig = ifelse(dw_p <=0.05,"Sig","Not Sig"))

count_sig <- D_W_df %>% group_by(Sig)%>%summarise(N=n())
vroom_write(D_W_df, "D:/Thesis Projects/Body Size Reviewer Analyses and data/final/Autocor_Table_Mammal_Mass_ALL_SEX.csv")

#individual autocor
D_W_df2 <- data.frame(NULL)
D_W_df3 <- data.frame(NULL)

for(b in unique(M_Mass_m$Binomial)){
  
  bin <-  M_Mass_m %>% filter(Binomial == b) 
  if(n_distinct(bin$Year)<2){
    next
  } else{
    sample_size <- nrow(bin)
    
    unique_years <- bin %>% summarise(N = n_distinct(Year))
    
    bin <- bin %>% group_by(Year) %>% summarise(Resid = mean(Resid,na.rm=T)) %>% 
      ungroup() %>% tidyr::complete(Year = tidyr::full_seq(1961:2018, 1))
    
    dw <- lmtest::dwtest(bin$Resid ~ bin$Year)
    
    df2 <- data.frame(Binomial = b,
                      dw_est = as.numeric(dw[[1]]),
                      dw_p = as.numeric(dw[[4]]),
                      N_Years = unique_years[1,1],
                      Sample_size = sample_size)
    
    D_W_df2 <- rbind(D_W_df2,df2)
    
    est_list<- c(NULL)
    p_list <- c(NULL)
    
    for(i in 1:100){
      
      if(i %in% c(10,20,30,40,50,60,70,80,90,100)){print(paste0(i))}
      
      bin2 <-  M_Mass_m %>% filter(Binomial == b) %>% group_by(Year)%>%slice_sample(n=1)%>%ungroup()%>%
        tidyr::complete(Year = tidyr::full_seq(1961:2018, 1))
      
      dw2 <- lmtest::dwtest(bin2$Resid ~ bin2$Year)
      
      est_list <- append(est_list,as.numeric(dw2[[1]]))
      p_list <- append(p_list,as.numeric(dw2[[4]]))
      
    }
    
    df3 <- data.frame(Binomial = b,
                      dw_est = mean(est_list, na.rm=T),
                      dw_p = mean(p_list, na.rm=T),
                      N_Years = unique_years[1,1],
                      Sample_size = sample_size)
    
    
    D_W_df3 <- rbind(D_W_df3,df3)
  }
}
sig1 <- D_W_df2 %>% filter(dw_p <=0.05) %>% arrange(desc(Sample_size))
sig2 <- D_W_df3 %>% filter(dw_p <=0.05) %>% arrange(desc(Sample_size))

vroom_write(D_W_df2, "D:/Thesis Projects/Body Size Reviewer Analyses and data/final/Autocor_Table_Mammal_Mass_Species_Mean_ALL_SEX.csv")
vroom_write(D_W_df3, "D:/Thesis Projects/Body Size Reviewer Analyses and data/final/Autocor_Table_Mammal_Mass_Species_Random_ALL_SEX.csv")

#Mammal Length
summary(phylo_ml)

rr2::R2(phylo_ml)
plot(density(resid(phylo_ml)))
plot_bayes(phylo_ml)

ml.sampl <- sample_n(residuals(phylo_ml) %>% as.data.frame() %>% tibble::rownames_to_column(), 3000)%>%
  mutate(rowname = as.numeric(rowname))
sp.corel <- ncf::spline.correlog(x = M_Length_tree2[ml.sampl$rowname,"Lon"]$Lon,
                                 y = M_Length_tree2[ml.sampl$rowname,"Lat"]$Lat,                                   
                                 z = ml.sampl[,2], 
                                 resamp = 1000, latlon = T, xmax = 1000)
plot(sp.corel ,main = paste("Mammal Length Spatial Correlation"))

M_Length_m <- M_Length_tree2

M_Length_m$Resid <- residuals(phylo_ml)

ggplot(M_Length_m, aes(x=Year, y=Resid))+geom_point(alpha=0.1, size=1)+geom_smooth(color="red")+
  theme_bw()+xlab("Year")+ylab("Residual")

D_W_df <- data.frame(NULL)

for(i in 1:1000){
  if(i %in% c(100,200,300,400,500,600,700,800,900,1000)){print(paste0(i))}
  
  bin <- M_Length_m%>% group_by(Year)%>%slice_sample(n=1)%>%ungroup()%>%
    tidyr::complete(Year = tidyr::full_seq(1961:2018, 1))
  
  dw <- lmtest::dwtest(bin$Resid ~ bin$Year)
  
  df <- data.frame(dw_est = as.numeric(dw[[1]]),
                   dw_p = as.numeric(dw[[4]]))
  
  D_W_df <- rbind(D_W_df,df)
  
}
D_W_df <- D_W_df %>% mutate(Sig = ifelse(dw_p <=0.05,"Sig","Not Sig"))

count_sig <- D_W_df %>% group_by(Sig)%>%summarise(N=n())

vroom_write(D_W_df, "D:/Thesis Projects/Body Size Reviewer Analyses and data/final/Autocor_Table_Mammal_Length_ALL_SEX.csv")

#individual autocor
D_W_df2 <- data.frame(NULL)
D_W_df3 <- data.frame(NULL)

for(b in unique(M_Length_m$Binomial)){
  
  bin <-  M_Length_m %>% filter(Binomial == b) 
  if(n_distinct(bin$Year)<2){
    next
  } else{
    sample_size <- nrow(bin)
    
    unique_years <- bin %>% summarise(N = n_distinct(Year))
    
    bin <- bin %>% group_by(Year) %>% summarise(Resid = mean(Resid,na.rm=T)) %>% 
      ungroup() %>% tidyr::complete(Year = tidyr::full_seq(1961:2018, 1))
    
    dw <- lmtest::dwtest(bin$Resid ~ bin$Year)
    
    df2 <- data.frame(Binomial = b,
                      dw_est = as.numeric(dw[[1]]),
                      dw_p = as.numeric(dw[[4]]),
                      N_Years = unique_years[1,1],
                      Sample_size = sample_size)
    
    D_W_df2 <- rbind(D_W_df2,df2)
    
    est_list<- c(NULL)
    p_list <- c(NULL)
    
    for(i in 1:100){
      
      if(i %in% c(10,20,30,40,50,60,70,80,90,100)){print(paste0(i))}
      
      bin2 <-  M_Length_m %>% filter(Binomial == b) %>% group_by(Year)%>%slice_sample(n=1)%>%ungroup()%>%
        tidyr::complete(Year = tidyr::full_seq(1961:2018, 1))
      
      dw2 <- lmtest::dwtest(bin2$Resid ~ bin2$Year)
      
      est_list <- append(est_list,as.numeric(dw2[[1]]))
      p_list <- append(p_list,as.numeric(dw2[[4]]))
      
    }
    
    df3 <- data.frame(Binomial = b,
                      dw_est = mean(est_list, na.rm=T),
                      dw_p = mean(p_list, na.rm=T),
                      N_Years = unique_years[1,1],
                      Sample_size = sample_size)
    
    
    D_W_df3 <- rbind(D_W_df3,df3)
  }
}
sig1 <- D_W_df2 %>% filter(dw_p <=0.05) %>% arrange(desc(Sample_size))
sig2 <- D_W_df3 %>% filter(dw_p <=0.05) %>% arrange(desc(Sample_size))

vroom_write(D_W_df2, "D:/Thesis Projects/Body Size Reviewer Analyses and data/final/Autocor_Table_Mammal_Length_Species_Mean_ALL_SEX.csv")
vroom_write(D_W_df3, "D:/Thesis Projects/Body Size Reviewer Analyses and data/final/Autocor_Table_Mammal_Length_Species_Random_ALL_SEX.csv")

#Birds Mass
summary(phylo_b)

rr2::R2(phylo_b)
plot(density(resid(phylo_b)))
plot_bayes(phylo_b)

bm.sampl <- sample_n(residuals(phylo_b) %>% as.data.frame() %>% tibble::rownames_to_column(), 3000)%>%
  mutate(rowname = as.numeric(rowname))
sp.corel <- ncf::spline.correlog(x = B_Mass_tree2[bm.sampl$rowname,"Lon"]$Lon,
                                 y = B_Mass_tree2[bm.sampl$rowname,"Lat"]$Lat,                                   
                                 z = bm.sampl[,2], 
                                 resamp = 1000, latlon = T, xmax = 1000)
plot(sp.corel ,main = paste("Bird Mass Spatial Correlation"))

B_Mass_m <- B_Mass_tree2
B_Mass_m$Resid <- residuals(phylo_b)

ggplot(B_Mass_m, aes(x=Year, y=Resid))+geom_point(alpha=0.1, size=1)+geom_smooth(color="red")+
  theme_bw()+xlab("Year")+ylab("Residual")

D_W_df <- data.frame(NULL)

for(i in 1:1000){
  if(i %in% c(100,200,300,400,500,600,700,800,900,1000)){print(paste0(i))}
  
  bin <- B_Mass_m%>% group_by(Year)%>%slice_sample(n=1)%>%ungroup()%>%
    tidyr::complete(Year = tidyr::full_seq(1961:2018, 1))
  
  dw <- lmtest::dwtest(bin$Resid ~ bin$Year)
  
  df <- data.frame(dw_est = as.numeric(dw[[1]]),
                   dw_p = as.numeric(dw[[4]]))
  
  D_W_df <- rbind(D_W_df,df)
  
}
D_W_df <- D_W_df %>% mutate(Sig = ifelse(dw_p <=0.05,"Sig","Not Sig"))

count_sig <- D_W_df %>% group_by(Sig)%>%summarise(N=n())
vroom_write(D_W_df, "D:/Thesis Projects/Body Size Reviewer Analyses and data/final/Autocor_Table_Bird_Mass_ALL_SEX.csv")

#individual autocor
D_W_df2 <- data.frame(NULL)
D_W_df3 <- data.frame(NULL)

for(b in unique(B_Mass_m$Binomial)){
  
  bin <-  B_Mass_m %>% filter(Binomial == b) 
  if(n_distinct(bin$Year)<2){
    next
  } else{
    sample_size <- nrow(bin)
    
    unique_years <- bin %>% summarise(N = n_distinct(Year))
    
    bin <- bin %>% group_by(Year) %>% summarise(Resid = mean(Resid,na.rm=T)) %>% 
      ungroup() %>% tidyr::complete(Year = tidyr::full_seq(1961:2018, 1))
    
    dw <- lmtest::dwtest(bin$Resid ~ bin$Year)
    
    df2 <- data.frame(Binomial = b,
                      dw_est = as.numeric(dw[[1]]),
                      dw_p = as.numeric(dw[[4]]),
                      N_Years = unique_years[1,1],
                      Sample_size = sample_size)
    
    D_W_df2 <- rbind(D_W_df2,df2)
    
    est_list<- c(NULL)
    p_list <- c(NULL)
    
    for(i in 1:100){
      
      if(i %in% c(10,20,30,40,50,60,70,80,90,100)){print(paste0(i))}
      
      bin2 <-  B_Mass_m %>% filter(Binomial == b) %>% group_by(Year)%>%slice_sample(n=1)%>%ungroup()%>%
        tidyr::complete(Year = tidyr::full_seq(1961:2018, 1))
      
      dw2 <- lmtest::dwtest(bin2$Resid ~ bin2$Year)
      
      est_list <- append(est_list,as.numeric(dw2[[1]]))
      p_list <- append(p_list,as.numeric(dw2[[4]]))
      
    }
    
    df3 <- data.frame(Binomial = b,
                      dw_est = mean(est_list, na.rm=T),
                      dw_p = mean(p_list, na.rm=T),
                      N_Years = unique_years[1,1],
                      Sample_size = sample_size)
    
    
    D_W_df3 <- rbind(D_W_df3,df3)
  }
}
sig1 <- D_W_df2 %>% filter(dw_p <=0.05) %>% arrange(desc(Sample_size))
sig2 <- D_W_df3 %>% filter(dw_p <=0.05) %>% arrange(desc(Sample_size))

vroom_write(D_W_df2, "D:/Thesis Projects/Body Size Reviewer Analyses and data/final/Autocor_Table_Bird_Mass_Species_Mean_ALL_SEX.csv")
vroom_write(D_W_df3, "D:/Thesis Projects/Body Size Reviewer Analyses and data/final/Autocor_Table_Bird_Mass_Species_Random_ALL_SEX.csv")

#Bird Length
summary(phylo_b_l)

rr2::R2(phylo_b_l)


bl.sampl <- sample_n(residuals(phylo_b_l) %>% as.data.frame() %>% tibble::rownames_to_column(), 3000)%>%
  mutate(rowname = as.numeric(rowname))
sp.corel <- ncf::spline.correlog(x = B_Length_tree2[bl.sampl$rowname,"Lon"]$Lon,
                                 y = B_Length_tree2[bl.sampl$rowname,"Lat"]$Lat,                                   
                                 z = bl.sampl[,2], 
                                 resamp = 1000, latlon = T, xmax = 1000)
plot(sp.corel ,main = paste("Bird Length Spatial Correlation"))

B_Length_m <- B_Length_tree2

B_Length_m$Resid <- residuals(phylo_b_l)

ggplot(B_Length_m, aes(x=Year, y=Resid))+geom_point(alpha=0.1, size=1)+geom_smooth(color="red")+
  theme_bw()+xlab("Year")+ylab("Residual")

D_W_df <- data.frame(NULL)

for(i in 1:1000){
  if(i %in% c(100,200,300,400,500,600,700,800,900,1000)){print(paste0(i))}
  
  bin <- B_Length_m%>% group_by(Year)%>%slice_sample(n=1)%>%ungroup()%>%
    tidyr::complete(Year = tidyr::full_seq(1961:2018, 1))
  
  dw <- lmtest::dwtest(bin$Resid ~ bin$Year)
  
  df <- data.frame(dw_est = as.numeric(dw[[1]]),
                   dw_p = as.numeric(dw[[4]]))
  
  D_W_df <- rbind(D_W_df,df)
  
}
D_W_df <- D_W_df %>% mutate(Sig = ifelse(dw_p <=0.05,"Sig","Not Sig"))

count_sig <- D_W_df %>% group_by(Sig)%>%summarise(N=n())
vroom_write(D_W_df, "D:/Thesis Projects/Body Size Reviewer Analyses and data/final/Autocor_Table_Bird_Length_ALL_SEX.csv")

#individual autocor
D_W_df2 <- data.frame(NULL)
D_W_df3 <- data.frame(NULL)

for(b in unique(B_Length_m$Binomial)){
  
  bin <-  B_Length_m %>% filter(Binomial == b) 
  if(n_distinct(bin$Year)<2){
    next
  } else{
    sample_size <- nrow(bin)
    
    unique_years <- bin %>% summarise(N = n_distinct(Year))
    
    bin <- bin %>% group_by(Year) %>% summarise(Resid = mean(Resid,na.rm=T)) %>% 
      ungroup() %>% tidyr::complete(Year = tidyr::full_seq(1961:2018, 1))
    
    dw <- lmtest::dwtest(bin$Resid ~ bin$Year)
    
    df2 <- data.frame(Binomial = b,
                      dw_est = as.numeric(dw[[1]]),
                      dw_p = as.numeric(dw[[4]]),
                      N_Years = unique_years[1,1],
                      Sample_size = sample_size)
    
    D_W_df2 <- rbind(D_W_df2,df2)
    
    est_list<- c(NULL)
    p_list <- c(NULL)
    
    for(i in 1:100){
      
      if(i %in% c(10,20,30,40,50,60,70,80,90,100)){print(paste0(i))}
      
      bin2 <-  B_Length_m %>% filter(Binomial == b) %>% group_by(Year)%>%slice_sample(n=1)%>%ungroup()%>%
        tidyr::complete(Year = tidyr::full_seq(1961:2018, 1))
      
      dw2 <- lmtest::dwtest(bin2$Resid ~ bin2$Year)
      
      est_list <- append(est_list,as.numeric(dw2[[1]]))
      p_list <- append(p_list,as.numeric(dw2[[4]]))
      
    }
    
    df3 <- data.frame(Binomial = b,
                      dw_est = mean(est_list, na.rm=T),
                      dw_p = mean(p_list, na.rm=T),
                      N_Years = unique_years[1,1],
                      Sample_size = sample_size)
    
    
    D_W_df3 <- rbind(D_W_df3,df3)
  }
}
sig1 <- D_W_df2 %>% filter(dw_p <=0.05) %>% arrange(desc(Sample_size))
sig2 <- D_W_df3 %>% filter(dw_p <=0.05) %>% arrange(desc(Sample_size))

vroom_write(D_W_df2, "D:/Thesis Projects/Body Size Reviewer Analyses and data/final/Autocor_Table_Bird_Length_Species_Mean_ALL_SEX.csv")
vroom_write(D_W_df3, "D:/Thesis Projects/Body Size Reviewer Analyses and data/final/Autocor_Table_Bird_Length_Species_Random_ALL_SEX.csv")

###
#3 Combined mass length and cubed root divide by length models####
library(DescTools)
M_Mass_Length <- M_Mass_tree %>% filter(!is.na(Body_Length) & !Dataset=="Neon")%>%
  mutate(LLength = log10(Body_Length),
         Body_Size = (Mass^0.333)/Body_Length)%>%
  mutate(TPI_Max= ((TPI_Max - mean(TPI_Max,na.rm=T))/sd(TPI_Max, na.rm=T)),
         API= ((API - mean(API,na.rm=T))/sd(API, na.rm=T)),
         HLU= ((HLU - mean(HLU,na.rm=T))/sd(HLU, na.rm=T)),
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
         Year = ((Year - mean(Year,na.rm=T))/sd(Year, na.rm=T)),
         LMass= ((LMass - mean(LMass,na.rm=T))/sd(LMass, na.rm=T)),
         LLength= ((LLength - mean(LLength,na.rm=T))/sd(LLength, na.rm=T)),
         Body_Size = ((Body_Size - mean(Body_Size,na.rm=T))/sd(Body_Size, na.rm=T)),
         Body_Size_w = ((Body_Size_w - mean(Body_Size_w,na.rm=T))/sd(Body_Size_w, na.rm=T)))

hist(B_Mass_Length$Body_Size)

mam_tree_mass2 <- drop.tip(mammaltree, mammaltree$tip.label[-na.omit(match(unique(M_Mass_Length$Binomial),mammaltree$tip.label))])
bird_tree_mass2 <-drop.tip(Birdtree, Birdtree$tip.label[-na.omit(match(unique(B_Mass_Length$Binomial),Birdtree$tip.label))])

M_Mass_compare <- pglmm(LMass ~ TPI_Max + API + HLU
                        + TPI_Max:API + TPI_Max:HLU+ Year
                        + (TPI_Max|Binomial__)
                        + (API|Binomial__)
                        + (HLU|Binomial__)
                        + (Year|Binomial__)
                        + (1|Binomial__)
                        +  (1|Realm) + (1|Site), 
                        data=M_Mass_Length, cov_ranef = list(Binomial = mam_tree_mass2), 
                        bayes = TRUE)

M_Length_compare <- pglmm(LLength_w ~ TPI_Max + API + HLU
                          + TPI_Max:API + TPI_Max:HLU+ Year
                          + (TPI_Max|Binomial__)
                          + (API|Binomial__)
                          + (HLU|Binomial__)
                          + (Year|Binomial__)
                          + (1|Binomial__)
                          +  (1|Realm) + (1|Site), 
                          data=M_Mass_Length, cov_ranef = list(Binomial = mam_tree_mass2), 
                          bayes = TRUE)

B_Mass_compare <- pglmm(LMass ~ TPI_Max + API + HLU
                        + TPI_Max:API + TPI_Max:HLU+ Year
                        + (TPI_Max|Binomial__)
                        + (API|Binomial__)
                        + (HLU|Binomial__)
                        + (Year|Binomial__)
                        + (1|Binomial__)
                        +  (1|Realm) + (1|Site), 
                        data=B_Mass_Length, cov_ranef = list(Binomial = bird_tree_mass2), 
                        bayes = TRUE)

B_Length_compare <- pglmm(LLength_w ~ TPI_Max + API + HLU
                          + TPI_Max:API + TPI_Max:HLU+ Year
                          + (TPI_Max|Binomial__)
                          + (API|Binomial__)
                          + (HLU|Binomial__)
                          + (Year|Binomial__)
                          + (1|Binomial__)
                          +  (1|Realm) + (1|Site), 
                          data=B_Mass_Length, cov_ranef = list(Binomial = bird_tree_mass2), 
                          bayes = TRUE)

M_Size_Model <- pglmm(Body_Size_w ~ TPI_Max + API + HLU
                      + TPI_Max:API + TPI_Max:HLU+ Year
                      + (TPI_Max|Binomial__)
                      + (API|Binomial__)
                      + (HLU|Binomial__)
                      + (Year|Binomial__)
                      + (1|Binomial__)
                      +  (1|Realm) + (1|Site), 
                      data=M_Mass_Length, cov_ranef = list(Binomial = mam_tree_mass2), 
                      bayes = TRUE)

B_Size_Model <- pglmm(Body_Size_w ~ TPI_Max + API + HLU
                      + TPI_Max:API + TPI_Max:HLU+ Year
                      + (TPI_Max|Binomial__)
                      + (API|Binomial__)
                      + (HLU|Binomial__)
                      + (Year|Binomial__)
                      + (1|Binomial__)
                      +  (1|Realm) + (1|Site), 
                      data=B_Mass_Length, cov_ranef = list(Binomial = bird_tree_mass2), 
                      bayes = TRUE)
car::vif(M_Mass_compare)

summary(M_Mass_compare)
summary(M_Length_compare)
summary(B_Mass_compare)
summary(B_Length_compare)
#
summary(M_Size_Model)
summary(B_Size_Model)

ms_vif <- lmer(Body_Size ~ TPI_Max + API + HLU
               + TPI_Max:API + TPI_Max:HLU+ Year
               + (TPI_Max + API + HLU+Year||Binomial)
               +(1|Realm)
               +(1|Site)
               , data=M_Mass_Length,
               control = lmerControl(optimizer = "optimx", 
                                     calc.derivs = FALSE, 
                                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

bs_vif <- lmer(Body_Size ~ TPI_Max + API + HLU
               + TPI_Max:API + TPI_Max:HLU+ Year
               + (TPI_Max + API + HLU+Year||Binomial)
               +(1|Realm)
               +(1|Site)
               , data=B_Mass_Length,
               control = lmerControl(optimizer = "optimx", 
                                     calc.derivs = FALSE, 
                                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
rr2::R2_pred(M_Mass_compare)
rr2::R2_pred(M_Length_compare)

rr2::R2_pred(B_Mass_compare)
rr2::R2_pred(B_Length_compare)

car::vif(ms_vif)
car::vif(bs_vif)

rr2::R2_pred(M_Size_Model)

rr2::R2_pred(B_Size_Model)

####Figures####
fixed_eff_plot <- function(x, n_samp = 1000, sort = TRUE, ...) {
  
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('plot_bayes requires the ggplot2 package but it is unavailable. Use install.packages("ggplot2") to install it.')
  }
  
  if(!x$bayes) {
    stop("plot_bayes only works on communityPGLMM objects fit with bayes = TRUE")
  }
  
  if(!requireNamespace("ggridges", quietly = TRUE)) {
    stop('plot_bayes requires the ggridges package but it is unavailable. Use install.packages("ggridges") to install it.')
  }
  
  re.names <- names(x$random.effects)
  if (x$family == "gaussian") re.names <- c("residual", re.names)
  
  
  fixed_samps <- lapply(x$inla.model$marginals.fixed, function(x) INLA::inla.rmarginal(n_samp, x)) %>%
    dplyr::as_tibble() %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "var",
                        values_to = "val") %>%
    dplyr::mutate(effect_type = "Fixed Effects")
  
  samps <- fixed_samps %>%
    dplyr::filter(!var=="(Intercept)")%>%
    dplyr::mutate(var= ifelse(var=="TPI_Max", "TPI",
                              ifelse(var=="HLU_5","HLU",
                                     ifelse(var=="TPI_Max:HLU_5", "TPI:HLU",
                                            ifelse(var=="TPI_Max:API","TPI:API",
                                                   ifelse(var=="TPI_Max:HLU","TPI:HLU",var))))))%>%
    dplyr::mutate(effect_type = factor(effect_type, 
                                       levels = c("Fixed Effects")))
  
  ci <- samps %>%
    dplyr::group_by(var, effect_type) %>%
    dplyr::summarise(lower = quantile(val, 0.025),
                     upper = quantile(val, 0.975),
                     mean = mean(val),
                     .groups = "drop_last")
  
  if(sort){
    ci <- dplyr::arrange(ci, mean) %>% dplyr::ungroup() %>% 
      dplyr::mutate(var = factor(as.character(var), levels = as.character(var)))
  }
  
  sig_vars <- ci %>%
    dplyr::mutate(sig = ifelse(effect_type == "Random Effects",
                               "CI no overlap with zero",
                               ifelse(sign(lower) == sign(upper),
                                      "CI no overlap with zero",
                                      "CI overlaps zero"))) %>%
    dplyr::select(var, sig)
  
  if(sort){
    samps <- dplyr::mutate(samps, var = factor(var, levels = levels(sig_vars$var)))
  }
  samps$var <- factor(samps$var, levels=c("TPI:HLU","TPI:API","Year", "HLU", "API","TPI"))
  
  samps <- samps %>%
    dplyr::left_join(sig_vars, by = "var") %>%
    dplyr::group_by(var) %>%
    dplyr::filter(abs(val - mean(val)) < (10 * sd(val))) %>% 
    dplyr::ungroup()
  
  pal <- c("#8da0cb", "salmon")
  p <- ggplot2::ggplot(samps, ggplot2::aes(val, var, height = ..density..)) +
    ggridges::geom_density_ridges(ggplot2::aes(alpha = sig, fill = sig), 
                                  stat = "density", adjust = 2, color = NA) +
    ggplot2::geom_point(ggplot2::aes(x = mean, y = var), data = ci, inherit.aes = FALSE,size=2) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper, y = var), data = ci,
                            inherit.aes = FALSE, height = 0.2) +
    ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = "black") +
    ggplot2::scale_alpha_manual(values = c(0.7, 0.7)) +
    ggplot2::scale_fill_manual(values = rev(pal)) +
    ggplot2::scale_color_manual(values=rev(pal))+
    ggplot2::ylab("") +
    ggplot2::xlab("Estimate") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   axis.text = ggplot2::element_text(size = 12,color="black"),
                   strip.text = ggplot2::element_text(size = 16),
                   axis.title = ggplot2::element_text(size=14),
                   plot.margin = margin(10, 10, 10, 20))
  
  p
}

fixed_eff_plot(phylo_m)
ggsave("mass_mammal_plot_all.jpg",
       path="D:/Thesis Projects/Body Size Reviewer Analyses and data/figures")

fixed_eff_plot(phylo_ml)
ggsave("length_mammal_plot_all.jpg",
       path="D:/Thesis Projects/Body Size Reviewer Analyses and data/figures")

fixed_eff_plot(phylo_b)
ggsave("mass_bird_plot_all.jpg",
       path="D:/Thesis Projects/Body Size Reviewer Analyses and data/figures")

fixed_eff_plot(phylo_b_l)
ggsave("length_bird_plot_all.jpg",
       path="D:/Thesis Projects/Body Size Reviewer Analyses and data/figures")

fixed_eff_plot(M_Size_Model)
ggsave("size_mammal_plot_all.jpg",
       path="D:/Thesis Projects/Body Size Reviewer Analyses and data/figures")

fixed_eff_plot(B_Size_Model)
ggsave("size_bird_plot_all.jpg",
       path="D:/Thesis Projects/Body Size Reviewer Analyses and data/figures")


api <- vroom("D:/PhD/Thesis/Body Size Chapter/A_SUBMISSION_UPDATED/FINAL SUBMISSION/Nature CC submission/Nautre Com/Reviewed Manuscript/fig2.csv")%>%
  group_by(Class,Metric)%>%mutate(Group = paste0(Class, " ",Metric))%>%ungroup()

ggplot(api, aes(x=API, y = `TPI Estimate`))+
  geom_point(size=4, aes(color = Group), position = position_dodge(width=0.5),shape=18)+
  geom_errorbar(position=position_dodge(width = 0.5), aes(color=Group,ymin=lower_ci, ymax=upper_ci), width=.2,linewidth=1)+
  scale_color_manual(values=c("firebrick", "steelblue", "black"))+
  geom_hline(yintercept = 0,linewidth=1,linetype="dashed")+
  labs(x = "API (z score)",
       y = "TPI Coefficient Estimate")+
  theme_hc()+
  theme(axis.title = element_text(size=14, color="black"),
        axis.text = element_text(size=12, color="black"))

ggsave("mammal_plot_tpiapi.jpg",
       path="D:/Thesis Projects/Body Size Reviewer Analyses and data/figures")

       
#maps
library(terra)
global_raster <- rast("E:/Coding Files/Climate Data/Temperature Files/Tmax/Baseline/tmax_1961_01.tif")
global_raster <- global_raster*0
global_raster_100 <- aggregate(global_raster,fact=24)
#mammal mass
mam_points <- M_Mass_tree%>%dplyr::select(Binomial,Lon,Lat)%>%mutate(count=1)
xy_mam_points <- as.matrix(mam_points%>%dplyr::select(Lon,Lat))
global_raster_mm <- rasterize(xy_mam_points, global_raster_100, values=mam_points$count, fun = sum, touches=T)
plot(global_raster_mm)
writeRaster(global_raster_mm, "D:/Thesis Projects/Body Size Reviewer Analyses and data/figures/Mammal_Mass_distribution.tif")
#mammal length
mam_pointsl <- M_Length_tree%>%dplyr::select(Binomial,Lon,Lat)%>%mutate(count=1)
xy_mam_pointsl <- as.matrix(mam_pointsl%>%dplyr::select(Lon,Lat))
global_raster_ml <- rasterize(xy_mam_pointsl, global_raster_100, values=mam_pointsl$count, fun = sum, touches=T)
plot(global_raster_ml)
writeRaster(global_raster_ml, "D:/Thesis Projects/Body Size Reviewer Analyses and data/figures/Mammal_Length_distribution.tif")

#bird mass
bird_points <- B_Mass_tree%>%dplyr::select(Binomial,Lon,Lat)%>%mutate(count=1)
xy_bird_points <- as.matrix(bird_points%>%dplyr::select(Lon,Lat))
global_raster_bm <- rasterize(xy_bird_points, global_raster_100, values=bird_points$count, fun = sum, touches=T)
plot(global_raster_bm)
writeRaster(global_raster_bm, "D:/Thesis Projects/Body Size Reviewer Analyses and data/figures/Bird_Mass_distribution.tif")

#bird length
bird_pointsl <- B_Length_tree%>%dplyr::select(Binomial,Lon,Lat)%>%mutate(count=1)
xy_bird_pointsl <- as.matrix(bird_pointsl%>%dplyr::select(Lon,Lat))
global_raster_bl <- rasterize(xy_bird_pointsl, global_raster_100, values=bird_pointsl$count, fun = sum, touches=T)
plot(global_raster_bl)
writeRaster(global_raster_bl, "D:/Thesis Projects/Body Size Reviewer Analyses and data/figures/Bird_Length_distribution.tif")

##mam points combo
mm <- M_Mass_tree %>% filter(is.na(Body_Length))%>%dplyr::select(Binomial,Lat,Lon)%>%mutate(count=1)
ml <- M_Length_tree %>%dplyr::select(Binomial,Lat,Lon)%>%mutate(count=1)%>%rbind(mm)
xy_ml_points <- as.matrix(ml%>%dplyr::select(Lon,Lat))
global_raster_ml_c <- rasterize(xy_ml_points, global_raster_100, values=ml$count, fun = sum, touches=T)
plot(global_raster_ml_c)
writeRaster(global_raster_ml_c, "D:/Thesis Projects/Body Size Reviewer Analyses and data/figures/Mammal_all_distribution.tif")

bm <- B_Mass_tree %>% filter(is.na(Body_Length))%>%dplyr::select(Binomial,Lat,Lon)%>%mutate(count=1)
bl <- B_Length_tree %>%dplyr::select(Binomial,Lat,Lon)%>%mutate(count=1)%>%rbind(bm)
xy_bl_points <- as.matrix(bl%>%dplyr::select(Lon,Lat))
global_raster_bl_c <- rasterize(xy_bl_points, global_raster_100, values=bl$count, fun = sum, touches=T)
plot(global_raster_bl_c)
writeRaster(global_raster_bl_c, "D:/Thesis Projects/Body Size Reviewer Analyses and data/figures/bird_all_distribution.tif")




M_Mass_m <- M_Mass_10_tree 
M_Mass_m$Resid <- residuals(Mammal_Mass_Mod_10)

ggplot(M_Mass_m, aes(x=Year, y=Resid))+geom_point(alpha=0.1, size=1)+geom_smooth(color="red")+
  theme_bw()+xlab("Year")+ylab("Residual")

ggsave("mass_mammal_resid_year.jpg",
       path="D:/Thesis Projects/Body Size Reviewer Analyses and data/figures")

M_Length_m <- M_Length_10_tree 
M_Length_m$Resid <- residuals(Mammal_Length_Mod_10)

ggplot(M_Length_m, aes(x=Year, y=Resid))+geom_point(alpha=0.1, size=1)+geom_smooth(color="red")+
  theme_bw()+xlab("Year")+ylab("Residual")

ggsave("length_mammal_resid_year.jpg",
       path="D:/Thesis Projects/Body Size Reviewer Analyses and data/figures")



B_Mass_m <- B_Mass_10_tree 
B_Mass_m$Resid <- residuals(Bird_Mass_Mod_10)

ggplot(B_Mass_m, aes(x=Year, y=Resid))+geom_point(alpha=0.1, size=1)+geom_smooth(color="red")+
  theme_bw()+xlab("Year")+ylab("Residual")

ggsave("mass_bird_resid_year.jpg",
       path="D:/Thesis Projects/Body Size Reviewer Analyses and data/figures")

B_Length_m <-B_Length_10_tree 
B_Length_m$Resid <- residuals(Bird_Length_Mod_10)

ggplot(B_Length_m, aes(x=Year, y=Resid))+geom_point(alpha=0.1, size=1)+geom_smooth(color="red")+
  theme_bw()+xlab("Year")+ylab("Residual")

ggsave("length_bird_resid_year.jpg",
       path="D:/Thesis Projects/Body Size Reviewer Analyses and data/figures")
