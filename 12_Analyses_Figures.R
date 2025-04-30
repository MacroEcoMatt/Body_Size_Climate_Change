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

M_Mass <- vroom("D:/Thesis Projects/GCB Revised Manuscript/Datasets/Data_4_Mammal_Mass.csv",delim=",")
M_Length <- vroom("D:/Thesis Projects/GCB Revised Manuscript/Datasets/Data_5_Mammal_Length.csv",delim=",")
B_Mass <- vroom("D:/Thesis Projects/GCB Revised Manuscript/Datasets/Data_1_Bird_Mass.csv",delim=",")
B_Length <- vroom("D:/Thesis Projects/GCB Revised Manuscript/Datasets/Data_2_Bird_Length",delim=",")

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
         ULU= ((ULU - mean(ULU,na.rm=T))/sd(ULU, na.rm=T)),
         ALU= ((ALU - mean(ALU,na.rm=T))/sd(ALU, na.rm=T)),
         Year = ((Year - mean(Year,na.rm=T))/sd(Year, na.rm=T)),
         LMass = ((LMass - mean(LMass,na.rm=T))/sd(LMass, na.rm=T)))


phylo_m_a <- pglmm(LMass ~ TPI_Max + API + ALU
                   + TPI_Max:API + TPI_Max:ALU+ Year
                   + (TPI_Max|Binomial__)
                   + (API|Binomial__)
                   + (ALU|Binomial__)
                   + (Year|Binomial__)
                   + (1|Binomial__)
                   +  (1|Realm) + (1|Site), 
                   data=M_Mass_tree2, cov_ranef = list(Binomial = mam_tree_mass), 
                   bayes = TRUE)

summary(phylo_m_a)

samples4 <- inla.posterior.sample(n = 1000, result = phylo_m_a$inla.model)

inter4 <- unlist(lapply(samples4, function(s) s$latent["(Intercept):1", 1]))
TPI4 <- unlist(lapply(samples4, function(s) s$latent["TPI_Max:1", 1]))
API4 <- unlist(lapply(samples4, function(s) s$latent["API:1", 1]))
ALU4 <- unlist(lapply(samples4, function(s) s$latent["ALU:1", 1]))
ULU4 <- unlist(lapply(samples4, function(s) s$latent["ULU:1", 1]))
ta4 <- unlist(lapply(samples4, function(s) s$latent["TPI_Max:API:1", 1]))
talu4 <- unlist(lapply(samples4, function(s) s$latent["TPI_Max:ALU:1", 1]))
tulu4 <- unlist(lapply(samples4, function(s) s$latent["TPI_Max:ULU:1", 1]))
ye4 <- unlist(lapply(samples4, function(s) s$latent["Year:1", 1]))

sigma4 <- unlist(lapply(samples4,
                        function(s) 1/sqrt(s$hyperpar["Precision for the Gaussian observations"])))

y.sim4 <- inter4 + TPI4 + API4 + ALU4 + ULU4 + ta4 + talu4 + tulu4 + ye4 + rnorm(1000, sd=sigma4)
y4 <- sample(M_Mass_tree2$LMass, 1000, replace=F)

d4 <- bind_rows(list(y=data.frame(y=y4), 
                     y_sim=data.frame(y=y.sim4)),
                .id="data.type")

ggplot(d4, aes(y, color=data.type,fill=data.type)) + geom_histogram(linewidth=1)+
  theme_minimal()+
  scale_fill_manual(values=c("black","firebrick"))+
  scale_colour_manual(values=c("black","firebrick"))+xlim(-3,3)+
  labs(y="Density", x="Log10 Body Mass (z-score)",color="",fill="")+
  theme(axis.title = element_text(color="black",size=12, face="bold"),
        axis.text = element_text(color="black",size=12))

ggsave("D:/Thesis Projects/GCB Revised Manuscript/figures/Mammal Mass/Mammal_Mass_PP_LandUseUpdate.jpg")
#p_direction
library(bayestestR)
p_direction_mammal_mass <- data.frame(intercept = p_direction(inter4)[1,2],
                                      TPI = p_direction(TPI4)[1,2],
                                      API = p_direction(API4)[1,2],
                                      ALU = p_direction(ALU4)[1,2],
                                      ULU = p_direction(ULU4)[1,2],
                                      TPI_API = p_direction(ta4)[1,2],
                                      TPI_ALU = p_direction(talu4)[1,2],
                                      TPI_ULU = p_direction(talu4)[1,2],
                                      Year = p_direction(ye4)[1,2],
                                      interceptp = p_direction(inter4,as_p=T)[1,2],
                                      TPIp = p_direction(TPI4,as_p=T)[1,2],
                                      APIp = p_direction(API4,as_p=T)[1,2],
                                      ALUp = p_direction(ALU4,as_p=T)[1,2],
                                      ULUp = p_direction(ALU4,as_p=T)[1,2],
                                      TPI_APIp = p_direction(ta4,as_p=T)[1,2],
                                      TPI_ALUp = p_direction(talu4,as_p=T)[1,2],
                                      TPI_ULUp = p_direction(talu4,as_p=T)[1,2],
                                      Yearp = p_direction(ye4,as_p=T)[1,2])

vroom_write(p_direction_mammal_mass, "D:/Thesis Projects/GCB Revised Manuscript/pp_check/mammal_mass_LandUseUpdate.csv",delim=",")

###
M_Length_tree2 <- M_Length_tree%>%filter(is.finite(LLength))%>%
  mutate(TPI_Max= ((TPI_Max - mean(TPI_Max,na.rm=T))/sd(TPI_Max, na.rm=T)),
         API= ((API - mean(API,na.rm=T))/sd(API, na.rm=T)),
         ULU= ((ULU - mean(ULU,na.rm=T))/sd(ULU, na.rm=T)),
         ALU= ((ALU - mean(ALU,na.rm=T))/sd(ALU, na.rm=T)),
         Year = ((Year - mean(Year,na.rm=T))/sd(Year, na.rm=T)),
         LLength= ((LLength - mean(LLength,na.rm=T))/sd(LLength, na.rm=T)))%>%
  filter(!Dataset=="Neon")%>%filter(LLength < 10 & LLength > -10)


phylo_ml_au <- pglmm(LLength~ TPI_Max + API + ALU + ULU
                   + TPI_Max:API + TPI_Max:ALU + TPI_Max:ULU+ Year
                   + (TPI_Max|Binomial__)
                   + (API|Binomial__)
                   + (ALU|Binomial__)
                   + (ULU|Binomial__)
                   + (Year|Binomial__)
                   + (1|Binomial__)
                   +  (1|Realm) + (1|Site), 
                   data=M_Length_tree2, cov_ranef = list(Binomial = mam_tree_length), 
                   bayes = TRUE)

summary(phylo_ml_au)

samples3 <- inla.posterior.sample(n = 1000, result = phylo_ml_au$inla.model)

inter3 <- unlist(lapply(samples3, function(s) s$latent["(Intercept):1", 1]))
TPI3 <- unlist(lapply(samples3, function(s) s$latent["TPI_Max:1", 1]))
API3 <- unlist(lapply(samples3, function(s) s$latent["API:1", 1]))
ALU3 <- unlist(lapply(samples3, function(s) s$latent["ALU:1", 1]))
ULU3 <- unlist(lapply(samples3, function(s) s$latent["ULU:1", 1]))

ta3 <- unlist(lapply(samples3, function(s) s$latent["TPI_Max:API:1", 1]))
talu3 <- unlist(lapply(samples3, function(s) s$latent["TPI_Max:ALU:1", 1]))
tulu3 <- unlist(lapply(samples3, function(s) s$latent["TPI_Max:ULU:1", 1]))

ye3 <- unlist(lapply(samples3, function(s) s$latent["Year:1", 1]))

sigma3 <- unlist(lapply(samples3,
                        function(s) 1/sqrt(s$hyperpar["Precision for the Gaussian observations"])))

y.sim3 <- inter3 + TPI3 + API3 + ALU3 + ULU3 + ta3 + talu3 + tulu3 + ye3 + rnorm(1000, sd=sigma3)
y3 <- sample(M_Length_tree2$LLength, 1000,replace=T)

d3 <- bind_rows(list(y=data.frame(y=y3), 
                     y_sim=data.frame(y=y.sim3)),
                .id="data.type")

ggplot(d3, aes(y, color=data.type,fill=data.type)) + geom_histogram(linewidth=1)+
  theme_minimal()+
  scale_fill_manual(values=c("black","firebrick"))+
  scale_colour_manual(values=c("black","firebrick"))+xlim(-3,3)+
  labs(y="Density", x="Log10 Body Length (z-score)",color="",fill="")+
  theme(axis.title = element_text(color="black",size=12, face="bold"),
        axis.text = element_text(color="black",size=12))

ggsave("D:/Thesis Projects/GCB Revised Manuscript/figures/Mammal Length/Mammal_Length_PP_LandUseUpdate.jpg")
#p_direction
p_direction_mammal_length <- data.frame(intercept = p_direction(inter3)[1,2],
                                        TPI = p_direction(TPI3)[1,2],
                                        API = p_direction(API3)[1,2],
                                        ALU = p_direction(ALU3)[1,2],
                                        ULU = p_direction(ULU3)[1,2],
                                        TPI_API = p_direction(ta3)[1,2],
                                        TPI_ALU = p_direction(talu3)[1,2],
                                        TPI_ULU = p_direction(tulu3)[1,2],
                                        Year = p_direction(ye3)[1,2],
                                        interceptp = p_direction(inter3,as_p=T)[1,2],
                                        TPIp = p_direction(TPI3,as_p=T)[1,2],
                                        APIp = p_direction(API3,as_p=T)[1,2],
                                        ALUp = p_direction(ALU3,as_p=T)[1,2],
                                        ULUp = p_direction(ULU3,as_p=T)[1,2],
                                        
                                        TPI_APIp = p_direction(ta3,as_p=T)[1,2],
                                        TPI_ALUp = p_direction(talu3,as_p=T)[1,2],
                                        TPI_ULUp = p_direction(tulu3,as_p=T)[1,2],
                                        Yearp = p_direction(ye3,as_p=T)[1,2])


vroom_write(p_direction_mammal_length, "D:/Thesis Projects/GCB Revised Manuscript/pp_check/mammal_length_LandUseUpdate.csv",delim=",")

###
B_Mass_tree2 <- B_Mass_tree%>%filter(is.finite(LMass) & is.finite(API))%>%
  mutate(TPI_Max= ((TPI_Max - mean(TPI_Max,na.rm=T))/sd(TPI_Max, na.rm=T)),
         API= ((API - mean(API,na.rm=T))/sd(API, na.rm=T)),
         ULU= ((ULU - mean(ULU,na.rm=T))/sd(ULU, na.rm=T)),
         ALU= ((ALU - mean(ALU,na.rm=T))/sd(ALU, na.rm=T)),
         Year = ((Year - mean(Year,na.rm=T))/sd(Year, na.rm=T)),
         LMass= ((LMass - mean(LMass,na.rm=T))/sd(LMass, na.rm=T)),
         API_w = DescTools::Winsorize(API, val = quantile(API, probs = c(0.005, 0.995), na.rm = T)))


phylo_b_au <- pglmm(LMass ~ TPI_Max + API_w + ALU + ULU
                   + TPI_Max:API_w + TPI_Max:ALU + TPI_Max:ULU + Year
                   + (TPI_Max|Binomial__)
                   + (API_w|Binomial__)
                   + (ALU|Binomial__)
                   + (ULU|Binomial__)
                   + (Year|Binomial__)
                   + (1|Binomial__)
                   +  (1|Realm) + (1|Site), 
                   data=B_Mass_tree2, cov_ranef = list(Binomial = bird_tree_mass), 
                   bayes = TRUE)

summary(phylo_b_au)

samples2 <- inla.posterior.sample(n = 1000, result = phylo_b_au$inla.model)
s <- samples2[[1]]$latent

inter2 <- unlist(lapply(samples2, function(s) s$latent["(Intercept):1", 1]))
TPI2 <- unlist(lapply(samples2, function(s) s$latent["TPI_Max:1", 1]))
API2 <- unlist(lapply(samples2, function(s) s$latent["API_w:1", 1]))
ALU2 <- unlist(lapply(samples2, function(s) s$latent["ALU:1", 1]))
ULU2 <- unlist(lapply(samples2, function(s) s$latent["ULU:1", 1]))

ta2 <- unlist(lapply(samples2, function(s) s$latent["TPI_Max:API_w:1", 1]))
talu2 <- unlist(lapply(samples2, function(s) s$latent["TPI_Max:ALU:1", 1]))
tulu2 <- unlist(lapply(samples2, function(s) s$latent["TPI_Max:ULU:1", 1]))

ye2 <- unlist(lapply(samples2, function(s) s$latent["Year:1", 1]))

sigma2 <- unlist(lapply(samples2,
                        function(s) 1/sqrt(s$hyperpar["Precision for the Gaussian observations"])))

y.sim2 <- inter2 + TPI2 + API2 + ALU2 + ULU2 + ta2 + talu2 + tulu2 + ye2 + rnorm(1000, sd=sigma2)
y2 <- sample(B_Mass_tree2$LMass,1000,replace=T)

d2 <- bind_rows(list(y=data.frame(y=y2), 
                     y_sim=data.frame(y=y.sim2)),
                .id="data.type")

ggplot(d2, aes(y, color=data.type,fill=data.type)) + geom_histogram(linewidth=1)+
  theme_minimal()+
  scale_fill_manual(values=c("black","firebrick"))+
  scale_colour_manual(values=c("black","firebrick"))+xlim(-3,3)+
  labs(y="Density", x="Log10 Body Mass (z-score)",color="",fill="")+
  theme(axis.title = element_text(color="black",size=12, face="bold"),
        axis.text = element_text(color="black",size=12))


ggsave("D:/Thesis Projects/GCB Revised Manuscript/figures/Bird Mass/Bird_Mass_PP_LandUseUpdate.jpg")
#p_direction
p_direction_bird_mass <- data.frame(intercept = p_direction(inter2)[1,2],
                                    TPI = p_direction(TPI2)[1,2],
                                    API = p_direction(API2)[1,2],
                                    ALU = p_direction(ALU2)[1,2],
                                    ULU = p_direction(ULU2)[1,2],
                                    
                                    TPI_API = p_direction(ta2)[1,2],
                                    TPI_ALU = p_direction(talu2)[1,2],
                                    TPI_ULU = p_direction(tulu2)[1,2],
                                    Year = p_direction(ye2)[1,2],
                                    interceptp = p_direction(inter2,as_p=T)[1,2],
                                    TPIp = p_direction(TPI2,as_p=T)[1,2],
                                    APIp = p_direction(API2,as_p=T)[1,2],
                                    ALUp = p_direction(ALU2,as_p=T)[1,2],
                                    ULUp = p_direction(ULU2,as_p=T)[1,2],
                                    TPI_APIp = p_direction(ta2,as_p=T)[1,2],
                                    TPI_ALUp = p_direction(talu2,as_p=T)[1,2],
                                    TPI_ULUp = p_direction(tulu2,as_p=T)[1,2],
                                    Yearp = p_direction(ye2,as_p=T)[1,2])

vroom_write(p_direction_bird_mass, "D:/Thesis Projects/GCB Revised Manuscript/pp_check/bird_mass_LandUseUpdate.csv",delim=",")
##
##
B_Length_tree2 <- B_Length_tree%>%filter(is.finite(LLength) & is.finite(API))%>%
  mutate(TPI_Max= ((TPI_Max - mean(TPI_Max,na.rm=T))/sd(TPI_Max, na.rm=T)),
         API= ((API - mean(API,na.rm=T))/sd(API, na.rm=T)),
         ULU= ((ULU - mean(ULU,na.rm=T))/sd(ULU, na.rm=T)),
         ALU= ((ALU - mean(ALU,na.rm=T))/sd(ALU, na.rm=T)),
         Year = ((Year - mean(Year,na.rm=T))/sd(Year, na.rm=T)),
         LLength= ((LLength - mean(LLength,na.rm=T))/sd(LLength, na.rm=T)))


phylo_b_l_au <- pglmm(LLength ~ TPI_Max + API + ALU + ULU
                     + TPI_Max:API + TPI_Max:ALU + TPI_Max:ULU + Year
                     + (TPI_Max|Binomial__)
                     + (API|Binomial__)
                     + (ALU|Binomial__)
                     + (ULU|Binomial__)
                     + (Year|Binomial__)
                     + (1|Binomial__)
                     +  (1|Realm) + (1|Site), 
                     data=B_Length_tree2, cov_ranef = list(Binomial = bird_tree_length), 
                     bayes = TRUE)

summary(phylo_b_l_au)

samples <- inla.posterior.sample(n = 1000, result = phylo_b_l_au$inla.model)

inter <- unlist(lapply(samples, function(s) s$latent["(Intercept):1", 1]))
TPI <- unlist(lapply(samples, function(s) s$latent["TPI_Max:1", 1]))
API <- unlist(lapply(samples, function(s) s$latent["API:1", 1]))
ALU <- unlist(lapply(samples, function(s) s$latent["ALU:1", 1]))
ULU <- unlist(lapply(samples, function(s) s$latent["ULU:1", 1]))

ta <- unlist(lapply(samples, function(s) s$latent["TPI_Max:API:1", 1]))
talu <- unlist(lapply(samples, function(s) s$latent["TPI_Max:ALU:1", 1]))
tulu <- unlist(lapply(samples, function(s) s$latent["TPI_Max:ULU:1", 1]))

ye <- unlist(lapply(samples, function(s) s$latent["Year:1", 1]))

sigma <- unlist(lapply(samples,
                       function(s) 1/sqrt(s$hyperpar["Precision for the Gaussian observations"])))

y.sim <- inter + TPI + API + ALU + ULU + ta + talu + tulu + ye + rnorm(1000, sd=sigma)

y <- sample(B_Length_tree2$LLength,1000,replace=T)

d <- bind_rows(list(y=data.frame(y=y), 
                    y_sim=data.frame(y=y.sim)),
               .id="data.type")

ggplot(d, aes(y, color=data.type,fill=data.type)) + geom_histogram(linewidth=1)+
  theme_minimal()+
  scale_fill_manual(values=c("black","firebrick"))+
  scale_colour_manual(values=c("black","firebrick"))+xlim(-1,3)+
  labs(y="Density", x="Log10 Body Length (z-score)",color="",fill="")+
  theme(axis.title = element_text(color="black",size=12, face="bold"),
        axis.text = element_text(color="black",size=12))

ggsave("D:/Thesis Projects/GCB Revised Manuscript/figures/Bird Length/Bird_Length_PP_LandUseUpdate.jpg")
#p_direction
p_direction_bird_length <- data.frame(intercept = p_direction(inter)[1,2],
                                      TPI = p_direction(TPI)[1,2],
                                      API = p_direction(API)[1,2],
                                      ALU = p_direction(ALU)[1,2],
                                      ULU = p_direction(ULU)[1,2],
                                      
                                      TPI_API = p_direction(ta)[1,2],
                                      TPI_ALU = p_direction(talu)[1,2],
                                      TPI_ULU = p_direction(tulu)[1,2],
                                      Year = p_direction(ye)[1,2],
                                      interceptp = p_direction(inter,as_p=T)[1,2],
                                      TPIp = p_direction(TPI,as_p=T)[1,2],
                                      APIp = p_direction(API,as_p=T)[1,2],
                                      ALUp = p_direction(ALU,as_p=T)[1,2],
                                      ULUp = p_direction(ULU,as_p=T)[1,2],
                                      TPI_APIp = p_direction(ta,as_p=T)[1,2],
                                      TPI_ALUp = p_direction(talu,as_p=T)[1,2],
                                      TPI_ULUp = p_direction(tulu,as_p=T)[1,2],
                                      Yearp = p_direction(ye,as_p=T)[1,2])

vroom_write(p_direction_bird_length, "D:/Thesis Projects/GCB Revised Manuscript/pp_check/bird_length_LandUseUpdate.csv",delim=",")
####

library(DescTools)
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

mam_tree_mass2 <- drop.tip(mammaltree, mammaltree$tip.label[-na.omit(match(unique(M_Mass_Length$Binomial),mammaltree$tip.label))])
bird_tree_mass2 <-drop.tip(Birdtree, Birdtree$tip.label[-na.omit(match(unique(B_Mass_Length$Binomial),Birdtree$tip.label))])

vroom_write(M_Mass_Length,"D:/Thesis Projects/GCB Revised Manuscript/Datasets/Mammal_Size.csv",delim=",")

vroom_write(B_Mass_Length,"D:/Thesis Projects/GCB Revised Manuscript/Datasets/Bird_Size.csv",delim=",")
phylo_m_ml_au <- pglmm(Body_Size_w ~ TPI_Max + API + ALU + ULU
                      + TPI_Max:API + TPI_Max:ALU+ TPI_Max:ULU + Year
                      + (TPI_Max|Binomial__)
                      + (API|Binomial__)
                      + (ALU|Binomial__)
                      + (ULU|Binomial__)
                      + (Year|Binomial__)
                      + (1|Binomial__)
                      +  (1|Realm) + (1|Site), 
                      data=M_Mass_Length, cov_ranef = list(Binomial = mam_tree_mass2), 
                      bayes = TRUE)

summary(phylo_m_ml_u)
summary(phylo_m_ml_a)
summary(phylo_m_ml_au)

samples6 <- inla.posterior.sample(n = 1000, result = phylo_m_ml_au$inla.model)

inter6 <- unlist(lapply(samples6, function(s) s$latent["(Intercept):1", 1]))
TPI6 <- unlist(lapply(samples6, function(s) s$latent["TPI_Max:1", 1]))
API6 <- unlist(lapply(samples6, function(s) s$latent["API:1", 1]))
HLU6 <- unlist(lapply(samples6, function(s) s$latent["HLU:1", 1]))
ta6 <- unlist(lapply(samples6, function(s) s$latent["TPI_Max:API:1", 1]))
th6 <- unlist(lapply(samples6, function(s) s$latent["TPI_Max:HLU:1", 1]))
ye6 <- unlist(lapply(samples6, function(s) s$latent["Year:1", 1]))

sigma6 <- unlist(lapply(samples6,
                        function(s) 1/sqrt(s$hyperpar["Precision for the Gaussian observations"])))

y.sim6 <- inter6 + TPI6 + API6 + HLU6 + ta6 + th6 + ye6 + rnorm(1000, sd=sigma6)

y6 <- sample(M_Mass_Length$Body_Size_w,1000,replace=T)

d6 <- bind_rows(list(y=data.frame(y=y6), 
                     y_sim=data.frame(y=y.sim6)),
                .id="data.type")

ggplot(d6, aes(y, color=data.type,fill=data.type)) + geom_histogram(linewidth=1)+
  theme_minimal()+
  scale_fill_manual(values=c("black","firebrick"))+
  scale_colour_manual(values=c("black","firebrick"))+xlim(-1,3)+
  labs(y="Density", x="Log10 Mass:Length Ratio (z-score)",color="",fill="")+
  theme(axis.title = element_text(color="black",size=12, face="bold"),
        axis.text = element_text(color="black",size=12))

ggsave("D:/Thesis Projects/GCB Revised Manuscript/figures/Mammal Length/Mammal_Mass_Length_PP_LandUseUpdate.jpg")

p_direction_mammal_mass_length <- data.frame(intercept = p_direction(inter5)[1,2],
                                             TPI = p_direction(TPI5)[1,2],
                                             API = p_direction(API5)[1,2],
                                             HLU = p_direction(HLU5)[1,2],
                                             TPI_API = p_direction(ta5)[1,2],
                                             TPI_HLU = p_direction(th5)[1,2],
                                             Year = p_direction(ye5)[1,2],
                                             interceptp = p_direction(inter5,as_p=T)[1,2],
                                             TPIp = p_direction(TPI5,as_p=T)[1,2],
                                             APIp = p_direction(API5,as_p=T)[1,2],
                                             HLUp = p_direction(HLU5,as_p=T)[1,2],
                                             TPI_APIp = p_direction(ta5,as_p=T)[1,2],
                                             TPI_HLUp = p_direction(th5,as_p=T)[1,2],
                                             Yearp = p_direction(ye5,as_p=T)[1,2])

vroom_write(p_direction_mammal_mass_length, "D:/Thesis Projects/GCB Revised Manuscript/pp_check/Mammal_mass_length_LandUseUpdate.csv",delim=",")
###
###

phylo_b_ml_au <- pglmm(Body_Size_w ~ TPI_Max + API+ ALU + ULU
                       + TPI_Max:API + TPI_Max:ALU + TPI_Max:ULU + Year
                       + (TPI_Max|Binomial__)
                       + (API|Binomial__)
                       + (ALU|Binomial__)
                       + (ULU|Binomial__)
                       + (Year|Binomial__)
                       + (1|Binomial__)
                       +  (1|Realm) + (1|Site), 
                       data=B_Mass_Length, cov_ranef = list(Binomial = bird_tree_mass2), 
                       bayes = TRUE)

summary(phylo_b_ml_u)
summary(phylo_b_ml_a)
summary(phylo_b_ml_au)

#PPcheck
samples5 <- inla.posterior.sample(n = 1000, result = phylo_b_ml_au$inla.model)

inter5 <- unlist(lapply(samples5, function(s) s$latent["(Intercept):1", 1]))
TPI5 <- unlist(lapply(samples5, function(s) s$latent["TPI_Max:1", 1]))
API5 <- unlist(lapply(samples5, function(s) s$latent["API:1", 1]))
HLU5 <- unlist(lapply(samples5, function(s) s$latent["HLU:1", 1]))
ta5 <- unlist(lapply(samples5, function(s) s$latent["TPI_Max:API:1", 1]))
th5 <- unlist(lapply(samples5, function(s) s$latent["TPI_Max:HLU:1", 1]))
ye5 <- unlist(lapply(samples5, function(s) s$latent["Year:1", 1]))

sigma5 <- unlist(lapply(samples5,
                        function(s) 1/sqrt(s$hyperpar["Precision for the Gaussian observations"])))

y.sim5 <- inter5 + TPI5 + API5 + HLU5 + ta5 + th5 + ye5 + rnorm(1000, sd=sigma5)

y5 <- sample(B_Mass_Length$Body_Size_w,1000,replace=T)

d5 <- bind_rows(list(y=data.frame(y=y5), 
                     y_sim=data.frame(y=y.sim5)),
                .id="data.type")

ggplot(d5, aes(y, color=data.type,fill=data.type)) + geom_histogram(linewidth=1)+
  theme_minimal()+
  scale_fill_manual(values=c("black","firebrick"))+
  scale_colour_manual(values=c("black","firebrick"))+xlim(-1,3)+
  labs(y="Density", x="Log10 Mass:Length Ratio (z-score)",color="",fill="")+
  theme(axis.title = element_text(color="black",size=12, face="bold"),
        axis.text = element_text(color="black",size=12))

ggsave("D:/Thesis Projects/GCB Revised Manuscript/figures/Bird Length/Bird_Mass_Length_PP_LandUseUpdate.jpg")

p_direction_bird_mass_length <- data.frame(intercept = p_direction(inter5)[1,2],
                                           TPI = p_direction(TPI5)[1,2],
                                           API = p_direction(API5)[1,2],
                                           HLU = p_direction(HLU5)[1,2],
                                           TPI_API = p_direction(ta5)[1,2],
                                           TPI_HLU = p_direction(th5)[1,2],
                                           Year = p_direction(ye5)[1,2],
                                           interceptp = p_direction(inter5,as_p=T)[1,2],
                                           TPIp = p_direction(TPI5,as_p=T)[1,2],
                                           APIp = p_direction(API5,as_p=T)[1,2],
                                           HLUp = p_direction(HLU5,as_p=T)[1,2],
                                           TPI_APIp = p_direction(ta5,as_p=T)[1,2],
                                           TPI_HLUp = p_direction(th5,as_p=T)[1,2],
                                           Yearp = p_direction(ye5,as_p=T)[1,2])

vroom_write(p_direction_bird_mass_length, "D:/Thesis Projects/GCB Revised Manuscript/pp_check/bird_mass_length_LandUseUpdate.csv",delim=",")


###get model variables
rr2::R2_pred(phylo_b_au)
rr2::R2_pred(phylo_b_l_au)
rr2::R2_pred(phylo_b_ml_au)
rr2::R2_pred(phylo_m_au)
rr2::R2_pred(phylo_ml_au)
rr2::R2_pred(phylo_m_ml_au)

##coeff plots
##coeff plots
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
                              ifelse(var=="API_w","API",
                                     ifelse(var=="TPI_Max:ULU", "TPI:ULU",
                                            ifelse(var=="TPI_Max:API","TPI:API",
                                                   ifelse(var=="TPI_Max:ALU","TPI:ALU",
                                                          ifelse(var=="TPI_Max:API_w","TPI:API",var)))))))%>%
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
  samps$var <- factor(samps$var, levels=c("TPI:ULU","TPI:ALU","TPI:API","Year", "ULU", "ALU" ,"API","TPI"))
  
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

fixed_eff_plot(phylo_m_au)
ggsave("mass_mammal_plot_all.jpg",
       path="D:/Thesis Projects/GCB Revised Manuscript/figures/New figs land use")

fixed_eff_plot(phylo_ml_au)
ggsave("length_mammal_plot_all.jpg",
       path="D:/Thesis Projects/GCB Revised Manuscript/figures/New figs land use")

fixed_eff_plot(phylo_m_ml_au)
ggsave("size_mammal_plot_all.jpg",
       path="D:/Thesis Projects/GCB Revised Manuscript/figures/New figs land use")

fixed_eff_plot(phylo_b_au)
ggsave("mass_bird_plot_all.jpg",
       path="D:/Thesis Projects/GCB Revised Manuscript/figures/New figs land use")

fixed_eff_plot(phylo_b_l_au)
ggsave("length_bird_plot_all.jpg",
       path="D:/Thesis Projects/GCB Revised Manuscript/figures/New figs land use")

fixed_eff_plot(phylo_b_ml_au)
ggsave("size_bird_plot_all.jpg",
       path="D:/Thesis Projects/GCB Revised Manuscript/figures/New figs land use")

## autocor
#mammal mass
mm.sampl <- sample_n(residuals(phylo_m_a) %>% as.data.frame() %>% tibble::rownames_to_column(), 3000)%>%
  mutate(rowname = as.numeric(rowname))
sp.corel <- ncf::spline.correlog(x = M_Mass_tree2[mm.sampl$rowname,"Lon"]$Lon,
                                 y = M_Mass_tree2[mm.sampl$rowname,"Lat"]$Lat,                                   
                                 z = mm.sampl[,2], 
                                 resamp = 1000, latlon = T, xmax = 1000)
plot(sp.corel ,main = paste("Mammal Mass Spatial Correlation"))
ggsave("mass_mammal_autocor.jpg",
       path="D:/Thesis Projects/GCB Revised Manuscript/figures/New figs land use")

M_Mass_m <- M_Mass_tree2 

M_Mass_m$Resid <- residuals(phylo_m_au)

ggplot(M_Mass_m, aes(x=Year, y=Resid))+geom_point(alpha=0.1, size=1)+geom_smooth(color="red")+
  theme_bw()+xlab("Year")+ylab("Residual")

ggsave("mass_mammal_resid_year.jpg",
       path="D:/Thesis Projects/GCB Revised Manuscript/figures/New figs land use")
#mammal length
ml.sampl <- sample_n(residuals(phylo_ml_au) %>% as.data.frame() %>% tibble::rownames_to_column(), 3000)%>%
  mutate(rowname = as.numeric(rowname))
sp.corel <- ncf::spline.correlog(x = M_Length_tree2[ml.sampl$rowname,"Lon"]$Lon,
                                 y = M_Length_tree2[ml.sampl$rowname,"Lat"]$Lat,                                   
                                 z = ml.sampl[,2], 
                                 resamp = 1000, latlon = T, xmax = 1000)
plot(sp.corel ,main = paste("Mammal Length Spatial Correlation"))


M_Length_m <- M_Length_tree2

M_Length_m$Resid <- residuals(phylo_ml_au)

ggplot(M_Length_m, aes(x=Year, y=Resid))+geom_point(alpha=0.1, size=1)+geom_smooth(color="red")+
  theme_bw()+xlab("Year")+ylab("Residual")
ggsave("length_mammal_resid_year.jpg",
       path="D:/Thesis Projects/GCB Revised Manuscript/figures/New figs land use")
#mammal size
mml.sampl <- sample_n(residuals(phylo_m_ml_au) %>% as.data.frame() %>% tibble::rownames_to_column(), 3000)%>%
  mutate(rowname = as.numeric(rowname))
sp.corel <- ncf::spline.correlog(x = M_Mass_Length[mml.sampl$rowname,"Lon"]$Lon,
                                 y = M_Mass_Length[mml.sampl$rowname,"Lat"]$Lat,                                   
                                 z = mml.sampl[,2], 
                                 resamp = 1000, latlon = T, xmax = 1000)
plot(sp.corel ,main = paste("Mammal Mass:Length Spatial Correlation"))

ggsave("mass_length_mammal_autocor.jpg",
       path="D:/Thesis Projects/GCB Revised Manuscript/figures/New figs land use")

M_Mass_ml <- M_Mass_Length 

M_Mass_ml$Resid <- residuals(phylo_m_m_au)

ggplot(M_Mass_ml, aes(x=Year, y=Resid))+geom_point(alpha=0.1, size=1)+geom_smooth(color="red")+
  theme_bw()+xlab("Year")+ylab("Residual")

ggsave("mass_length_mammal_resid_year.jpg",
       path="D:/Thesis Projects/GCB Revised Manuscript/figures/New figs land use")
#bird mass
bm.sampl <- sample_n(residuals(phylo_b_au) %>% as.data.frame() %>% tibble::rownames_to_column(), 3000)%>%
  mutate(rowname = as.numeric(rowname))
sp.corel <- ncf::spline.correlog(x = M_Mass_tree2[bm.sampl$rowname,"Lon"]$Lon,
                                 y = M_Mass_tree2[bm.sampl$rowname,"Lat"]$Lat,                                   
                                 z = bm.sampl[,2], 
                                 resamp = 1000, latlon = T, xmax = 1000)
plot(sp.corel ,main = paste("Bird Mass Spatial Correlation"))

ggsave("mass_bird_autocor.jpg",
       path="D:/Thesis Projects/GCB Revised Manuscript/figures/New figs land use")

B_Mass_m <- B_Mass_tree2 

B_Mass_m$Resid <- residuals(phylo_b_au)

ggplot(B_Mass_m, aes(x=Year, y=Resid))+geom_point(alpha=0.1, size=1)+geom_smooth(color="red")+
  theme_bw()+xlab("Year")+ylab("Residual")

ggsave("mass_bird_resid_year.jpg",
       path="D:/Thesis Projects/GCB Revised Manuscript/figures/New figs land use")

#bird length
bl.sampl <- sample_n(residuals(phylo_b_l_au) %>% as.data.frame() %>% tibble::rownames_to_column(), 3000)%>%
  mutate(rowname = as.numeric(rowname))
sp.corel <- ncf::spline.correlog(x = B_Length_tree2[bl.sampl$rowname,"Lon"]$Lon,
                                 y = B_Length_tree2[bl.sampl$rowname,"Lat"]$Lat,                                   
                                 z = bl.sampl[,2], 
                                 resamp = 1000, latlon = T, xmax = 1000)
plot(sp.corel ,main = paste("Bird Length Spatial Correlation"))


B_Length_m <- B_Length_tree2

B_Length_m$Resid <- residuals(phylo_b_l_au)

ggplot(B_Length_m, aes(x=Year, y=Resid))+geom_point(alpha=0.1, size=1)+geom_smooth(color="red")+
  theme_bw()+xlab("Year")+ylab("Residual")

ggsave("length_bird_resid_year.jpg",
       path="D:/Thesis Projects/GCB Revised Manuscript/figures/New figs land use")

#bird size
bml.sampl <- sample_n(residuals(phylo_b_ml_au) %>% as.data.frame() %>% tibble::rownames_to_column(), 3000)%>%
  mutate(rowname = as.numeric(rowname))
sp.corel <- ncf::spline.correlog(x = B_Mass_Length[bml.sampl$rowname,"Lon"]$Lon,
                                 y = B_Mass_Length[bml.sampl$rowname,"Lat"]$Lat,                                   
                                 z = bml.sampl[,2], 
                                 resamp = 1000, latlon = T, xmax = 1000)
plot(sp.corel ,main = paste("Bird Mass:Length Spatial Correlation"))
ggsave("mass_length_bird_autocor.jpg",
       path="D:/Thesis Projects/GCB Revised Manuscript/figures/New figs land use")

B_Mass_ml <- B_Mass_Length 

B_Mass_ml$Resid <- residuals(phylo_b_ml_au)

ggplot(B_Mass_ml, aes(x=Year, y=Resid))+geom_point(alpha=0.1, size=1)+geom_smooth(color="red")+
  theme_bw()+xlab("Year")+ylab("Residual")

ggsave("mass_length_bird_resid_year.jpg",
       path="D:/Thesis Projects/GCB Revised Manuscript/figures/New figs land use")

##no year check

phylo_b_au_no_year <- pglmm(LMass ~ TPI_Max + API_w + ALU + ULU
                            + TPI_Max:API_w + TPI_Max:ALU + TPI_Max:ULU
                            + (TPI_Max|Binomial__)
                            + (API_w|Binomial__)
                            + (ALU|Binomial__)
                            + (ULU|Binomial__)
                            + (1|Binomial__) + (1|Realm) + (1|Site), 
                            data=B_Mass_tree2, cov_ranef = list(Binomial = bird_tree_mass), 
                            bayes = TRUE)
summary(phylo_b_au_no_year)

phylo_b_l_au_no_year <- pglmm(LLength ~ TPI_Max + API + ALU + ULU
                              + TPI_Max:API + TPI_Max:ALU + TPI_Max:ULU
                              + (TPI_Max|Binomial__)
                              + (API|Binomial__)
                              + (ALU|Binomial__)
                              + (ULU|Binomial__)
                              + (1|Binomial__)
                              +  (1|Realm) + (1|Site), 
                              data=B_Length_tree2, cov_ranef = list(Binomial = bird_tree_length), 
                              bayes = TRUE)

summary(phylo_b_l_au_no_year)

phylo_b_ml_au_no_year <- pglmm(Body_Size_w ~ TPI_Max + API+ ALU + ULU
                               + TPI_Max:API + TPI_Max:ALU + TPI_Max:ULU
                               + (TPI_Max|Binomial__)
                               + (API|Binomial__)
                               + (ALU|Binomial__)
                               + (ULU|Binomial__)
                               + (1|Binomial__)
                               +  (1|Realm) + (1|Site), 
                               data=B_Mass_Length, cov_ranef = list(Binomial = bird_tree_mass2), 
                               bayes = TRUE)
summary(phylo_b_ml_au_no_year)

#mammal

phylo_m_au_no_year <- pglmm(LMass ~ TPI_Max + API + ALU + ULU
                            + TPI_Max:API + TPI_Max:ALU + TPI_Max:ULU
                            + (TPI_Max|Binomial__)
                            + (API|Binomial__)
                            + (ALU|Binomial__)
                            + (ULU|Binomial__)
                            + (1|Binomial__) + (1|Realm) + (1|Site), 
                            data=M_Mass_tree2, cov_ranef = list(Binomial = mam_tree_mass), 
                            bayes = TRUE)
summary(phylo_m_au_no_year)

phylo_m_l_au_no_year <- pglmm(LLength ~ TPI_Max + API + ALU + ULU
                              + TPI_Max:API + TPI_Max:ALU + TPI_Max:ULU
                              + (TPI_Max|Binomial__)
                              + (API|Binomial__)
                              + (ALU|Binomial__)
                              + (ULU|Binomial__)
                              + (1|Binomial__)
                              +  (1|Realm) + (1|Site), 
                              data=M_Length_tree2, cov_ranef = list(Binomial = mam_tree_length), 
                              bayes = TRUE)

summary(phylo_m_l_au_no_year)

phylo_m_ml_au_no_year <- pglmm(Body_Size_w ~ TPI_Max + API + ALU + ULU
                               + TPI_Max:API + TPI_Max:ALU+ TPI_Max:ULU 
                               + (TPI_Max|Binomial__)
                               + (API|Binomial__)
                               + (ALU|Binomial__)
                               + (ULU|Binomial__)
                               + (1|Binomial__)
                               +  (1|Realm) + (1|Site), 
                               data=M_Mass_Length, cov_ranef = list(Binomial = mam_tree_mass2), 
                               bayes = TRUE)
summary(phylo_m_ml_au_no_year)
