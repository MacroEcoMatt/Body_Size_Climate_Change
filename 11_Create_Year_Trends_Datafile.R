#' ---
#' Matthew Watson
#' conducts Linear models and correlations for analysis of body size trends over time
#' conducts Linear models and correlations for analysis of environmental trends over time
#' ---
library(vroom)
library(dplyr)
library(sjmisc)
library(sjPlot)
library(nlme)
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggthemes)
library(performance)
setwd("") #set to where model output files will be stored
###############################MAMMAL ANALYSIS#############################################
#change file path to location of datafiles
#MAMMALS
M_Mass <- read.csv("./Data_S4_Mammal_Mass_Official.csv")
M_Length <- read.csv("./Data_S5_Mammal_Length_Official.csv")
M_Size<- read.csv("./Data_S6_Mammal_Size_Official3.csv")

####year
#do lmer model with random slope and int
mass_lme <- lmer(LMass ~ Year_sc + (Year_sc|Binomial),data=M_Mass,REML=F,
                 control = lmerControl(optimizer = "optimx", 
                                       calc.derivs = FALSE, 
                                       optCtrl = list(method = "nlminb")))
tab_model(mass_lme, digits=5, file="C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mam_mass_year_model.html")

length_lme <- lmer(LLength ~ Year_sc + (Year_sc|Binomial),data=M_Length,REML=F,
                   control = lmerControl(calc.derivs = FALSE))

tab_model(length_lme, digits=5, file="C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mam_length_year_model.html")

size_lme <- lmer(LSize ~ Year_sc + (Year_sc|Binomial),data=M_Size2,REML=F,
                 control = lmerControl(calc.derivs = FALSE))

tab_model(size_lme, digits=5, file="C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mam_size_year_model.html")

#get variables
mass_ran <- broom.mixed::tidy(mass_lme, effects="ran_vals",conf.int=T)
mass_ran_yr <- mass_ran%>%filter(term=="Year_sc")

len_ran <- broom.mixed::tidy(length_lme, effects="ran_vals",conf.int=T)
len_ran_yr <- len_ran%>%filter(term=="Year_sc")

size_ran <- broom.mixed::tidy(size_lme, effects="ran_vals",conf.int=T)
size_ran_yr <- size_ran%>%filter(term=="Year_sc")

#do seperate lm for each sp
mass_year <- M_Mass %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(LMass ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)

mass_year_sig <- mass_year %>% filter(term=="Year_sc" & !is.na(p.value))%>%
  mutate(siglevel = ifelse(p.value <0.05, "Sig","Not Sig"))

length_year <- M_Length %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(LLength ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)

length_year_sig <- length_year %>% filter(term=="Year_sc" & !is.na(p.value))%>%
  mutate(siglevel = ifelse(p.value <0.05, "Sig","Not Sig"))

size_year <- M_Size2 %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(LSize ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)

size_year_sig <- size_year %>% filter(term=="Year_sc" & !is.na(p.value))%>%
  mutate(siglevel = ifelse(p.value <0.05, "Sig","Not Sig"))


#combine estimates
lm_values <- mass_year_sig[,c("Binomial", "estimate", "siglevel")]
lm_valuesl <- length_year_sig[,c("Binomial", "estimate", "siglevel")]
lm_valuess <- size_year_sig[,c("Binomial", "estimate", "siglevel")]

colnames(lm_values) <- c("Binomial","lm_estimate","lm_sig")
colnames(mass_ran_yr)[3] <- "Binomial"

colnames(lm_valuesl) <- c("Binomial","lm_estimate","lm_sig")
colnames(len_ran_yr)[3] <- "Binomial"

colnames(lm_valuess) <- c("Binomial","lm_estimate","lm_sig")
colnames(size_ran_yr)[3] <- "Binomial"


mass_ran_yr <- left_join(mass_ran_yr,lm_values) %>% mutate(lme_sig = ifelse(conf.low < 0 & conf.high < 0 | 
                                                                              conf.low > 0 & conf.high > 0,
                                                                            "Sig","Not Sig"),
                                                           model_diff = estimate/lm_estimate,
                                                           model_match = ifelse(lm_sig== "Sig" & lme_sig=="Sig", "Both Sig",
                                                                                ifelse(lm_sig=="Sig" & lme_sig=="Not Sig", "LM Sig",
                                                                                       ifelse(lm_sig=="Not Sig" & lme_sig=="Sig", "LME Sig", "Not Sig"))))

outliers_year_lm <- boxplot(mass_ran_yr$lm_estimate, plot=F)$out
outliers_year_lme <- boxplot(mass_ran_yr$estimate, plot=F)$out

mass_yr_final <- mass_ran_yr %>% mutate(out_lier = ifelse(lm_estimate %in% outliers_year_lm|
                                                            estimate %in% outliers_year_lme, "Outlier", "Not Outlier"))

len_ran_yr <- left_join(len_ran_yr,lm_valuesl) %>% mutate(lme_sig = ifelse(conf.low < 0 & conf.high < 0 | 
                                                                             conf.low > 0 & conf.high > 0,
                                                                           "Sig","Not Sig"),
                                                          model_diff = estimate/lm_estimate,
                                                          model_match = ifelse(lm_sig== "Sig" & lme_sig=="Sig", "Both Sig",
                                                                               ifelse(lm_sig=="Sig" & lme_sig=="Not Sig", "LM Sig",
                                                                                      ifelse(lm_sig=="Not Sig" & lme_sig=="Sig", "LME Sig", "Not Sig"))))

outliers_year_lm <- boxplot(len_ran_yr$lm_estimate, plot=F)$out
outliers_year_lme <- boxplot(len_ran_yr$estimate, plot=F)$out

len_yr_final <- len_ran_yr %>% mutate(out_lier = ifelse(lm_estimate %in% outliers_year_lm|
                                                          estimate %in% outliers_year_lme, "Outlier", "Not Outlier"))


size_ran_yr <- left_join(size_ran_yr,lm_valuess) %>% mutate(lme_sig = ifelse(conf.low < 0 & conf.high < 0 | 
                                                                               conf.low > 0 & conf.high > 0,
                                                                             "Sig","Not Sig"),
                                                            model_diff = estimate/lm_estimate,
                                                            model_match = ifelse(lm_sig== "Sig" & lme_sig=="Sig", "Both Sig",
                                                                                 ifelse(lm_sig=="Sig" & lme_sig=="Not Sig", "LM Sig",
                                                                                        ifelse(lm_sig=="Not Sig" & lme_sig=="Sig", "LME Sig", "Not Sig"))))

outliers_year_lm <- boxplot(size_ran_yr$lm_estimate, plot=F)$out
outliers_year_lme <- boxplot(size_ran_yr$estimate, plot=F)$out

size_yr_final <- size_ran_yr %>% mutate(out_lier = ifelse(lm_estimate %in% outliers_year_lm|
                                                            estimate %in% outliers_year_lme, "Outlier", "Not Outlier"))

#tpi by year lm and cor
tpi_year <-  M_Mass %>% group_by(Binomial) %>%
  summarise(cor(Year_sc, TPI_month_max, method = "pearson"))
colnames(tpi_year)[2] <- "TPIcor"
tpi_year_lm <- M_Mass %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(TPI_month_max ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
tpi_year_lm <- tpi_year_lm %>% mutate(ifelse(p.value<0.05,"Sig","Not Sig"))%>%filter(!is.na(statistic))
tpicor <- left_join(tpi_year_lm, tpi_year)
colnames(tpicor)[c(3,7)] <- c("TPI_trend","TPI_sig")
tpicor <- tpicor[,c(1,3,7,8)]

tpi_yearl <-  M_Length %>% group_by(Binomial) %>%
  summarise(cor(Year_sc, TPI_month_max, method = "pearson"))
colnames(tpi_yearl)[2] <- "TPIcor"
tpi_year_lml <- M_Length %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(TPI_month_max ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
tpi_year_lml <- tpi_year_lml %>% mutate(ifelse(p.value<0.05,"Sig","Not Sig"))%>%filter(!is.na(statistic))
tpicorl <- left_join(tpi_year_lml, tpi_yearl)
colnames(tpicorl)[c(3,7)] <- c("TPI_trend","TPI_sig")
tpicorl <- tpicorl[,c(1,3,7,8)]

tpi_years <-  M_Size2 %>% group_by(Binomial) %>%
  summarise(cor(Year_sc, TPI_month_max, method = "pearson"))
colnames(tpi_years)[2] <- "TPIcor"
tpi_year_lms <- M_Size2 %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(TPI_month_max ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
tpi_year_lms <- tpi_year_lms %>% mutate(ifelse(p.value<0.05,"Sig","Not Sig"))%>%filter(!is.na(statistic))
tpicors <- left_join(tpi_year_lms, tpi_years)
colnames(tpicors)[c(3,7)] <- c("TPI_trend","TPI_sig")
tpicors <- tpicors[,c(1,3,7,8)]

#ai by year lm and cor
ai_year <-  M_Mass %>% group_by(Binomial) %>%
  summarise(cor(Year_sc, AI, method = "pearson"))
colnames(ai_year)[2] <- "AIcor"
ai_year_lm <- M_Mass %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(AI ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
ai_year_lm <- ai_year_lm %>% mutate(ifelse(p.value<0.05,"Sig","Not Sig"))%>%filter(!is.na(statistic))
aicor <- left_join(ai_year_lm, ai_year)
colnames(aicor)[c(3,7)] <- c("AI_trend","AI_sig")
aicor <- aicor[,c(1,3,7,8)]

ai_yearl <-  M_Length %>% group_by(Binomial) %>%
  summarise(cor(Year_sc, AI, method = "pearson"))
colnames(ai_yearl)[2] <- "AIcor"
ai_year_lml <- M_Length %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(AI ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
ai_year_lml <- ai_year_lml %>% mutate(ifelse(p.value<0.05,"Sig","Not Sig"))%>%filter(!is.na(statistic))
aicorl <- left_join(ai_year_lml, ai_yearl)
colnames(aicorl)[c(3,7)] <- c("AI_trend","AI_sig")
aicorl <- aicorl[,c(1,3,7,8)]

ai_years <-  M_Size2 %>% group_by(Binomial) %>%
  summarise(cor(Year_sc, AI, method = "pearson"))
colnames(ai_years)[2] <- "AIcor"
ai_year_lms <- M_Size2 %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(AI ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
ai_year_lms <- rbind(ai_year_lms, ai_year_lmes)
ai_year_lms <- ai_year_lms %>% mutate(ifelse(p.value<0.05,"Sig","Not Sig"))%>%filter(!is.na(statistic))
aicors <- left_join(ai_year_lms, ai_years)
colnames(aicors)[c(3,7)] <- c("AI_trend","AI_sig")
aicors <- aicors[,c(1,3,7,8)]

#hlu by year lm and cor
hlu_year <- M_Mass %>% group_by(Binomial) %>%
  summarise(cor(Year_sc, HLU, method = "pearson"))
colnames(hlu_year)[2] <- "HLUcor"
hlu_year_lm <- M_Mass %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(HLU ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
hlu_year_lm <- hlu_year_lm %>% mutate(ifelse(p.value<0.05,"Sig","Not Sig"))%>%filter(!is.na(statistic))
hlucor <- left_join(hlu_year_lm, hlu_year)
colnames(hlucor)[c(3,7)] <- c("HLU_trend","HLU_sig")
hlucor <- hlucor[,c(1,3,7,8)]

hlu_yearl <- M_Length %>% group_by(Binomial) %>%
  summarise(cor(Year_sc, HLU, method = "pearson"))
colnames(hlu_yearl)[2] <- "HLUcor"
hlu_year_lml <- M_Length %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(HLU ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
hlu_year_lml <- hlu_year_lml %>% mutate(ifelse(p.value<0.05,"Sig","Not Sig"))%>%filter(!is.na(statistic))
hlucorl <- left_join(hlu_year_lml, hlu_yearl)
colnames(hlucorl)[c(3,7)] <- c("HLU_trend","HLU_sig")
hlucorl <- hlucorl[,c(1,3,7,8)]

hlu_years <- M_Size2 %>% group_by(Binomial) %>%
  summarise(cor(Year_sc, HLU, method = "pearson"))
colnames(hlu_years)[2] <- "HLUcor"
hlu_year_lms <- M_Size2 %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(HLU ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
hlu_year_lms <- hlu_year_lms %>% mutate(ifelse(p.value<0.05,"Sig","Not Sig"))%>%filter(!is.na(statistic))
hlucors <- left_join(hlu_year_lms, hlu_years)
colnames(hlucors)[c(3,7)] <- c("HLU_trend","HLU_sig")
hlucors <- hlucors[,c(1,3,7,8)]

mass_yr_final <- left_join(mass_yr_final,tpicor)
mass_yr_final<- left_join(mass_yr_final,aicor)
mass_yr_final <- left_join(mass_yr_final,hlucor)%>%filter(!is.na(HLU_trend))

len_yr_final <- left_join(len_yr_final,tpicorl)%>%filter(!is.na(TPI_trend))
len_yr_final<- left_join(len_yr_final,aicorl)%>%filter(!is.na(AI_trend))
len_yr_final <- left_join(len_yr_final,hlucorl)%>%filter(!is.na(HLU_trend))

size_yr_final <- left_join(size_yr_final,tpicors)%>%filter(!is.na(TPI_trend))
size_yr_final<- left_join(size_yr_final,aicors)%>%filter(!is.na(AI_trend))
size_yr_final <- left_join(size_yr_final,hlucors)%>%filter(!is.na(HLU_trend))

#add absolute corr level
mass_yr_final <- mass_yr_final %>% mutate(temp_trend = ifelse(TPIcor > 0 & TPI_trend > 0 & TPI_sig=="Sig", "Hotter",
                                                              ifelse(TPIcor < 0 & TPI_trend < 0 & TPI_sig =="Sig","Colder","No Change")),
                                          water_trend = ifelse(AI_trend > 0 & AIcor > 0 & AI_sig=="Sig", "Wetter",
                                                               ifelse(AI_trend < 0 & AIcor < 0 & AI_sig =="Sig","Dryer","No Change")),
                                          land_trend = ifelse(HLU_trend > 0 &HLUcor > 0 & HLU_sig=="Sig", "More Land Use",
                                                              ifelse(HLU_trend < 0 & HLUcor < 0 & HLU_sig =="Sig","Less Land Use","No Change in Land Use")),
                                          size_trend_lm = ifelse(lm_estimate > 0 & lm_sig=="Sig", "Larger",
                                                                 ifelse(lm_estimate < 0 & lm_sig =="Sig","Smaller","No Change")),
                                          size_trend_lme = ifelse(estimate > 0 & lme_sig=="Sig", "Larger",
                                                                  ifelse(estimate < 0 & lme_sig =="Sig","Smaller","No Change")))

len_yr_final <- len_yr_final %>% mutate(temp_trend = ifelse(TPIcor > 0 & TPI_trend > 0 & TPI_sig=="Sig", "Hotter",
                                                            ifelse(TPIcor < 0 & TPI_trend < 0 & TPI_sig =="Sig","Colder","No Change")),
                                        water_trend = ifelse(AI_trend > 0 & AIcor > 0 & AI_sig=="Sig", "Wetter",
                                                             ifelse(AI_trend < 0 & AIcor < 0 & AI_sig =="Sig","Dryer","No Change")),
                                        land_trend = ifelse(HLU_trend > 0 &HLUcor > 0 & HLU_sig=="Sig", "More Land Use",
                                                            ifelse(HLU_trend < 0 & HLUcor < 0 & HLU_sig =="Sig","Less Land Use","No Change in Land Use")),
                                        size_trend_lm = ifelse(lm_estimate > 0 & lm_sig=="Sig", "Larger",
                                                               ifelse(lm_estimate < 0 & lm_sig =="Sig","Smaller","No Change")),
                                        size_trend_lme = ifelse(estimate > 0 & lme_sig=="Sig", "Larger",
                                                                ifelse(estimate < 0 & lme_sig =="Sig","Smaller","No Change")))

size_yr_final <- size_yr_final %>% mutate(temp_trend = ifelse(TPIcor > 0 & TPI_trend > 0 & TPI_sig=="Sig", "Hotter",
                                                              ifelse(TPIcor < 0 & TPI_trend < 0 & TPI_sig =="Sig","Colder","No Change")),
                                          water_trend = ifelse(AI_trend > 0 & AIcor > 0 & AI_sig=="Sig", "Wetter",
                                                               ifelse(AI_trend < 0 & AIcor < 0 & AI_sig =="Sig","Dryer","No Change")),
                                          land_trend = ifelse(HLU_trend > 0 &HLUcor > 0 & HLU_sig=="Sig", "More Land Use",
                                                              ifelse(HLU_trend < 0 & HLUcor < 0 & HLU_sig =="Sig","Less Land Use","No Change in Land Use")),
                                          size_trend_lm = ifelse(lm_estimate > 0 & lm_sig=="Sig", "Larger",
                                                                 ifelse(lm_estimate < 0 & lm_sig =="Sig","Smaller","No Change")),
                                          size_trend_lme = ifelse(estimate > 0 & lme_sig=="Sig", "Larger",
                                                                  ifelse(estimate < 0 & lme_sig =="Sig","Smaller","No Change")))


#filter to necessary columns
mass_yr_final_mam <- mass_yr_final[,-c(1,2)]
write.csv(mass_yr_final, "./Mam_Year_Trends_mass.csv")
len_yr_final_mam <- len_yr_final[,-c(1,2)]
write.csv(len_yr_final, "./Mam_Year_Trends_length.csv")
size_yr_final_mam <- size_yr_final[,-c(1,2)]
write.csv(size_yr_final, "./Mam_Year_Trends_size.csv")

###############BIRDS#######################################
B_Mass <- vroom("./Data_S1_Bird_Mass_Official.csv")%>%mutate(LMass = log10(Mass))
B_Length <- vroom("./Data_S2_Bird_Length_Official.csv")%>%mutate(LLength = log10(Body_Length))%>%filter(AI<75)
B_Size <- vroom("./Data_S3_Bird_Size_Official.csv")%>%mutate(LSize = log10(Mass)/log10(Body_Length))%>%filter(AI<75)


####year
#do lmer model with random slope and int
mass_lme <- lmer(log10(Mass) ~ Year_sc + (Year_sc|Binomial),data=B_Mass, REML=F,
                 control = lmerControl(calc.derivs = FALSE))
                 
tab_model(mass_lme, digits=5, file="./Results/Bird_mass_year_model.html")

length_lme <- lmer(log10(Body_Length) ~ Year_sc + (Year_sc|Binomial),data=B_Length,REML=F,
                   control = lmerControl(calc.derivs = FALSE))

tab_model(length_lme, digits=5, file="./Results/Bird_length_year_model.html")

size_lme <- lmer((log10(Mass)/log10(Body_Length)) ~ Year_sc + (Year_sc|Binomial),data=B_Size,REML=F,
                 control = lmerControl(calc.derivs = FALSE))

tab_model(size_lme, digits=5, file="./Results/Bird_size_year_model.html")

#get variables
mass_ran <- broom.mixed::tidy(mass_lme, effects="ran_vals",conf.int=T)
mass_ran_yr <- mass_ran%>%filter(term=="Year_sc")

len_ran <- broom.mixed::tidy(length_lme, effects="ran_vals",conf.int=T)
len_ran_yr <- len_ran%>%filter(term=="Year_sc")

size_ran <- broom.mixed::tidy(size_lme, effects="ran_vals",conf.int=T)
size_ran_yr <- size_ran%>%filter(term=="Year_sc")

#do seperate lm for each sp
mass_year <- B_Mass %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(log10(Mass) ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)

mass_year_sig <- mass_year %>% filter(term=="Year_sc" & !is.na(p.value))%>%
  mutate(siglevel = ifelse(p.value <0.05, "Sig","Not Sig"))

length_year <- B_Length %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(log10(Body_Length) ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)

length_year_sig <- length_year %>% filter(term=="Year_sc" & !is.na(p.value))%>%
  mutate(siglevel = ifelse(p.value <0.05, "Sig","Not Sig"))

size_year <- B_Size %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm((log10(Mass)/log10(Body_Length)) ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)

size_year_sig <- size_year %>% filter(term=="Year_sc" & !is.na(p.value))%>%
  mutate(siglevel = ifelse(p.value <0.05, "Sig","Not Sig"))

#combine estimates
lm_values <- mass_year_sig[,c("Binomial", "estimate", "siglevel")]
lm_valuesl <- length_year_sig[,c("Binomial", "estimate", "siglevel")]
lm_valuess <- size_year_sig[,c("Binomial", "estimate", "siglevel")]

colnames(lm_values) <- c("Binomial","lm_estimate","lm_sig")
colnames(mass_ran_yr)[3] <- "Binomial"

colnames(lm_valuesl) <- c("Binomial","lm_estimate","lm_sig")
colnames(len_ran_yr)[3] <- "Binomial"

colnames(lm_valuess) <- c("Binomial","lm_estimate","lm_sig")
colnames(size_ran_yr)[3] <- "Binomial"


mass_ran_yr <- left_join(mass_ran_yr,lm_values) %>% mutate(lme_sig = ifelse(conf.low < 0 & conf.high < 0 | 
                                                                              conf.low > 0 & conf.high > 0,
                                                                            "Sig","Not Sig"),
                                                           model_diff = estimate/lm_estimate,
                                                           model_match = ifelse(lm_sig== "Sig" & lme_sig=="Sig", "Both Sig",
                                                                                ifelse(lm_sig=="Sig" & lme_sig=="Not Sig", "LM Sig",
                                                                                       ifelse(lm_sig=="Not Sig" & lme_sig=="Sig", "LME Sig", "Not Sig"))))

outliers_year_lm <- boxplot(mass_ran_yr$lm_estimate, plot=F)$out
outliers_year_lme <- boxplot(mass_ran_yr$estimate, plot=F)$out

mass_yr_final <- mass_ran_yr %>% mutate(out_lier = ifelse(lm_estimate %in% outliers_year_lm|
                                                            estimate %in% outliers_year_lme, "Outlier", "Not Outlier"))

len_ran_yr <- left_join(len_ran_yr,lm_valuesl) %>% mutate(lme_sig = ifelse(conf.low < 0 & conf.high < 0 | 
                                                                             conf.low > 0 & conf.high > 0,
                                                                           "Sig","Not Sig"),
                                                          model_diff = estimate/lm_estimate,
                                                          model_match = ifelse(lm_sig== "Sig" & lme_sig=="Sig", "Both Sig",
                                                                               ifelse(lm_sig=="Sig" & lme_sig=="Not Sig", "LM Sig",
                                                                                      ifelse(lm_sig=="Not Sig" & lme_sig=="Sig", "LME Sig", "Not Sig"))))

outliers_year_lm <- boxplot(len_ran_yr$lm_estimate, plot=F)$out
outliers_year_lme <- boxplot(len_ran_yr$estimate, plot=F)$out

len_yr_final <- len_ran_yr %>% mutate(out_lier = ifelse(lm_estimate %in% outliers_year_lm|
                                                          estimate %in% outliers_year_lme, "Outlier", "Not Outlier"))


size_ran_yr <- left_join(size_ran_yr,lm_valuess) %>% mutate(lme_sig = ifelse(conf.low < 0 & conf.high < 0 | 
                                                                               conf.low > 0 & conf.high > 0,
                                                                             "Sig","Not Sig"),
                                                            model_diff = estimate/lm_estimate,
                                                            model_match = ifelse(lm_sig== "Sig" & lme_sig=="Sig", "Both Sig",
                                                                                 ifelse(lm_sig=="Sig" & lme_sig=="Not Sig", "LM Sig",
                                                                                        ifelse(lm_sig=="Not Sig" & lme_sig=="Sig", "LME Sig", "Not Sig"))))

outliers_year_lm <- boxplot(size_ran_yr$lm_estimate, plot=F)$out
outliers_year_lme <- boxplot(size_ran_yr$estimate, plot=F)$out

size_yr_final <- size_ran_yr %>% mutate(out_lier = ifelse(lm_estimate %in% outliers_year_lm|
                                                            estimate %in% outliers_year_lme, "Outlier", "Not Outlier"))

#tpi by year lm and cor
tpi_year <-  B_Mass %>% group_by(Binomial) %>%
  summarise(cor(Year_sc, TPI_month_max, method = "pearson"))
colnames(tpi_year)[2] <- "TPIcor"
tpi_year_lm <- B_Mass %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(TPI_month_max ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
tpi_year_lm <- tpi_year_lm %>% mutate(ifelse(p.value<0.05,"Sig","Not Sig"))%>%filter(!is.na(statistic))
tpicor <- left_join(tpi_year_lm, tpi_year)
colnames(tpicor)[c(3,7)] <- c("TPI_trend","TPI_sig")
tpicor <- tpicor[,c(1,3,7,8)]

tpi_yearl <-  B_Length %>% group_by(Binomial) %>%
  summarise(cor(Year_sc, TPI_month_max, method = "pearson"))
colnames(tpi_yearl)[2] <- "TPIcor"
tpi_year_lml <- B_Length %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(TPI_month_max ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
tpi_year_lml <- tpi_year_lml %>% mutate(ifelse(p.value<0.05,"Sig","Not Sig"))%>%filter(!is.na(statistic))
tpicorl <- left_join(tpi_year_lml, tpi_yearl)
colnames(tpicorl)[c(3,7)] <- c("TPI_trend","TPI_sig")
tpicorl <- tpicorl[,c(1,3,7,8)]

tpi_years <- B_Size %>% group_by(Binomial) %>%
  summarise(cor(Year_sc, TPI_month_max, method = "pearson"))
colnames(tpi_years)[2] <- "TPIcor"
tpi_year_lms <- B_Size %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(TPI_month_max ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
tpi_year_lms <- tpi_year_lms %>% mutate(ifelse(p.value<0.05,"Sig","Not Sig"))%>%filter(!is.na(statistic))
tpicors <- left_join(tpi_year_lms, tpi_years)
colnames(tpicors)[c(3,7)] <- c("TPI_trend","TPI_sig")
tpicors <- tpicors[,c(1,3,7,8)]

#ai by year lm and cor
ai_year <-  B_Mass %>% group_by(Binomial) %>%
  summarise(cor(Year_sc, AI, method = "pearson"))
colnames(ai_year)[2] <- "AIcor"
ai_year_lm <- B_Mass %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(AI ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
ai_year_lm <- ai_year_lm %>% mutate(ifelse(p.value<0.05,"Sig","Not Sig"))%>%filter(!is.na(statistic))
aicor <- left_join(ai_year_lm, ai_year)
colnames(aicor)[c(3,7)] <- c("AI_trend","AI_sig")
aicor <- aicor[,c(1,3,7,8)]

ai_yearl <-  B_Length %>% group_by(Binomial) %>%
  summarise(cor(Year_sc, AI, method = "pearson"))
colnames(ai_yearl)[2] <- "AIcor"
ai_year_lml <- B_Length %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(AI ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
ai_year_lml <- ai_year_lml %>% mutate(ifelse(p.value<0.05,"Sig","Not Sig"))%>%filter(!is.na(statistic))
aicorl <- left_join(ai_year_lml, ai_yearl)
colnames(aicorl)[c(3,7)] <- c("AI_trend","AI_sig")
aicorl <- aicorl[,c(1,3,7,8)]

ai_years <-  B_Size %>% group_by(Binomial) %>%
  summarise(cor(Year_sc, AI, method = "pearson"))
colnames(ai_years)[2] <- "AIcor"
ai_year_lms <- B_Size %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(AI ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
ai_year_lms <- ai_year_lms %>% mutate(ifelse(p.value<0.05,"Sig","Not Sig"))%>%filter(!is.na(statistic))
aicors <- left_join(ai_year_lms, ai_years)
colnames(aicors)[c(3,7)] <- c("AI_trend","AI_sig")
aicors <- aicors[,c(1,3,7,8)]

#hlu by year lm and cor
hlu_year <- B_Mass %>% group_by(Binomial) %>%
  summarise(cor(Year_sc, HLU, method = "pearson"))
colnames(hlu_year)[2] <- "HLUcor"
hlu_year_lm <- B_Mass %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(HLU ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
hlu_year_lm <- hlu_year_lm %>% mutate(ifelse(p.value<0.05,"Sig","Not Sig"))%>%filter(!is.na(statistic))
hlucor <- left_join(hlu_year_lm, hlu_year)
colnames(hlucor)[c(3,7)] <- c("HLU_trend","HLU_sig")
hlucor <- hlucor[,c(1,3,7,8)]

hlu_yearl <- B_Length %>% group_by(Binomial) %>%
  summarise(cor(Year_sc, HLU, method = "pearson"))
colnames(hlu_yearl)[2] <- "HLUcor"
hlu_year_lml <- B_Length %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(HLU ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
hlu_year_lml <- hlu_year_lml %>% mutate(ifelse(p.value<0.05,"Sig","Not Sig"))%>%filter(!is.na(statistic))
hlucorl <- left_join(hlu_year_lml, hlu_yearl)
colnames(hlucorl)[c(3,7)] <- c("HLU_trend","HLU_sig")
hlucorl <- hlucorl[,c(1,3,7,8)]

hlu_years <- B_Size %>% group_by(Binomial) %>%
  summarise(cor(Year_sc, HLU, method = "pearson"))
colnames(hlu_years)[2] <- "HLUcor"
hlu_year_lms <- B_Size %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(HLU ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
hlu_year_lms <- hlu_year_lms %>% mutate(ifelse(p.value<0.05,"Sig","Not Sig"))%>%filter(!is.na(statistic))
hlucors <- left_join(hlu_year_lms, hlu_years)
colnames(hlucors)[c(3,7)] <- c("HLU_trend","HLU_sig")
hlucors <- hlucors[,c(1,3,7,8)]

mass_yr_final <- left_join(mass_yr_final,tpicor)
mass_yr_final<- left_join(mass_yr_final,aicor)
mass_yr_final <- left_join(mass_yr_final,hlucor)%>%filter(!is.na(HLU_trend))

len_yr_final <- left_join(len_yr_final,tpicorl)%>%filter(!is.na(TPI_trend))
len_yr_final<- left_join(len_yr_final,aicorl)%>%filter(!is.na(AI_trend))
len_yr_final <- left_join(len_yr_final,hlucorl)%>%filter(!is.na(HLU_trend))

size_yr_final <- left_join(size_yr_final,tpicors)%>%filter(!is.na(TPI_trend))
size_yr_final<- left_join(size_yr_final,aicors)%>%filter(!is.na(AI_trend))
size_yr_final <- left_join(size_yr_final,hlucors)%>%filter(!is.na(HLU_trend))

#add absolute corr level
mass_yr_final <- mass_yr_final %>% mutate(temp_trend = ifelse(TPIcor > 0 & TPI_trend > 0 & TPI_sig=="Sig", "Hotter",
                                                              ifelse(TPIcor < 0 & TPI_trend < 0 & TPI_sig =="Sig","Colder","No Change")),
                                          water_trend = ifelse(AI_trend > 0 & AIcor > 0 & AI_sig=="Sig", "Wetter",
                                                               ifelse(AI_trend < 0 & AIcor < 0 & AI_sig =="Sig","Dryer","No Change")),
                                          land_trend = ifelse(HLU_trend > 0 &HLUcor > 0 & HLU_sig=="Sig", "More Land Use",
                                                              ifelse(HLU_trend < 0 & HLUcor < 0 & HLU_sig =="Sig","Less Land Use","No Change in Land Use")),
                                          size_trend_lm = ifelse(lm_estimate > 0 & lm_sig=="Sig", "Larger",
                                                                 ifelse(lm_estimate < 0 & lm_sig =="Sig","Smaller","No Change")),
                                          size_trend_lme = ifelse(estimate > 0 & lme_sig=="Sig", "Larger",
                                                                  ifelse(estimate < 0 & lme_sig =="Sig","Smaller","No Change")))

len_yr_final <- len_yr_final %>% mutate(temp_trend = ifelse(TPIcor > 0 & TPI_trend > 0 & TPI_sig=="Sig", "Hotter",
                                                            ifelse(TPIcor < 0 & TPI_trend < 0 & TPI_sig =="Sig","Colder","No Change")),
                                        water_trend = ifelse(AI_trend > 0 & AIcor > 0 & AI_sig=="Sig", "Wetter",
                                                             ifelse(AI_trend < 0 & AIcor < 0 & AI_sig =="Sig","Dryer","No Change")),
                                        land_trend = ifelse(HLU_trend > 0 &HLUcor > 0 & HLU_sig=="Sig", "More Land Use",
                                                            ifelse(HLU_trend < 0 & HLUcor < 0 & HLU_sig =="Sig","Less Land Use","No Change in Land Use")),
                                        size_trend_lm = ifelse(lm_estimate > 0 & lm_sig=="Sig", "Larger",
                                                               ifelse(lm_estimate < 0 & lm_sig =="Sig","Smaller","No Change")),
                                        size_trend_lme = ifelse(estimate > 0 & lme_sig=="Sig", "Larger",
                                                                ifelse(estimate < 0 & lme_sig =="Sig","Smaller","No Change")))

size_yr_final <- size_yr_final %>% mutate(temp_trend = ifelse(TPIcor > 0 & TPI_trend > 0 & TPI_sig=="Sig", "Hotter",
                                                              ifelse(TPIcor < 0 & TPI_trend < 0 & TPI_sig =="Sig","Colder","No Change")),
                                          water_trend = ifelse(AI_trend > 0 & AIcor > 0 & AI_sig=="Sig", "Wetter",
                                                               ifelse(AI_trend < 0 & AIcor < 0 & AI_sig =="Sig","Dryer","No Change")),
                                          land_trend = ifelse(HLU_trend > 0 &HLUcor > 0 & HLU_sig=="Sig", "More Land Use",
                                                              ifelse(HLU_trend < 0 & HLUcor < 0 & HLU_sig =="Sig","Less Land Use","No Change in Land Use")),
                                          size_trend_lm = ifelse(lm_estimate > 0 & lm_sig=="Sig", "Larger",
                                                                 ifelse(lm_estimate < 0 & lm_sig =="Sig","Smaller","No Change")),
                                          size_trend_lme = ifelse(estimate > 0 & lme_sig=="Sig", "Larger",
                                                                  ifelse(estimate < 0 & lme_sig =="Sig","Smaller","No Change")))


#filter to necessary columns
mass_yr_final_bird <- mass_yr_final[,-c(1,2)]
write.csv(mass_yr_final, "./Bird_Year_Trends_mass.csv")
len_yr_final_bird <- len_yr_final[,-c(1,2)]
write.csv(len_yr_final, "./Bird_Year_Trends_length.csv")
size_yr_final_bird <- size_yr_final[,-c(1,2)]
write.csv(size_yr_final, "./Bird_Year_Trends_size.csv")
