
###year
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
setwd("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/A_SUBMISSION_UPDATED/FINAL SUBMISSION/Supplmentary Datafiles for Publication") #set to where model output files will be stored
###############################MAMMAL ANALYSIS#############################################
#change file path to location of datafiles
API <- vroom("Data_S8_TPI_Limits.csv")%>%
  dplyr::select(Binomial, AMin, AMax, MonthName)%>%
  rename(Mnth = MonthName)%>%
  mutate(Mnth = ifelse(Mnth=="Jun", "June",
                       ifelse(Mnth=="Jul", "July", Mnth)))
#MAMMALS
M_Mass <- read.csv("./Data_S4_Mammal_Mass.csv")%>%left_join(API)%>%
  mutate(API = (AI-AMin)/(AMax-AMin))
M_Length <- read.csv("./Data_S5_Mammal_Length.csv")%>%left_join(API)%>%
  mutate(API = (AI-AMin)/(AMax-AMin))
M_Size<- read.csv("./Data_S6_Mammal_Size.csv")%>%mutate(LSize = log10(Mass)/log10(Body_Length))%>%left_join(API)%>%
  mutate(API = (AI-AMin)/(AMax-AMin))



#do seperate lm for each sp
mass_year <- M_Mass %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(LMass ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)

length_year <- M_Length %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(LLength ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)

size_year <- M_Size %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(LSize ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)


#combine estimates
lm_values <- mass_year[,c("Binomial", "lm_estimate")]
lm_valuesl <- length_year[,c("Binomial", "lm_estimate")]
lm_valuess <- size_year[,c("Binomial", "lm_estimate")]


#tpi by year lm and cor
tpi_year_lm <- M_Mass %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(TPI_month_max ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
tpi_year_lm <- tpi_year_lm[,c(2:4)]
colnames(tpi_year_lm)[c(3)] <- c("TPI_trend")

tpi_year_lml <- M_Length %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(TPI_month_max ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
tpi_year_lml <- tpi_year_lml[,c(2:4)]
colnames(tpi_year_lml)[c(3)] <- c("TPI_trend")

tpi_year_lms <- M_Size %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(TPI_month_max ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
tpi_year_lms <- tpi_year_lms[,c(2:4)]
colnames(tpi_year_lms)[c(3)] <- c("TPI_trend")

#ai by year lm and cor
ai_year_lm <- M_Mass %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(API ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
ai_year_lm <- ai_year_lm[,c(2:4)]
colnames(ai_year_lm)[c(3)] <- c("API_trend")

ai_year_lml <- M_Length %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(API ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
ai_year_lml <- ai_year_lml[,c(2:4)]
colnames(ai_year_lml)[c(3)] <- c("API_trend")

ai_year_lms <- M_Size %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(API ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
ai_year_lms <- ai_year_lms[,c(2:4)]
colnames(ai_year_lms)[c(3)] <- c("API_trend")

#hlu by year lm and cor
hlu_year_lm <- M_Mass %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(HLU ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
hlu_year_lm <- hlu_year_lm[,c(2:4)]
colnames(hlu_year_lm)[c(3)] <- c("HLU_trend")

hlu_year_lml <- M_Length %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(HLU ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
hlu_year_lml <- hlu_year_lml[,c(2:4)]
colnames(hlu_year_lml)[c(3)] <- c("HLU_trend")

hlu_year_lms <- M_Size %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(HLU ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
hlu_year_lms <- hlu_year_lms[,c(2:4)]
colnames(hlu_year_lms)[c(3)] <- c("HLU_trend")

mass_yr_final <- left_join(mass_yr_final,tpi_year_lm)
mass_yr_final<- left_join(mass_yr_final,api_year_lm)
mass_yr_final <- left_join(mass_yr_final,hlu_year_lm)%>%filter(!is.na(HLU_trend))

len_yr_final <- left_join(len_yr_final,tpi_year_lml)%>%filter(!is.na(TPI_trend))
len_yr_final<- left_join(len_yr_final,api_year_lml)%>%filter(!is.na(API_trend))
len_yr_final <- left_join(len_yr_final,hlu_year_lml)%>%filter(!is.na(HLU_trend))

size_yr_final <- left_join(size_yr_final,tpi_year_lms)%>%filter(!is.na(TPI_trend))
size_yr_final<- left_join(size_yr_final,api_year_lms)%>%filter(!is.na(API_trend))
size_yr_final <- left_join(size_yr_final,hlu_year_lms)%>%filter(!is.na(HLU_trend))

#filter to necessary columns
mass_yr_final_mam <- mass_yr_final[,-c(1)]
write.csv(mass_yr_final_mam, "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/A_SUBMISSION_UPDATED/Mam_Year_Trends_mass_api.csv")
len_yr_final_mam <- len_yr_final[,-c(1)]
write.csv(len_yr_final_mam, "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/A_SUBMISSION_UPDATED/Mam_Year_Trends_length_api.csv")
size_yr_final_mam <- size_yr_final[,-c(1)]
write.csv(size_yr_final_mam, "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/A_SUBMISSION_UPDATED/Mam_Year_Trends_size_api.csv")

mass_yr_final_mam <- mass_yr_final_mam %>% mutate(Class ="Mammalia",
                                                 Metric="Mass")
len_yr_final_mam <- len_yr_final_mam %>% mutate(Class ="Mammalia",
                                                  Metric="Length")
size_yr_final_mam <- size_yr_final_mam %>% mutate(Class ="Mammalia",
                                                  Metric="Size")

Mammal <- rbind(mass_yr_final_mam,len_yr_final_mam,size_yr_final_mam)
###############BIRDS#######################################
B_Mass <- vroom("./Data_S1_Bird_Mass.csv")%>%mutate(LMass = log10(Mass))%>%left_join(API)%>%
  mutate(API = (AI-AMin)/(AMax-AMin))
B_Length <- vroom("./Data_S2_Bird_Length.csv")%>%
  mutate(LLength = log10(Body_Length))%>%filter(AI<75)%>%left_join(API)%>%
  mutate(AI_win = DescTools::Winsorize(AI, probs = c(0.014,0.986)))%>%
  mutate(API = (AI_win-AMin)/(AMax-AMin))
B_Size <- vroom("./Data_S3_Bird_Size.csv")%>% 
  mutate(LSize = log10(Mass)/log10(Body_Length))%>%filter(AI<75)%>%left_join(API)%>%
  mutate(AI_win = DescTools::Winsorize(AI, probs = c(0.004,0.996)))%>%
  mutate(API = (AI_win-AMin)/(AMax-AMin))

####yea

#do seperate lm for each sp
mass_year <- B_Mass %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(log10(Mass) ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)


length_year <- B_Length %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(log10(Body_Length) ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)


size_year <- B_Size %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm((log10(Mass)/log10(Body_Length)) ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)
size_year <- size_year[,c(2:4)]
colnames(size_year)[c(3)] <- c("TPI_trend")

#combine estimates
lm_values <- mass_year[,c("Binomial", "estimate")]
lm_valuesl <- length_year[,c("Binomial", "estimate")]
lm_valuess <- size_year[,c("Binomial", "estimate")]

colnames(lm_values) <- c("Binomial","lm_estimate")
colnames(lm_valuesl) <- c("Binomial","lm_estimate")
colnames(lm_valuess) <- c("Binomial","lm_estimate")


#tpi by year lm and cor

tpi_year_lm <- B_Mass %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(TPI_month_max ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
tpi_year_lm <- tpi_year_lm[,c(2:4)]
colnames(tpi_year_lm)[c(3)] <- c("TPI_trend")

tpi_year_lml <- B_Length %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(TPI_month_max ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
tpi_year_lml <- tpi_year_lml[,c(2:4)]
colnames(tpi_year_lml)[c(3)] <- c("TPI_trend")

tpi_year_lms <- B_Size %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(TPI_month_max ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
tpi_year_lms <- tpi_year_lms[,c(2:4)]
colnames(tpi_year_lms)[c(3)] <- c("TPI_trend")

#ai by year lm and cor
ai_year_lm <- B_Mass %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(API ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
ai_year_lm <- ai_year_lm[,c(2:4)]
colnames(ai_year_lm)[c(3)] <- c("API_trend")

ai_year_lml <- B_Length %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(API ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
ai_year_lml <- ai_year_lml[,c(2:4)]
colnames(ai_year_lml)[c(3)] <- c("API_trend")

ai_year_lms <- B_Size %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(API ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
ai_year_lms <- ai_year_lms[,c(2:4)]
colnames(ai_year_lms)[c(3)] <- c("API_trend")

#hlu by year lm and cor
hlu_year_lm <- B_Mass %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(HLU ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
hlu_year_lm <- hlu_year_lm[,c(2:4)]
colnames(hlu_year_lm)[c(3)] <- c("HLU_trend")

hlu_year_lml <- B_Length %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(HLU ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
hlu_year_lml <- hlu_year_lml[,c(2:4)]
colnames(hlu_year_lml)[c(3)] <- c("HLU_trend")


hlu_year_lms <- B_Size %>%
  group_by(Binomial) %>%
  summarise(model = list(broom::tidy(lm(HLU ~ Year_sc, data = cur_data())))) %>%
  tidyr::unnest(model)%>%
  filter(term=="Year_sc")
hlu_year_lms <- hlu_year_lms[,c(2:4)]
colnames(hlu_year_lms)[c(3)] <- c("HLU_trend")

mass_yr_final <- left_join(mass_yr_final,tpi_year_lm)
mass_yr_final<- left_join(mass_yr_final,api_year_lm)
mass_yr_final <- left_join(mass_yr_final,hlu_year_lm)%>%filter(!is.na(HLU_trend))

len_yr_final2 <- left_join(len_yr_final,tpi_year_lml)%>%filter(!is.na(TPI_trend))
len_yr_final2<- left_join(len_yr_final2,api_year_lml)%>%filter(!is.na(API_trend))
len_yr_final2 <- left_join(len_yr_final2,hlu_year_lml)%>%filter(!is.na(HLU_trend))

size_yr_final <- left_join(size_yr_final,tpi_year_lms)%>%filter(!is.na(TPI_trend))
size_yr_final<- left_join(size_yr_final,api_year_lms)%>%filter(!is.na(API_trend))
size_yr_final <- left_join(size_yr_final,hlu_year_lms)%>%filter(!is.na(HLU_trend))

#filter to necessary columns
mass_yr_final_bird <- mass_yr_final[,-c(1)]
write.csv(mass_yr_final_bird, "./Bird_Year_Trends_mass_api.csv")
len_yr_final_bird <- len_yr_final[,-c(1)]
write.csv(len_yr_final_bird, "./Bird_Year_Trends_length_api.csv")
size_yr_final_bird <- size_yr_final[,-c(1)]
write.csv(size_yr_final_bird, "./Bird_Year_Trends_size_api.csv")


mass_yr_final_bird <- mass_yr_final_bird %>% mutate(Class ="Aves",
                                                  Metric="Mass")
len_yr_final_bird <- len_yr_final_bird %>% mutate(Class ="Aves",
                                                Metric="Length")
size_yr_final_bird <- size_yr_final_bird %>% mutate(Class ="Aves",
                                                  Metric="Size")

Aves <- rbind(mass_yr_final_bird,len_yr_final_bird,size_yr_final_bird)

allsp <- rbind(Aves,Mammal)
write.csv(allsp, "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data_S9_Body_Size_Time_Trend_Data.csv")
