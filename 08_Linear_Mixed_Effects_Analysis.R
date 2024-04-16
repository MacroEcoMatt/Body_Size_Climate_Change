#' ---
#' Step 8: Conduct Linear Mixed Effects Modeling
#' Matthew Watson
#' conducts LME models for analysis of body size trends
#' ---
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
setwd("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/For Submisstion/Supplmentary Datafiles for Publication")
###############################MAMMAL ANALYSIS#############################################
#change file path to location of datafiles
M_Mass <- vroom("Data_S4_Mammal_Mass.csv")%>%mutate(LMass = log10(Mass), 
                                                      AI = ifelse(Aridity>100,100,Aridity))%>%
  group_by(Binomial) %>% mutate(S_Mass = (LMass - mean(LMass))/sd(LMass))%>%
  ungroup()

M_Length <- vroom("Data_S5_Mammal_Length.csv")%>%mutate(LLength = log10(Body_Length), 
                                                        AI = ifelse(Aridity>100,100,Aridity))%>%
  group_by(Binomial) %>% mutate(S_Length = (LLength - mean(LLength))/sd(LLength))%>%
  ungroup()

M_Size <- vroom("Data_S6_Mammal_Size.csv")%>%mutate(LSize = log10(Mass)/log10(Body_Length), 
                                                      AI = ifelse(Aridity>100,100,Aridity),
                                                      LLength = log10(Body_Length),
                                                      LMass = log10(Mass)) %>%
  group_by(Binomial) %>% mutate(S_Size = (LSize - mean(LSize)) / sd(LSize))%>%
  ungroup()

API <- vroom("C:/Users/matth/OneDrive/Documents/PhD/Thesis/TPI and API/Month_Limits.csv")%>%
  dplyr::select(Binomial, AMin, AMax, MonthName)%>%
  rename(Mnth = MonthName)%>%
  mutate(Mnth = ifelse(Mnth=="Jun", "June",
                       ifelse(Mnth=="Jul", "July", Mnth)))

M_Mass <- left_join(M_Mass,API) %>%
  mutate(API = (AI-AMin)/(AMax-AMin))

M_Length <- left_join(M_Length,API) %>%
  mutate(API = (AI-AMin)/(AMax-AMin))

M_Size <- left_join(M_Size,API) %>%
  mutate(API = (AI-AMin)/(AMax-AMin))

sp_mass_mod <- lme(LMass ~ 1,
                   random = ~1|Binomial, 
                   data= M_Mass)
plot(Variogram(sp_mass_mod, form=~Lat+Lon|Binomial, maxDist = 5))

#FULL MASS MODEL
final_mass_model <- lmer(LMass ~ TPI_month_max + API + HLU +
                           TPI_month_max:API + TPI_month_max:HLU +
                           (1|Binomial),data=M_Mass,REML=F)


mass_model_step <- step(final_mass_model,direction="backward")

mass_model_step_top <- get_model(mass_model_step)

AIC(final_mass_model,mass_model_step_top)

tab_model(final_mass_model,mass_model_step_top, digits=5)

check_model(final_mass_model)

#after step wise removal based on AIC and BIC
#FINAL MASS MODEL

final_mass_model <- lmer(LMass ~ TPI_month_max + API + HLU +
                           TPI_month_max:API + TPI_month_max:HLU +
                           (1|Binomial),data=M_Mass)

tab_model(final_mass_model, digits=5)

plot(density(residuals(final_mass_model)))
plot(final_mass_model)
plot(check_outliers(final_mass_model))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
library(corrplot)
Mm <- M_Mass%>%filter(!is.na(API))
m_d <- corrplot(cor(as.matrix(Mm[,c("Year_sc","TPI_month_max","API","HLU")])),
                method="number",type="upper",col=col(200),
                addCoef.col = "black", tl.col="black",tl.srt=45)  

tab_model(final_mass_model, digits=5, 
          file = "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mammal_Mass_Results_api.html")

sink("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mammal_mass_LME_api.txt")
"Mammal Mass Model LME"
summary(final_mass_model)
"R squared"
print(MuMIn::r.squaredGLMM(final_mass_model))
sink()

#spatial variogram
sp_length_mod <- lme(LLength ~ 1,
                     random = ~1|Binomial, 
                     data= M_Length)
plot(Variogram(sp_length_mod, form=~Lat+Lon|Binomial, maxDist = 5))

#FULL LENGTH MODEL
length_model <- lmer(LLength ~ TPI_month_max + API + HLU +
                       TPI_month_max:API + TPI_month_max:HLU +
                       (1|Binomial),data=M_Length,REML = F,
                     control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

length_model_step <- step(length_model)

length_model_step_top <- get_model(length_model_step)

AIC(length_model,length_model_step_top)
tab_model(length_model,length_model_step_top, digits=5)

#after step wise removal based on AIC and BIC
#FINAL LENGTH MODEL

final_length_model <- lmer(LLength ~ TPI_month_max + API + HLU +
                             TPI_month_max:API + TPI_month_max:HLU +
                             (1|Binomial),data=M_Length,REML = T,
                           control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

plot(density(residuals(final_length_model)))
plot(final_length_model)
plot(check_outliers(final_length_model))
ll <- M_Length %>% filter(!is.na(API))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
m_d <- corrplot(cor(as.matrix(ll[,c("Year_sc","TPI_month_max","API","HLU")])),
                method="number",type="upper",col=col(200),
                addCoef.col = "black", tl.col="black",tl.srt=45)

tab_model(final_length_model, digits=5, 
          file = "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mammal_Length_Results_api.html")

sink("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mammal_length_LME_api.txt")
"Mammal Length Model LME"
summary(final_length_model)
"R squared"
print(MuMIn::r.squaredGLMM(final_length_model))
""
sink()

####MASS:LENGTH ANALYSIS####

#spatial variogram
sp_size_mod <- lme(LSize ~ 1,
                   random = ~1|Binomial, 
                   data= SIZE_MASS_INDEX)
plot(Variogram(sp_size_mod, form=~Lat+Lon|Binomial, maxDist = 5))

#FULL MASS:LENGTH MODEL

size_model <- lmer(LSize ~ TPI_month_max + API + HLU +
                     TPI_month_max:API + TPI_month_max:HLU +
                     (1|Binomial),data=M_Size, REML=F,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


ml_step <- step(size_model)

ml_top <- get_model(ml_step)

tab_model(size_model,ml_top, digits = 5)

AIC(ml_top)


#contrast coded complimentary model
final_size_mode <- lmer(LSize ~ TPI_month_max + API + HLU +
                          TPI_month_max:API + TPI_month_max:HLU +
                          (1|Binomial),data=M_Size, REML=T,
                        control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


plot(density(residuals(final_size_mode)))
plot(final_size_mode)
plot(check_outliers(final_size_mode))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
ms <- M_Size %>% filter(!is.na(API))
m_d <- corrplot(cor(as.matrix(ms[,c("Year_sc","TPI_month_max","API","HLU")])),
                method="number",type="upper",col=col(200),
                addCoef.col = "black", tl.col="black",tl.srt=45)

tab_model(final_size_mode, digits=5, 
          file = "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mammal_SMI_Results_api.html")

sink("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mammal_size_LME_api.txt")
"Mammal SIZE Model LME"
summary(final_size_mode)
"R squared"
print(MuMIn::r.squaredGLMM(final_size_mode))
sink()
####AVES
bird_m <- vroom("Data_S1_Bird_Mass.csv")%>%mutate(LMass = log10(Mass), 
                                                  AI = ifelse(Aridity>100,100,Aridity))%>%
  group_by(Binomial) %>% mutate(S_Mass = (LMass - mean(LMass))/sd(LMass))%>%
  ungroup()
bird_m <- bird_m %>% mutate(AI_win = DescTools::Winsorize(AI, probs = c(0.008,0.992)))

bird_l <- vroom("Data_S2_Bird_Length.csv")%>%mutate(LLength = log10(Body_Length), 
                                                    AI = ifelse(Aridity>100,100,Aridity))%>%
  group_by(Binomial) %>% mutate(S_Length = (LLength - mean(LLength))/sd(LLength))%>%
  ungroup()
bird_l <- bird_l %>% mutate(AI_win = DescTools::Winsorize(AI, probs = c(0.014,0.986)))

bird_s <- vroom("Data_S3_Bird_Size.csv")%>%mutate(LSize = log10(Mass)/log10(Body_Length), 
                                                  AI = ifelse(Aridity>100,100,Aridity),
                                                  LLength = log10(Body_Length),
                                                  LMass = log10(Mass)) %>%
  group_by(Binomial) %>% mutate(S_Length = (LLength - mean(LLength))/sd(LLength),
                                S_Mass = (LMass - mean(LMass))/sd(LMass),
                                S_Size = ((S_Mass/S_Length) - mean(S_Mass/S_Length)) / sd(S_Mass/S_Length))%>%
  ungroup()
bird_s <- bird_s %>% mutate(AI_win = DescTools::Winsorize(AI, probs = c(0.004,0.996)))


bird_m <- left_join(bird_m,API)%>%
  mutate(API = (AI-AMin)/(AMax-AMin))

bird_l <- left_join(bird_l,API)%>%
  mutate(API = (AI_win-AMin)/(AMax-AMin))

bird_s <- left_join(bird_s,API)%>%
  mutate(API = (AI_win-AMin)/(AMax-AMin))

#FULL MASS MODEL
#spatial variogram
sp_size_mod2 <- lme(LSize ~ 1,
                   random = ~1|Binomial, 
                   data= bird_m)
plot(Variogram(sp_size_mod2, form=~Lat+Lon|Binomial, maxDist = 5))

final_mass_model2 <- lmer(LMass ~ TPI_month_max + API + HLU +
                           TPI_month_max:API + TPI_month_max:HLU +
                           (1|Binomial),data=bird_m,REML=F)


mass_model_step2 <- step(final_mass_model,direction="backward")

mass_model_step_top2 <- get_model(mass_model_step2)

AIC(final_mass_model2,mass_model_step_top2)

tab_model(final_mass_model2,mass_model_step_top2, digits=5)

check_model(final_mass_model2)

#after step wise removal based on AIC and BIC
#FINAL MASS MODEL

final_mass_model2 <- lmer(LMass ~ TPI_month_max + API + HLU +
                           TPI_month_max:API + TPI_month_max:HLU +
                           (1|Binomial),data=bird_m)

tab_model(final_mass_model2, digits=5)

plot(density(residuals(final_mass_model2)))
plot(final_mass_model2)
plot(check_outliers(final_mass_model2))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
m_d <- corrplot(cor(as.matrix(bird_m[,c("Year_sc","TPI_month_max","API","HLU")])),
                method="number",type="upper",col=col(200),
                addCoef.col = "black", tl.col="black",tl.srt=45)  

tab_model(final_mass_model2, digits=5, 
          file = "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Aves_Mass_Results_api.html")

sink("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Aves_mass_LME_api.txt")
"Mammal Mass Model LME"
summary(final_mass_model2)
"R squared"
print(MuMIn::r.squaredGLMM(final_mass_model2))
sink()

#spatial variogram
sp_length_mod2 <- lme(LLength ~ 1,
                     random = ~1|Binomial, 
                     data= bird_l)
plot(Variogram(sp_length_mod2, form=~Lat+Lon|Binomial, maxDist = 5))

#FULL LENGTH MODEL
length_model2 <- lmer(LLength ~ TPI_month_max + API + HLU +
                       TPI_month_max:API + TPI_month_max:HLU +
                       (1|Binomial),data=bird_l,REML = F,
                     control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

length_model_step2 <- step(length_model2)

length_model_step_top2 <- get_model(length_model_step2)

AIC(length_model2,length_model_step_top2)
tab_model(length_model2,length_model_step_top2, digits=5)

#after step wise removal based on AIC and BIC
#FINAL LENGTH MODEL

final_length_model2 <- lmer(LLength ~ TPI_month_max + API + HLU +
                             TPI_month_max:API + TPI_month_max:HLU +
                             (1|Binomial),data=bird_l,REML = T,
                           control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summary(final_mass_model2)
plot(density(residuals(final_length_model2)))
plot(final_length_model2)
plot(check_outliers(final_length_model2))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
m_d <- corrplot(cor(as.matrix(bird_l[,c("Year_sc","TPI_month_max","API","HLU")])),
                method="number",type="upper",col=col(200),
                addCoef.col = "black", tl.col="black",tl.srt=45)

tab_model(final_length_model2, digits=5, 
          file = "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Aves_Length_Results_api.html")

sink("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Aves_length_LME_api.txt")
"Mammal Length Model LME"
summary(final_length_model2)
"R squared"
print(MuMIn::r.squaredGLMM(final_length_model2))
""
sink()

####MASS:LENGTH ANALYSIS####

#spatial variogram
sp_size_mod2 <- lme(LSize ~ 1,
                   random = ~1|Binomial, 
                   data= bird_s)
plot(Variogram(sp_size_mod2, form=~Lat+Lon|Binomial, maxDist = 5))

#FULL MASS:LENGTH MODEL

size_model2 <- lmer(LSize ~ TPI_month_max + API + HLU +
                     TPI_month_max:API + TPI_month_max:HLU +
                     (1|Binomial),data=bird_s, REML=F,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


ml_step2 <- step(size_model2)

ml_top2 <- get_model(ml_step2)

tab_model(size_model2,ml_top2, digits = 5)

AIC(ml_top2)


#contrast coded complimentary model
final_size_mode2 <- lmer(LSize ~ TPI_month_max + HLU +
                          (1|Binomial),data=bird_s, REML=T,
                        control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

tab_model(final_size_mode2, digits = 5)

plot(density(residuals(final_size_mode2)))
plot(final_size_mode2)
plot(check_outliers(final_size_mode2))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
ss <- bird_s %>% filter(!is.na(API))
m_d <- corrplot(cor(as.matrix(ss[,c("Year_sc","TPI_month_max","API","HLU")])),
                method="number",type="upper",col=col(200),
                addCoef.col = "black", tl.col="black",tl.srt=45)

tab_model(final_size_mode2, digits=5, 
          file = "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Aves_SMI_Results_api.html")

sink("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Aves_size_LME_api.txt")
"Mammal SIZE Model LME"
summary(final_size_mode2)
"R squared"
print(MuMIn::r.squaredGLMM(final_size_mode2))
sink()


###figures
#birds mass
tpi_m <- ggpredict(final_mass_model2, terms=c("TPI_month_max"))
ai_m <- ggpredict(final_mass_model2, terms=c("API [0:1 by=0.1]"))
hlu_m <- ggpredict(final_mass_model2, terms=c("HLU"))
tpiai_m <- ggpredict(final_mass_model2, terms=c("TPI_month_max","API [0.25:0.75 by=0.25]"))
tpihlu_m <- ggpredict(final_mass_model2, terms=c("TPI_month_max","HLU [0.25:0.75 by=0.25]"))

#mammal mass
tpi_mm <- ggpredict(final_mass_model, terms=c("TPI_month_max [0:1 by=0.1]"))
ai_mm <- ggpredict(final_mass_model, terms=c("API [0:1 by=0.1]"))
hlu_mm <- ggpredict(final_mass_model, terms=c("HLU [0:1 by=0.1]"))
tpiai_mm <- ggpredict(final_mass_model, terms=c("TPI_month_max [0:1 by=0.1]","API [0.25:0.75 by=0.25]"))
tpihlu_mm <- ggpredict(final_mass_model, terms=c("TPI_month_max [0:1 by=0.1]","HLU [0.25:0.75 by=0.25]"))

#bird length
tpi_l <- ggpredict(final_length_model2, terms=c("TPI_month_max [0:1 by=0.1]"))
ai_l <- ggpredict(final_length_model2, terms=c("API [0:1 by=0.1]"))
hlu_l <- ggpredict(final_length_model2, terms=c("HLU [0:1 by=0.1]"))
tpiai_l <- ggpredict(final_length_model2, terms=c("TPI_month_max","API [0.25:0.75 by=0.25]"))
tpihlu_l <- ggpredict(final_length_model2, terms=c("TPI_month_max","HLU [0.25:0.75 by=0.25]"))

#mammal length
tpi_lm <- ggpredict(final_length_model, terms=c("TPI_month_max [0:1 by=0.1]"))
ai_lm <- ggpredict(final_length_model, terms=c("API [0:1 by=0.1]"))
hlu_lm <- ggpredict(final_length_model, terms=c("HLU [0:1 by=0.1]"))
tpiai_lm <- ggpredict(final_length_model, terms=c("TPI_month_max [0:1 by=0.1]","API [0.25:0.75 by=0.25]"))
tpihlu_lm <- ggpredict(final_length_model, terms=c("TPI_month_max [0:1 by=0.1]","HLU [0.25:0.75 by=0.25]"))

tpi_s <- ggpredict(final_size_mode2, terms=c("TPI_month_max [0:1 by=0.1]"))
hlu_s <- ggpredict(final_size_mode2, terms=c("HLU [0:1 by=0.1]"))

tpi_sm <- ggpredict(final_size_mode, terms=c("TPI_month_max [0:1 by=0.1]"))
ai_sm <- ggpredict(final_size_mode, terms=c("API [0:1 by=0.1]"))
hlu_sm <- ggpredict(final_size_mode, terms=c("HLU [0:1 by=0.1]"))
tpiai_sm <- ggpredict(final_size_mode, terms=c("TPI_month_max [0:1 by=0.1]","API [0.25:0.75 by=0.25]"))
tpihlu_sm <- ggpredict(final_size_mode, terms=c("TPI_month_max [0:1 by=0.1]","HLU [0.25:0.75 by=0.25]"))
#mass plots
b1 <- plot(tpi_m, colors=c("red3"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Mass (g)",
       title = "",
       color="")+xlim(0,1)+ylim(1.35,1.65)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        axis.title.x = element_text(color="red3"),
        legend.text = element_text(color="black",size=10, face="bold"),
        legend.position = "none",
        plot.margin = margin(0,1,0.2,0, "cm"),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5))

b2 <- plot(ai_m, colors=c("steelblue2"), line.size = 1)+
  labs(x = "Aridity Position Index",
       y = "Log Body Mass (g)",
       title = "",
       color="")+ylim(1.35,1.65)+xlim(0,1)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        axis.title.x = element_text(color="steelblue2"),
        legend.text = element_text(color="black",size=10, face="bold"),
        legend.position = "none",
        plot.margin = margin(0,1,0.2,0, "cm"),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5))

b3 <- plot(hlu_m, colors=c("forestgreen"), line.size = 1)+
  labs(x = "Human Land Use Percent",
       y = "Log Body Mass (g)",
       title = "",
       color="")+xlim(0,1)+ylim(1.35,1.65)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        axis.title.x = element_text(color="forestgreen"),
        legend.text = element_text(color="black",size=10, face="bold"),
        legend.position = "none",
        plot.margin = margin(0,1,0.2,0, "cm"),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5))

b4 <- plot(tpiai_m, ci=F,  colors=c("red3","gold3","steelblue3"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Mass (g)",
       title = "",
       color="API")+xlim(0,1)+ylim(1.46,1.56)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold"))
b5 <- plot(tpihlu_m, ci=F, colors=c("forestgreen","grey60","saddlebrown"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Mass (g)",
       title = "",
       color="HLU %")+xlim(0,1)+ylim(1.46,1.56)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold"))
b6 <- plot(tpi_l, colors=c("red3"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Length (mm)",
       title = "",
       color="")+ylim(2.05,2.26)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=10, face="bold"),
        axis.title.x = element_text(color="red3"),
        legend.position = "none",
        plot.margin = margin(0,1,0.2,0, "cm"),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5))

b7 <- plot(ai_l, colors=c("steelblue2"), line.size = 1)+
  labs(x = "Aridity Position Index",
       y = "Log Body Length (mm)",
       title = "",
       color="")+ylim(2.05,2.26)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=10, face="bold"),
        axis.title.x = element_text(color="steelblue2"),
        legend.position = "none",
        plot.margin = margin(0,1,0.2,0, "cm"),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5))

b8 <- plot(hlu_l, colors=c("forestgreen"), line.size = 1)+
  labs(x = "Human Land Use Percent",
       y = "Log Body Length (mm)",
       title = "",
       color="")+ylim(2.05,2.26)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=10, face="bold"),
        axis.title.x = element_text(color="forestgreen"),
        legend.position = "none",
        plot.margin = margin(0,1,0.2,0, "cm"),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5))

b9 <- plot(tpiai_l, ci=F,  colors=c("red3","gold3","steelblue3"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Length (mm)",
       title = "",
       color="API")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold"))

b10 <- plot(tpihlu_l, ci=F, colors=c("forestgreen","grey60","saddlebrown"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Length (mm)",
       title = "",
       color="HLU %")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold"))

b11 <- plot(tpi_s, colors=c("red3"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Mass:Length (g/mm)",
       title = "",
       color="")+ylim(0.55,0.8)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=10, face="bold"),
        axis.title.x = element_text(color="red3"),
        legend.position = "none",
        plot.margin = margin(0,1,0.2,0, "cm"),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5))

b12 <- plot(hlu_s, colors=c("forestgreen"), line.size = 1)+
  labs(x = "Human Land Use Percent",
       y = "Log Mass:Length (g/mm)",
       title = "",
       color="")+ylim(0.55,0.8)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=10, face="bold"),
        axis.title.x = element_text(color="forestgreen"),
        legend.position = "none",
        plot.margin = margin(0,1,0.2,0, "cm"),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5))
plot(b12)
#mammals
m1 <- plot(tpi_mm, colors=c("red3"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Mass (g)",
       title = "",
       color="")+ylim(1.45,1.75)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        axis.title.x=element_text(color="red3"),
        legend.text = element_text(color="black",size=10, face="bold"),
        legend.position = "none",
        plot.margin = margin(0,1,0.2,0, "cm"),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5))

m2 <- plot(ai_mm, colors=c("steelblue2"), line.size = 1)+
  labs(x = "Ariditry Position Index",
       y = "Log Body Mass (g)",
       title = "",
       color="")+ylim(1.45,1.75)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        axis.title.x=element_text(color="steelblue2"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.margin = margin(0,1,0.2,0, "cm"),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5))

m3 <- plot(hlu_mm, colors=c("forestgreen"), line.size = 1)+
  labs(x = "Human Land Use Percent",
       y = "Log Body Mass (g)",
       title = "",
       color="")+ylim(1.45,1.75)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        axis.title.x=element_text(color="forestgreen"),
        legend.text = element_text(color="black",size=10, face="bold"),
        legend.position = "none",
        plot.margin = margin(0,1,0.2,0, "cm"),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5))

m4 <- plot(tpiai_mm, ci=F,  colors=c("red3","gold3","steelblue3"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Mass (g)",
       title = "",
       color="API")+xlim(0,1)+ylim(1.59,1.64)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold"))

m5 <- plot(tpihlu_mm, ci=F, colors=c("forestgreen","grey60","saddlebrown"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Mass (g)",
       title = "",
       color="HLU %")+xlim(0,1)+ylim(1.59,1.64)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold"))

m6 <- plot(tpi_lm, colors=c("red3"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Length (mm)",
       title = "",
       color="")+xlim(0,1)+ylim(2.17,2.3)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=10, face="bold"),
        axis.title.x=element_text(color="red3"),
        legend.position = "none",
        plot.margin = margin(0,1,0.2,0, "cm"),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5))

m7 <- plot(ai_lm, colors=c("steelblue2"), line.size = 1)+
  labs(x = "Ariditry Position Index",
       y = "Log Body Length (mm)",
       title = "",
       color="")+ylim(2.17,2.3)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=10, face="bold"),
        axis.title.x=element_text(color="steelblue2"),
        legend.position = "none",
        plot.margin = margin(0,1,0.2,0, "cm"),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5))

m8 <- plot(hlu_lm, colors=c("forestgreen"), line.size = 1)+
  labs(x = "Human Land Use Percent",
       y = "Log Body Length (mm)",
       title = "",
       color="")+ylim(2.17,2.3)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        axis.title.x=element_text(color="forestgreen"),
        legend.text = element_text(color="black",size=10, face="bold"),
        legend.position = "none",
        plot.margin = margin(0,1,0.2,0, "cm"),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5))

m9 <- plot(tpiai_lm, ci=F,  colors=c("red3","gold3","steelblue3"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Length (mm)",
       title = "",
       color="API")+xlim(0,1)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold"))

m10 <- plot(tpihlu_lm, ci=F, colors=c("forestgreen","grey60","saddlebrown"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Length (mm)",
       title = "",
       color="HLU %")+xlim(0,1)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold"))

m11 <- plot(tpi_sm, colors=c("red3"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Mass:Length (g/mm)",
       title = "",
       color="")+ylim(0.65,0.76)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        axis.title.x=element_text(color="red3"),
        legend.text = element_text(color="black",size=10, face="bold"),
        legend.position = "none",
        plot.margin = margin(0,1,0.2,0, "cm"),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5))

m12 <- plot(ai_sm, colors=c("steelblue2"), line.size = 1)+
  labs(x = "Ariditry Position Index",
       y = "Log Mass:Length (g/mm)",
       title = "",
       color="")+ylim(0.65,0.76)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        axis.title.x=element_text(color="steelblue2"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.margin = margin(0,1,0.2,0, "cm"),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5))

m13 <- plot(hlu_sm, colors=c("forestgreen"), line.size = 1)+
  labs(x = "Human Land Use Percent",
       y = "Log Mass:Length (g/mm)",
       title = "",
       color="")+ylim(0.65,0.76)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        axis.title.x=element_text(color="forestgreen"),
        legend.text = element_text(color="black",size=10, face="bold"),
        legend.position = "none",
        plot.margin = margin(0,1,0.2,0, "cm"),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5))

m14 <- plot(tpiai_sm, ci=F,  colors=c("red3","gold3","steelblue3"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Mass:Length (g/mm)",
       title = "",
       color="API")+xlim(0,1)+ylim(0.69,0.71)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold"),
        legend.position = "top")

m15 <- plot(tpihlu_sm, ci=F, colors=c("forestgreen","grey60","saddlebrown"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Mass:Length (g/mm)",
       title = "",
       color="HLU %")+xlim(0,1)+ylim(0.69,0.71)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold"),
        legend.position = "top")

####
library(ggpubr)
#REMEMBER REOMVE X AXIS LABELS WHEN ALL COMBINED
b_mass <- ggarrange(b1,m1,b2,m2,b3,m3,
                    ncol = 2,
                    nrow = 3,
                    labels = "AUTO",vjust = 1.3,hjust=-2)
plot(b_mass)
ggsave("mass_plot.jpg",
       path="C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/plots")

b_lenght <- ggarrange(b6,m6,b7,m7,b8,m8,
                    ncol = 2,
                    nrow = 3,
                    labels = "AUTO",vjust = 1.3,hjust=-2)
plot(b_lenght)
ggsave("length_plot.jpg",
       path="C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/plots")

p1 <- ggplot() + theme_void()
b_size <- ggarrange(b11,m11,p1,m12,b12,m13,
                      ncol = 2,
                      nrow = 3,
                      labels = "AUTO",vjust = 1.3,hjust=-2)
plot(b_size)
ggsave("size_plot.jpg",
       path="C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/plots")



b_inter_m <- ggarrange(b4,m4,
                     ncol = 2,
                     labels = "AUTO",
                     common.legend = T,legend="top")
b_inter_m2 <- ggarrange(b5,m5,
                       ncol = 2,
                       labels = c("C","D"),
                       common.legend = T,legend="top")

plot(b_inter_m)
ggsave("mass_api_inter.jpg",
       path="C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/plots")
plot(b_inter_m2)
ggsave("mass_hlu_inter.jpg",
       path="C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/plots")

b_inter_l <- ggarrange(b9,m9,
                       ncol = 2,
                       labels = "AUTO",
                       common.legend = T,legend="top")
b_inter_l2 <- ggarrange(b10,m10,
                        ncol = 2,
                        labels = c("C","D"),
                        common.legend = T,legend="top")

plot(b_inter_l)
ggsave("length_api_inter.jpg",
       path="C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/plots")
plot(b_inter_l2)
ggsave("length_hlu_inter.jpg",
       path="C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/plots")


b_inter_s <- ggarrange(m14,m15,
                       ncol = 2,
                       labels = "AUTO")
plot(b_inter_s)
ggsave("size_api_inter.jpg",
       path="C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/plots")
