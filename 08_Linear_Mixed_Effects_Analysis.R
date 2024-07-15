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

#update to file location
setwd("")
###############################MAMMAL ANALYSIS#############################################
#change file path to location of datafiles
M_Mass <- vroom("Data_4_Mammal_Mass.csv")%>%mutate(LMass = log10(Mass), 
                                                      AI = ifelse(Aridity>100,100,Aridity))%>%
  group_by(Binomial) %>% mutate(S_Mass = (LMass - mean(LMass))/sd(LMass))%>%
  ungroup()

M_Length <- vroom("Data_5_Mammal_Length.csv")%>%mutate(LLength = log10(Body_Length), 
                                                        AI = ifelse(Aridity>100,100,Aridity))%>%
  group_by(Binomial) %>% mutate(S_Length = (LLength - mean(LLength))/sd(LLength))%>%
  ungroup()

M_Size <- vroom("Data_6_Mammal_Size.csv")%>%mutate(LSize = log10(Mass)/log10(Body_Length), 
                                                      AI = ifelse(Aridity>100,100,Aridity),
                                                      LLength = log10(Body_Length),
                                                      LMass = log10(Mass)) %>%
  group_by(Binomial) %>% mutate(S_Size = (LSize - mean(LSize)) / sd(LSize))%>%
  ungroup()

API <- vroom("Data_7_TPI_Limits.csv")%>%
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

summary(final_mass_model)
tab_model(final_mass_model, digits=4)

plot(density(residuals(final_mass_model)))
plot(final_mass_model)
plot(check_outliers(final_mass_model))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
library(corrplot)
Mm <- M_Mass%>%filter(!is.na(API))
m_d <- corrplot(cor(as.matrix(Mm[,c("Year_sc","TPI_month_max","API","HLU")])),
                method="number",type="upper",col=col(200),
                addCoef.col = "black", tl.col="black",tl.srt=45)  
tab_model(final_mass_model, digits=4)
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
tab_model(final_length_model, digits = 4)
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
tab_model(final_size_mode, digits=4)
tab_model(final_size_mode, digits=5, 
          file = "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mammal_SMI_Results_api.html")

sink("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mammal_size_LME_api.txt")
"Mammal SIZE Model LME"
summary(final_size_mode)
"R squared"
print(MuMIn::r.squaredGLMM(final_size_mode))
sink()
####AVES
bird_m <- vroom("Data_1_Bird_Mass.csv")%>%mutate(LMass = log10(Mass), 
                                                  AI = ifelse(Aridity>100,100,Aridity))%>%
  group_by(Binomial) %>% mutate(S_Mass = (LMass - mean(LMass))/sd(LMass))%>%
  ungroup()
bird_m <- bird_m %>% mutate(AI_win = DescTools::Winsorize(AI, probs = c(0.008,0.992)))

bird_l <- vroom("Data_2_Bird_Length.csv")%>%mutate(LLength = log10(Body_Length), 
                                                    AI = ifelse(Aridity>100,100,Aridity))%>%
  group_by(Binomial) %>% mutate(S_Length = (LLength - mean(LLength))/sd(LLength))%>%
  ungroup()
bird_l <- bird_l %>% mutate(AI_win = DescTools::Winsorize(AI, probs = c(0.014,0.986)))

bird_s <- vroom("Data_3_Bird_Size.csv")%>%mutate(LSize = log10(Mass)/log10(Body_Length), 
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
summary(final_mass_model2)
plot(density(residuals(final_mass_model2)))
plot(final_mass_model2)
plot(check_outliers(final_mass_model2))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
m_d <- corrplot(cor(as.matrix(bird_m[,c("Year_sc","TPI_month_max","API","HLU")])),
                method="number",type="upper",col=col(200),
                addCoef.col = "black", tl.col="black",tl.srt=45)  
tab_model(final_mass_model2, digits=4)
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
summary(final_length_model2)
plot(density(residuals(final_length_model2)))
plot(final_length_model2)
plot(check_outliers(final_length_model2))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
m_d <- corrplot(cor(as.matrix(bird_l[,c("Year_sc","TPI_month_max","API","HLU")])),
                method="number",type="upper",col=col(200),
                addCoef.col = "black", tl.col="black",tl.srt=45)
tab_model(final_length_model2, digits=4)
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
tab_model(final_size_mode2, digits=4)
tab_model(final_size_mode2, digits=5, 
          file = "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Aves_SMI_Results_api.html")

sink("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Aves_size_LME_api.txt")
"Mammal SIZE Model LME"
summary(final_size_mode2)
"R squared"
print(MuMIn::r.squaredGLMM(final_size_mode2))
sink()

library(ggeffects)
###figures



library(ggplot2)
library(ggthemes)
library(ggeffects)
library(dplyr)
library(vroom)

#update to file location
da <- vroom("/Data_9_figure_data.csv")

###Generate base plots####
###MASS
mt <- da%>%filter(Metric == "Mass" & Variable=="TPI")
mass_tpi <- ggplot(mt,x=x1,y=y1)

masspol <- data.frame(Class = c("Aves","Aves","Aves","Aves","Mammalia","Mammalia","Mammalia","Mammalia"),
                      x = c(mt$x1[1],mt$x2[1],mt$x3[1],mt$x4[1],mt$x1[2],mt$x2[2],mt$x3[2],mt$x4[2]),
                      y = c(mt$y2[1],mt$y1[1],mt$y3[1],mt$y4[1],mt$y2[2],mt$y1[2],mt$y3[2],mt$y4[2]))

ma <- da%>%filter(Metric == "Mass" & Variable=="API")
mass_ai <- ggplot(ma,x=x1,y=y1)

masspol2 <- data.frame(Class = c("Aves","Aves","Aves","Aves","Mammalia","Mammalia","Mammalia","Mammalia"),
                       x = c(ma$x1[1],ma$x2[1],ma$x3[1],ma$x4[1],ma$x1[2],ma$x2[2],ma$x3[2],ma$x4[2]),
                       y = c(ma$y2[1],ma$y1[1],ma$y3[1],ma$y4[1],ma$y2[2],ma$y1[2],ma$y3[2],ma$y4[2]))

mh <- da%>%filter(Metric == "Mass" & Variable=="HLU")
mass_hlu <- ggplot(mh,x=x1,y=y1)

masspol3 <- data.frame(Class = c("Aves","Aves","Aves","Aves","Mammalia","Mammalia","Mammalia","Mammalia"),
                       x = c(mh$x1[1],mh$x2[1],mh$x3[1],mh$x4[1],mh$x1[2],mh$x2[2],mh$x3[2],mh$x4[2]),
                       y = c(mh$y2[1],mh$y1[1],mh$y3[1],mh$y4[1],mh$y2[2],mh$y1[2],mh$y3[2],mh$y4[2]))
###LENGTH
lt <- da%>%filter(Metric == "Length" & Variable=="TPI")
l_tpi <- ggplot(lt,x=x1,y=y1)

lengthpol <- data.frame(Class = c("Aves","Aves","Aves","Aves","Mammalia","Mammalia","Mammalia","Mammalia"),
                        x = c(lt$x1[1],lt$x2[1],lt$x3[1],lt$x4[1],lt$x1[2],lt$x2[2],lt$x3[2],lt$x4[2]),
                        y = c(lt$y2[1],lt$y1[1],lt$y3[1],lt$y4[1],lt$y2[2],lt$y1[2],lt$y3[2],lt$y4[2]))

la <- da%>%filter(Metric == "Length" & Variable=="API")
l_ai <- ggplot(la,x=x1,y=y1)

lengthpol2 <- data.frame(Class = c("Aves","Aves","Aves","Aves","Mammalia","Mammalia","Mammalia","Mammalia"),
                         x = c(la$x1[1],la$x2[1],la$x3[1],la$x4[1],la$x1[2],la$x2[2],la$x3[2],la$x4[2]),
                         y = c(la$y2[1],la$y1[1],la$y3[1],la$y4[1],la$y2[2],la$y1[2],la$y3[2],la$y4[2]))

lh <- da%>%filter(Metric == "Length" & Variable=="HLU")
l_hlu <- ggplot(lh,x=x1,y=y1)

lengthpol3 <- data.frame(Class = c("Aves","Aves","Aves","Aves","Mammalia","Mammalia","Mammalia","Mammalia"),
                         x = c(lh$x1[1],lh$x2[1],lh$x3[1],lh$x4[1],lh$x1[2],lh$x2[2],lh$x3[2],lh$x4[2]),
                         y = c(lh$y2[1],lh$y1[1],lh$y3[1],lh$y4[1],lh$y2[2],lh$y1[2],lh$y3[2],lh$y4[2]))

###Size
st <- da%>%filter(Metric == "Size" & Variable=="TPI")
s_tpi <- ggplot(st,x=x1,y=y1)

spol <- data.frame(Class = c("Aves","Aves","Aves","Aves","Mammalia","Mammalia","Mammalia","Mammalia"),
                   x = c(st$x1[1],st$x2[1],st$x3[1],st$x4[1],st$x1[2],st$x2[2],st$x3[2],st$x4[2]),
                   y = c(st$y2[1],st$y1[1],st$y3[1],st$y4[1],st$y2[2],st$y1[2],st$y3[2],st$y4[2]))

sa <- da%>%filter(Metric == "Size" & Variable=="API")
s_ai <- ggplot(sa,x=x1,y=y1)

spol2 <- data.frame(Class = c("Aves","Aves","Aves","Aves","Mammalia","Mammalia","Mammalia","Mammalia"),
                    x = c(sa$x1[1],sa$x2[1],sa$x3[1],sa$x4[1],sa$x1[2],sa$x2[2],sa$x3[2],sa$x4[2]),
                    y = c(sa$y2[1],sa$y1[1],sa$y3[1],sa$y4[1],sa$y2[2],sa$y1[2],sa$y3[2],sa$y4[2]))

sh <- da%>%filter(Metric == "Size" & Variable=="HLU")
s_hlu <- ggplot(sh,x=x1,y=y1)

spol3 <- data.frame(Class = c("Aves","Aves","Aves","Aves","Mammalia","Mammalia","Mammalia","Mammalia"),
                    x = c(sh$x1[1],sh$x2[1],sh$x3[1],sh$x4[1],sh$x1[2],sh$x2[2],sh$x3[2],sh$x4[2]),
                    y = c(sh$y2[1],sh$y1[1],sh$y3[1],sh$y4[1],sh$y2[2],sh$y1[2],sh$y3[2],sh$y4[2]))

###Mass PLOTS####
mass_plot_tpi <- mass_tpi + geom_blank() + 
  geom_segment(linetype= "solid",aes(x = x1[1], xend = x3[1], y = Intercept[1], yend = Intercept[1] + Slope[1]),color="steelblue1",linewidth=1)+
  geom_segment(linetype= "solid", aes(x = x1[2], xend = x3[2], y = Intercept[2], yend = Intercept[2] + Slope[2]),color="saddlebrown",linewidth=1)+
  scale_fill_manual(values = c("steelblue1", "saddlebrown"))+
  geom_polygon(data=masspol, aes(x=x,y=y, group=Class, fill=Class),alpha=0.2)+
  xlim(0,1)+ylim(1.4,1.75)+
  ylab("Log Body Mass (g)")+
  xlab("Thermal Position Index")+
  theme_fivethirtyeight()+
  labs(fill="")+
  theme(axis.title.y = element_text(vjust = 3,size = 14, face="bold", color="black"),
        axis.title.x = element_text(vjust = -2,size = 14, face="bold", color="red3"),
        axis.text.x = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        axis.text.y = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        legend.position = c(0.77,.995),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill ="white",color="white")
  )
plot(mass_plot_tpi)

mass_plot_ai <- mass_ai + geom_blank() + 
  geom_segment(linetype= "solid",aes(x = x1[1], xend = x3[1], y = Intercept[1], yend = Intercept[1] + (Slope[1])),color="steelblue1",linewidth=1)+
  geom_segment(linetype= "solid", aes(x = x1[2], xend = x3[2], y = Intercept[2], yend = Intercept[2] + (Slope[2])),color="saddlebrown",linewidth=1)+
  scale_fill_manual(values = c("steelblue1", "saddlebrown"))+
  geom_polygon(data=masspol2, aes(x=x,y=y, group=Class, fill=Class),alpha=0.2)+
  xlim(0,1)+ylim(1.4,1.75)+
  ylab("Log Body Mass (g)")+
  xlab("Aridity Position Index")+
  theme_fivethirtyeight()+
  labs(fill="")+
  theme(axis.title.y = element_text(vjust = 3,size = 14, face="bold", color="black"),
        axis.title.x = element_text(vjust = -2,size = 14, face="bold", color="royalblue3"),
        axis.text.x = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        axis.text.y = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        legend.position = c(0.77,.995),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill ="white",color="white")
  )
plot(mass_plot_ai)

mass_plot_hlu <- mass_hlu + geom_blank() + 
  geom_segment(linetype= "solid",aes(x = x1[1], xend = x3[1], y = Intercept[1], yend = Intercept[1] + Slope[1]),color="steelblue1",linewidth=1)+
  geom_segment(linetype= "solid", aes(x = x1[2], xend = x3[2], y = Intercept[2], yend = Intercept[2] + Slope[2]),color="saddlebrown",linewidth=1)+
  scale_fill_manual(values = c("steelblue1", "saddlebrown"))+
  geom_polygon(data=masspol3, aes(x=x,y=y, group=Class, fill=Class),alpha=0.2)+
  xlim(0,1)+ylim(1.4,1.75)+
  ylab("Log Body Mass (g)")+
  xlab("Human Land Use %")+
  theme_fivethirtyeight()+
  labs(fill="")+
  theme(axis.title.y = element_text(vjust = 3,size = 14, face="bold", color="black"),
        axis.title.x = element_text(vjust = -2,size = 14, face="bold", color="forestgreen"),
        axis.text.x = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        axis.text.y = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        legend.position = c(0.77,.995),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill ="white",color="white")
  )
plot(mass_plot_hlu)

all_mass <- ggpubr::ggarrange(mass_plot_tpi,mass_plot_ai,mass_plot_hlu,
                              ncol = 3,
                              nrow = 1,
                              labels = "AUTO",
                              legend="top",
                              common.legend=T)
plot(all_mass)
ggsave("all_mass_results.jpg",
       path="C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/plots")

###Length PLOTS####
length_plot_tpi <- l_tpi + geom_blank() + 
  geom_segment(linetype= "solid",aes(x = x1[1], xend = x3[1], y = Intercept[1], yend = Intercept[1] + Slope[1]),color="steelblue1",linewidth=1)+
  geom_segment(linetype= "solid", aes(x = x1[2], xend = x3[2], y = Intercept[2], yend = Intercept[2] + Slope[2]),color="saddlebrown",linewidth=1)+
  scale_fill_manual(values = c("steelblue1", "saddlebrown"))+
  geom_polygon(data=lengthpol, aes(x=x,y=y, group=Class, fill=Class),alpha=0.2)+
  xlim(0,1)+ylim(2.0,2.35)+
  ylab("Log Body Length (mm)")+
  xlab("Thermal Position Index")+
  theme_fivethirtyeight()+
  labs(fill="")+
  theme(axis.title.y = element_text(vjust = 3,size = 14, face="bold", color="black"),
        axis.title.x = element_text(vjust = -2,size = 14, face="bold", color="red3"),
        axis.text.x = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        axis.text.y = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        legend.position = c(0.77,.995),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill ="white",color="white")
  )
plot(length_plot_tpi)

length_plot_ai <- l_ai + geom_blank() + 
  geom_segment(linetype= "solid",aes(x = x1[1], xend = x3[1], y = Intercept[1], yend = Intercept[1] + (Slope[1])),color="steelblue1",linewidth=1)+
  geom_segment(linetype= "solid", aes(x = x1[2], xend = x3[2], y = Intercept[2], yend = Intercept[2] + (Slope[2])),color="saddlebrown",linewidth=1)+
  scale_fill_manual(values = c("steelblue1", "saddlebrown"))+
  geom_polygon(data=lengthpol2, aes(x=x,y=y, group=Class, fill=Class),alpha=0.2)+
  xlim(0,1)+ylim(2.0,2.35)+
  ylab("Log Body Length (mm)")+
  xlab("Aridity Position Index")+
  theme_fivethirtyeight()+
  labs(fill="")+
  theme(axis.title.y = element_text(vjust = 3,size = 14, face="bold", color="black"),
        axis.title.x = element_text(vjust = -2,size = 14, face="bold", color="royalblue3"),
        axis.text.x = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        axis.text.y = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        legend.position = c(0.77,.995),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill ="white",color="white")
  )
plot(length_plot_ai)

length_plot_hlu <- l_hlu + geom_blank() + 
  geom_segment(linetype= "solid",aes(x = x1[1], xend = x3[1], y = Intercept[1], yend = Intercept[1] + Slope[1]),color="steelblue1",linewidth=1)+
  geom_segment(linetype= "solid", aes(x = x1[2], xend = x3[2], y = Intercept[2], yend = Intercept[2] + Slope[2]),color="saddlebrown",linewidth=1)+
  scale_fill_manual(values = c("steelblue1", "saddlebrown"))+
  geom_polygon(data=lengthpol3, aes(x=x,y=y, group=Class, fill=Class),alpha=0.2)+
  xlim(0,1)+ylim(2.0,2.35)+
  ylab("Log Body Length (mm)")+
  xlab("Human Land Use %")+
  theme_fivethirtyeight()+
  labs(fill="")+
  theme(axis.title.y = element_text(vjust = 3,size = 14, face="bold", color="black"),
        axis.title.x = element_text(vjust = -2,size = 14, face="bold", color="forestgreen"),
        axis.text.x = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        axis.text.y = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        legend.position = c(0.77,.995),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill ="white",color="white")
  )
plot(length_plot_hlu)

all_length <- ggpubr::ggarrange(length_plot_tpi,length_plot_ai,length_plot_hlu,
                                ncol = 3,
                                nrow = 1,
                                labels = "AUTO",
                                legend="top",
                                common.legend=T)
plot(all_length)
ggsave("all_length_results.jpg",
       path="C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/plots")

###Size PLOTS####
size_plot_tpi <- s_tpi + geom_blank() + 
  geom_segment(linetype= "solid",aes(x = x1[1], xend = x3[1], y = Intercept[1], yend = Intercept[1] + Slope[1]),color="steelblue1",linewidth=1)+
  geom_segment(linetype= "solid", aes(x = x1[2], xend = x3[2], y = Intercept[2], yend = Intercept[2] + Slope[2]),color="saddlebrown",linewidth=1)+
  scale_fill_manual(values = c("steelblue1", "saddlebrown"))+
  geom_polygon(data=spol, aes(x=x,y=y, group=Class, fill=Class),alpha=0.2)+
  xlim(0,1)+ylim(0.55,0.8)+
  ylab("Log Mass:Length (g/mm)")+
  xlab("Thermal Position Index")+
  theme_fivethirtyeight()+
  labs(fill="")+
  theme(axis.title.y = element_text(vjust = 3,size = 14, face="bold", color="black"),
        axis.title.x = element_text(vjust = -2,size = 14, face="bold", color="red3"),
        axis.text.x = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        axis.text.y = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        legend.position = c(0.77,.995),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill ="white",color="white")
  )
plot(size_plot_tpi)

size_plot_ai <- s_ai + geom_blank() + 
  geom_segment(linetype= "solid",aes(x = x1[1], xend = x3[1], y = Intercept[1], yend = Intercept[1] + (Slope[1])),color="steelblue1",linewidth=1)+
  geom_segment(linetype= "solid", aes(x = x1[2], xend = x3[2], y = Intercept[2], yend = Intercept[2] + (Slope[2])),color="saddlebrown",linewidth=1)+
  scale_fill_manual(values = c("steelblue1", "saddlebrown"))+
  geom_polygon(data=spol2, aes(x=x,y=y, group=Class, fill=Class),alpha=0.2)+
  xlim(0,1)+ylim(0.55,0.8)+
  ylab("Log Mass:Length (g/mm)")+
  xlab("Aridity Position Index")+
  theme_fivethirtyeight()+
  labs(fill="")+
  theme(axis.title.y = element_text(vjust = 3,size = 14, face="bold", color="black"),
        axis.title.x = element_text(vjust = -2,size = 14, face="bold", color="royalblue3"),
        axis.text.x = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        axis.text.y = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        legend.position = c(0.77,.995),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill ="white",color="white")
  )
plot(size_plot_ai)

size_plot_hlu <- s_hlu + geom_blank() + 
  geom_segment(linetype= "solid",aes(x = x1[1], xend = x3[1], y = Intercept[1], yend = Intercept[1] + Slope[1]),color="steelblue1",linewidth=1)+
  geom_segment(linetype= "solid", aes(x = x1[2], xend = x3[2], y = Intercept[2], yend = Intercept[2] + Slope[2]),color="saddlebrown",linewidth=1)+
  scale_fill_manual(values = c("steelblue1", "saddlebrown"))+
  geom_polygon(data=spol3, aes(x=x,y=y, group=Class, fill=Class),alpha=0.2)+
  xlim(0,1)+ylim(0.55,0.8)+
  ylab("Log Mass:Length (g/mm)")+
  xlab("Human Land Use %")+
  theme_fivethirtyeight()+
  labs(fill="")+
  theme(axis.title.y = element_text(vjust = 3,size = 14, face="bold", color="black"),
        axis.title.x = element_text(vjust = -2,size = 14, face="bold", color="forestgreen"),
        axis.text.x = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        axis.text.y = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        legend.position = c(0.77,.995),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill ="white",color="white")
  )
plot(size_plot_hlu)

all_size <- ggpubr::ggarrange(size_plot_tpi,size_plot_ai,size_plot_hlu,
                              ncol = 3,
                              nrow = 1,
                              labels = "AUTO",
                              legend="top",
                              common.legend=T)
plot(all_size)
ggsave("all_size_results.jpg",
       path="C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Data for map of trends/plots")

#birds mass

tpiai_m <- ggpredict(final_mass_model2, terms=c("TPI_month_max [0:1 by=0.1]","API [0:1 by=0.5]"))
tpihlu_m <- ggpredict(final_mass_model2, terms=c("TPI_month_max [0:1 by=0.1]","HLU [0:1 by=0.5]"))

#mammal mass

tpiai_mm <- ggpredict(final_mass_model, terms=c("TPI_month_max [0:1 by=0.1]","API [0:1 by=0.5]"))
tpihlu_mm <- ggpredict(final_mass_model, terms=c("TPI_month_max [0:1 by=0.1]","HLU [0:1 by=0.5]"))

#bird length

tpiai_l <- ggpredict(final_length_model2, terms=c("TPI_month_max [0:1 by=0.1]","API [0:1 by=0.5]"))
tpihlu_l <- ggpredict(final_length_model2, terms=c("TPI_month_max [0:1 by=0.1]","HLU [0:1 by=0.5]"))

#mammal length

tpiai_lm <- ggpredict(final_length_model, terms=c("TPI_month_max [0:1 by=0.1]","API [0:1 by=0.5]"))
tpihlu_lm <- ggpredict(final_length_model, terms=c("TPI_month_max [0:1 by=0.1]","HLU [0:1 by=0.5]"))




tpiai_sm <- ggpredict(final_size_mode, terms=c("TPI_month_max [0:1 by=0.1]","API [0:1 by=0.5]"))
tpihlu_sm <- ggpredict(final_size_mode, terms=c("TPI_month_max [0:1 by=0.1]","HLU [0:1 by=0.5]"))
#mass plot

b4 <- plot(tpiai_m, ci=F,  colors=c("red3","gold3","steelblue3"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Mass (g)",
       title = "",
       color="API")+xlim(0,1)+ylim(1.46,1.56)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold",size=14, color = "black"))

b5 <- plot(tpihlu_m, ci=F, colors=c("forestgreen","grey60","saddlebrown"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Mass (g)",
       title = "",
       color="HLU %")+xlim(0,1)+ylim(1.46,1.56)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold",size=14, color = "black"))


b9 <- plot(tpiai_l, ci=F,  colors=c("red3","gold3","steelblue3"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Length (mm)",
       title = "",
       color="API")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold",size=14, color = "black"))

b10 <- plot(tpihlu_l, ci=F, colors=c("forestgreen","grey60","saddlebrown"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Length (mm)",
       title = "",
       color="HLU %")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold",size=14, color = "black"))

#mammals


m4 <- plot(tpiai_mm, ci=F,  colors=c("red3","gold3","steelblue3"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Mass (g)",
       title = "",
       color="API")+xlim(0,1)+ylim(1.59,1.64)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold",size=14, color = "black"))

m5 <- plot(tpihlu_mm, ci=F, colors=c("forestgreen","grey60","saddlebrown"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Mass (g)",
       title = "",
       color="HLU %")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold",size=14, color = "black"))



m9 <- plot(tpiai_lm, ci=F,  colors=c("red3","gold3","steelblue3"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Length (mm)",
       title = "",
       color="API")+xlim(0,1)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold",size=14, color = "black"))

m10 <- plot(tpihlu_lm, ci=F, colors=c("forestgreen","grey60","saddlebrown"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Body Length (mm)",
       title = "",
       color="HLU %")+xlim(0,1)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold",size=14, color = "black"))


m14 <- plot(tpiai_sm, ci=F,  colors=c("red3","gold3","steelblue3"), line.size = 1)+
  labs(x = "Thermal Position Index",
       y = "Log Mass:Length (g/mm)",
       title = "",
       color="API")+xlim(0,1)+ylim(0.69,0.71)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12),
        plot.title = element_text(face = "bold", size=12, color = "black",hjust = 0.5),
        legend.title = element_text(face="bold",size=14, color = "black"),
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
        legend.title = element_text(face="bold",size=14, color = "black"),
        legend.position = "top")


####
library(ggpubr)
#REMEMBER REOMVE X AXIS LABELS WHEN ALL COMBINED

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
