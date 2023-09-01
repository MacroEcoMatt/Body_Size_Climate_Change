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
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggthemes)
library(performance)
setwd("") #set to where model output files will be stored
###############################MAMMAL ANALYSIS#############################################
#change file path to location of datafiles
M_Mass <- vroom("./Mammal_Mass.csv")
M_Mass <- M_Mass%>%mutate(LMass = log10(Mass), AI = ifelse(Aridity>100,100,Aridity))
M_Length <- vroom("./Mammal_Length.csv")
M_Length <- M_Length %>%mutate(LLength = log10(Body_Length), AI = ifelse(Aridity>100,100,Aridity))
M_Size <- vroom("./Mammal_Size.csv")
M_Size <-M_Size%>%mutate(LSize = (log10(Mass)/log10(Body_Length)), AI = ifelse(Aridity>100,100,Aridity))

bt <- vroom("F:/Body Size Chapter/Chapter 2 Validation/Extra files for process/Mammal traits.csv")
bt <- bt%>%distinct()
M_Mass <- M_Mass %>%left_join(bt)%>%filter(!lifestyle=="Scansorial")
M_Length <- M_Length %>%left_join(bt)%>%filter(!lifestyle=="Scansorial")
M_Size <- M_Size %>%left_join(bt)%>%filter(!lifestyle=="Scansorial")


###Factor coding###
M_Mass$Season <- as.factor(M_Mass$Season)
M_Length$Season <- as.factor(M_Length$Season)
M_Size$Season <- as.factor(M_Size$Season)

M_Mass_c <- M_Mass
M_Length_c <- M_Length
M_Size_c <- M_Size

#contrast coding for analysis#
M_Mass$lifestyle <- as.factor(M_Mass$lifestyle)
contrasts(M_Mass$lifestyle) = contr.sum(3)
M_Mass$hibernation_torpor <- as.factor(M_Mass$hibernation_torpor)
contrasts(M_Mass$hibernation_torpor) = contr.sum(2)
M_Mass$activity_cycle <- as.factor(M_Mass$activity_cycle)
contrasts(M_Mass$activity_cycle) = contr.sum(3)

M_Mass_c$lifestyle <- factor(as.character(M_Mass_c$lifestyle), levels =c("Aerial","Ground", "Arboreal") )
contrasts(M_Mass_c$lifestyle) = contr.sum(3)
M_Mass_c$hibernation_torpor <- factor(as.character(M_Mass_c$hibernation_torpor), levels =c("Yes", "No"))
contrasts(M_Mass_c$hibernation_torpor) = contr.sum(2)
M_Mass_c$activity_cycle <- factor(as.character(M_Mass_c$activity_cycle), levels =c("All","Nocturnal", "Diurnal"))
contrasts(M_Mass_c$activity_cycle) = contr.sum(3)

M_Length$lifestyle <- as.factor(M_Length$lifestyle)
contrasts(M_Length$lifestyle) = contr.sum(3)
M_Length$hibernation_torpor <- as.factor(M_Length$hibernation_torpor)
contrasts(M_Length$hibernation_torpor) = contr.sum(2)
M_Length$activity_cycle <- as.factor(M_Length$activity_cycle)
contrasts(M_Length$activity_cycle) = contr.sum(3)

M_Length_c$lifestyle <- factor(as.character(M_Length_c$lifestyle), levels =c("Aerial","Ground", "Arboreal") )
contrasts(M_Length_c$lifestyle) = contr.sum(3)
M_Length_c$hibernation_torpor <- factor(as.character(M_Length_c$hibernation_torpor), levels =c("Yes", "No"))
contrasts(M_Length_c$hibernation_torpor) = contr.sum(2)
M_Length_c$activity_cycle <- factor(as.character(M_Length_c$activity_cycle), levels =c("All","Nocturnal", "Diurnal"))
contrasts(M_Length_c$activity_cycle) = contr.sum(3)

M_Size$lifestyle <- as.factor(M_Size$lifestyle)
contrasts(M_Size$lifestyle) = contr.sum(3)
M_Size$hibernation_torpor <- as.factor(M_Size$hibernation_torpor)
contrasts(M_Size$hibernation_torpor) = contr.sum(2)
M_Size$activity_cycle <- as.factor(M_Size$activity_cycle)
contrasts(M_Size$activity_cycle) = contr.sum(3)

M_Size_c$lifestyle <- factor(as.character(M_Size_c$lifestyle), levels =c("Aerial", "Ground", "Arboreal"))
contrasts(M_Size_c$lifestyle) = contr.sum(3)
M_Size_c$hibernation_torpor <- factor(as.character(M_Size_c$hibernation_torpor), levels =c("Yes", "No"))
contrasts(M_Size_c$hibernation_torpor) = contr.sum(2)
M_Size_c$activity_cycle <- factor(as.character(M_Size_c$activity_cycle), levels =c("All","Nocturnal", "Diurnal"))
contrasts(M_Size_c$activity_cycle) = contr.sum(3)

###check for influence of extreme AI values####
#use to build model with winsorized extreme values
#If model results differ use winsorized model
nrow(M_Mass%>%filter(AI > mean(AI)+(3*sd(AI))))/nrow(M_Mass)
nrow(M_Length%>%filter(AI > mean(AI)+(3*sd(AI))))/nrow(M_Length)
nrow(M_Size%>%filter(AI > mean(AI)+(3*sd(AI))))/nrow(M_Size)

M_Mass2 <- M_Mass %>% mutate(AI_win = DescTools::Winsorize(AI, probs = c(0.008,0.992)))
M_Length2 <- M_Length %>% mutate(AI_win = DescTools::Winsorize(AI, probs = c(0.008,0.992)))
M_Size2 <- M_Size %>% mutate(AI_win = DescTools::Winsorize(AI, probs = c(0.009,0.991)))
####MASS ANALYSIS####

#spatial variogram
sp_mass_mod <- lme(LMass ~ 1,
                   random = ~1|Binomial, 
                   data= M_Mass)
plot(Variogram(sp_mass_mod, form=~Lat+Lon|Binomial, maxDist = 5))

#FULL MASS MODEL
mass_model <- lmer(LMass ~ TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + hibernation_torpor +
                     TPI_month_max:AI + TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                     (1|Binomial),data=M_Mass,REML=F)

mass_model_winsor <- lmer(LMass ~ TPI_month_max + AI_win + HLU +
                              lifestyle + activity_cycle + hibernation_torpor +
                              TPI_month_max:AI_win + TPI_month_max:HLU +
                              TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                              (1|Binomial),data=M_Mass2)
tab_model(mass_model,mass_model_winsor)
#no change to results significance - continue with mass_model
mass_model_step <- step(mass_model,direction="backward")

mass_model_step_top <- get_model(mass_model_step)

AIC(mass_model,mass_model_step_top)

tab_model(mass_model,mass_model_step_top, digits=5)

check_model(mass_model)
#collect coef for figure generation
coef_mass <- coef(summary(mass_model_step_top))
write.csv(coef_mass, "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Figure Generation/Mam_Mass_API_coef.csv")

#after step wise removal based on AIC and BIC
#FINAL MASS MODEL

final_mass_model <- lmer(LMass ~ TPI_month_max + AI + HLU +
                           lifestyle + activity_cycle + hibernation_torpor +
                           TPI_month_max:AI + TPI_month_max:HLU +
                           TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                           (1|Binomial),data=M_Mass)
tab_model(final_mass_model, digits=5)

#contrast coded complimentary model
final_mass_model_c <- lmer(LMass ~ TPI_month_max + AI + HLU +
                             lifestyle + activity_cycle + hibernation_torpor +
                             TPI_month_max:AI + TPI_month_max:HLU +
                             TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                             (1|Binomial),data=M_Mass_c)

check_singularity(final_mass_model)
check_outliers(final_mass_model)
qqnorm(residuals(final_mass_model))

plot(final_mass_model)

tab_model(final_mass_model,final_mass_model_c, digits=5, 
          file = "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mammal_Mass_Results.html")

sink("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mammal_mass_LME.txt")
"Mammal Mass Model LME"
summary(final_mass_model)
"R squared"
print(MuMIn::r.squaredGLMM(final_mass_model))
""
""
"Contrast"
summary(final_mass_model_c)
"R squared"
print(MuMIn::r.squaredGLMM(final_mass_model_c))
sink()

####LENGTH ANALYSIS####
#spatial variogram
sp_length_mod <- lme(LLength ~ 1,
                     random = ~1|Binomial, 
                     data= M_Length)
plot(Variogram(sp_length_mod, form=~Lat+Lon|Binomial, maxDist = 5))
mlm <- M_Length%>%filter(!is.na(API))
#FULL LENGTH MODEL
length_model <- lmer(LLength ~ TPI_month_max + AI + HLU +
                       lifestyle + activity_cycle + hibernation_torpor +
                       TPI_month_max:AI + TPI_month_max:HLU +
                       TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                       (1|Binomial),data=M_Length,REML = F,
                     control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

length_model_winsor<- lmer(LLength ~ TPI_month_max + AI_win + HLU +
                              lifestyle + activity_cycle + hibernation_torpor +
                              TPI_month_max:AI_win + TPI_month_max:HLU +
                              TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                              (1|Binomial),data=M_Length2,REML = F,
                            control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
tab_model(length_model,length_model_winsor)
#no difference continue with length_model
length_model_step <- step(length_model)

length_model_step_top <- get_model(length_model_step)

AIC(length_model,length_model_step_top)
tab_model(length_model,length_model_step_top, digits=5)

#collect coef for figure generation
coef_length <- coef(summary(length_model_step_top))
write.csv(coef_length, "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Figure Generation/Mam_Length_coef.csv")

#after step wise removal based on AIC and BIC
#FINAL LENGTH MODEL

final_length_model <- lmer(LLength ~ TPI_month_max + AI + HLU +
                             lifestyle + activity_cycle + hibernation_torpor +
                             TPI_month_max:AI + TPI_month_max:HLU +
                             TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                             (1|Binomial),data=M_Length,REML = T,
                           control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

#contrast coded complimentary model
final_length_model_c <- lmer(LLength ~ TPI_month_max + AI + HLU +
                               lifestyle + activity_cycle + hibernation_torpor +
                               TPI_month_max:AI + TPI_month_max:HLU +
                               TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                               (1|Binomial),data=M_Length_c,REML = T,
                             control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

check_singularity(final_length_model)
check_outliers(final_length_model)

qqnorm(residuals(final_length_model))
plot(final_length_model)

check_model(final_length_model)

tab_model(final_length_model,final_length_model_c, digits=5, 
          file = "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mammal_Length_Results.html")

sink("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mammal_length_LME.txt")
"Mammal Length Model LME"
summary(final_length_model)
"R squared"
print(MuMIn::r.squaredGLMM(final_length_model))
""
""
"Contrast"
summary(final_length_model_c)
"R squared"
print(MuMIn::r.squaredGLMM(final_length_model_c))
sink()

####MASS:LENGTH ANALYSIS####

#spatial variogram
sp_size_mod <- lme(LSize ~ 1,
                   random = ~1|Binomial, 
                   data= SIZE_MASS_INDEX)
plot(Variogram(sp_size_mod, form=~Lat+Lon|Binomial, maxDist = 5))

#FULL MASS:LENGTH MODEL

size_model <- lmer(LSize ~ TPI_month_max + AI + HLU +
                        lifestyle + activity_cycle + hibernation_torpor +
                        TPI_month_max:AI + TPI_month_max:HLU +
                        TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                        (1|Binomial),data=M_Size, REML=F,
                      control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

size_model_2 <- lmer(LSize ~ TPI_month_max + AI_win + HLU +
                            lifestyle + activity_cycle + hibernation_torpor +
                            TPI_month_max:AI_win + TPI_month_max:HLU +
                            TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                            (1|Binomial),data=M_Size2, REML=T,
                          control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
tab_model(size_model,size_model_2)
#no difference continue with size_model
ml_step <- step(size_model_ml)

ml_top <- get_model(ml_step)

tab_model(size_model_ml,ml_top, digits = 5)

AIC(ml_top)


#collect coef for figure generation
coef_size_size <- coef(summary(ml_top))

write.csv(coef_size_size, "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Figure Generation/Mam_masslength_coef.csv")

#after step wise removal based on AIC and BIC
#FINAL MASS:LENGTH MODEL

#contrast coded complimentary model
final_size_mode <- lmer(LSize ~ TPI_month_max + AI + HLU +
                             lifestyle + activity_cycle + hibernation_torpor +
                             TPI_month_max:AI + TPI_month_max:HLU +
                             TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                             (1|Binomial),data=M_Size, REML=T,
                           control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
final_size_mode_c <- lmer(LSize ~ TPI_month_max + AI + HLU +
                               lifestyle + activity_cycle + hibernation_torpor +
                               TPI_month_max:AI + TPI_month_max:HLU +
                               TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                               (1|Binomial),data=M_Size_c, REML=T,
                             control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


check_singularity(final_size_mode)
check_outliers(final_size_mode)

qqnorm(residuals(final_size_mode))
plot(final_size_mode)

check_model(final_size_mode)

tab_model(final_size_mode,final_size_mode_c, digits=5, 
          file = "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mammal_SMI_Results.html")

sink("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mammal_size_LME.txt")
"Mammal SIZE Model LME"
summary(final_size_modelml)
"R squared"
print(MuMIn::r.squaredGLMM(final_size_modelml))
""
""
"Contrast"
summary(final_size_modelml_c)
"R squared"
print(MuMIn::r.squaredGLMM(final_size_modelml_c))
sink()
###############################BIRD ANALYSIS#############################################
#change file path to location of datafiles
B_Mass <- vroom("./Bird_Mass_Official.csv")%>%mutate(LMass = log10(Mass), AI = ifelse(Aridity>100,100,Aridity))
B_Length <- vroom("./Bird_Length_Official.csv")%>%mutate(LLength = log10(Body_Length), AI = ifelse(Aridity>100,100,Aridity))
B_Size <- vroom("./Bird_Size_Official.csv")%>%mutate(LSize = (log10(Mass)/log10(Body_Length)), AI = ifelse(Aridity>100,100,Aridity))


B_Mass_out$lifestyle <- as.factor(B_Mass_out$lifestyle)
contrasts(B_Mass_out$lifestyle) = contr.sum(5)
B_Mass_out$Migration <- as.factor(B_Mass_out$Migration)
contrasts(B_Mass_out$Migration) = contr.sum(3)
B_Mass_out$activity_cycle <- as.factor(B_Mass_out$activity_cycle)
contrasts(B_Mass_out$activity_cycle) = contr.sum(2)
B_Length_out$lifestyle <- as.factor(B_Length_out$lifestyle)
contrasts(B_Length_out$lifestyle) = contr.sum(5)
B_Length_out$Migration <- as.factor(B_Length_out$Migration)
contrasts(B_Length_out$Migration) = contr.sum(3)
B_Length_out$activity_cycle <- as.factor(B_Length_out$activity_cycle)
contrasts(B_Length_out$activity_cycle) = contr.sum(2)
B_Size_out$lifestyle <- as.factor(B_Size_out$lifestyle)
contrasts(B_Size_out$lifestyle) = contr.sum(5)
B_Size_out$Migration <- as.factor(B_Size_out$Migration)
contrasts(B_Size_out$Migration) = contr.sum(3)
B_Size_out$activity_cycle <- as.factor(B_Size_out$activity_cycle)
contrasts(B_Size_out$activity_cycle) = contr.sum(2)
###Factor coding###
B_Mass$Season <- as.factor(B_Mass$Season)
B_Size$Season <- as.factor(B_Size$Season)

B_Mass_c <- B_Mass
B_Length_c <- B_Length
B_Size_c <- B_Size

#contrast coding for analysis#
B_Mass$lifestyle <- as.factor(B_Mass$lifestyle)
contrasts(B_Mass$lifestyle) = contr.sum(5)
B_Mass$Migration <- as.factor(B_Mass$Migration)
contrasts(B_Mass$Migration) = contr.sum(3)
B_Mass$activity_cycle <- as.factor(B_Mass$activity_cycle)
contrasts(B_Mass$activity_cycle) = contr.sum(2)

B_Mass_c$lifestyle <- factor(as.character(B_Mass_c$lifestyle), levels =c("Aerial", "Aquatic", "Arboreal", "Ground", "Generalist") )
contrasts(B_Mass_c$lifestyle) = contr.sum(5)
B_Mass_c$Migration <- factor(as.character(B_Mass_c$Migration), levels =c("Migratory", "Sedentary", "Partially Migratory"))
contrasts(B_Mass_c$Migration) = contr.sum(3)
B_Mass_c$activity_cycle <- factor(as.character(B_Mass_c$activity_cycle), levels =c("Nocturnal", "Diurnal"))
contrasts(B_Mass_c$activity_cycle) = contr.sum(2)

B_Length$lifestyle <- as.factor(B_Length$lifestyle)
contrasts(B_Length$lifestyle) = contr.sum(5)
B_Length$Migration <- as.factor(B_Length$Migration)
contrasts(B_Length$Migration) = contr.sum(3)
B_Length$activity_cycle <- as.factor(B_Length$activity_cycle)
contrasts(B_Length$activity_cycle) = contr.sum(2)

B_Length_c$lifestyle <- factor(as.character(B_Length_c$lifestyle), levels =c("Aerial", "Aquatic", "Arboreal", "Ground", "Generalist") )
contrasts(B_Length_c$lifestyle) = contr.sum(5)
B_Length_c$Migration <- factor(as.character(B_Length_c$Migration), levels =c("Migratory", "Sedentary", "Partially Migratory"))
contrasts(B_Length_c$Migration) = contr.sum(3)
B_Length_c$activity_cycle <- factor(as.character(B_Length_c$activity_cycle), levels =c("Nocturnal", "Diurnal"))
contrasts(B_Length_c$activity_cycle) = contr.sum(2)

B_Size$lifestyle <- as.factor(B_Size$lifestyle)
contrasts(B_Size$lifestyle) = contr.sum(5)
B_Size$Migration <- as.factor(B_Size$Migration)
contrasts(B_Size$Migration) = contr.sum(3)
B_Size$activity_cycle <- as.factor(B_Size$activity_cycle)
contrasts(B_Size$activity_cycle) = contr.sum(2)

B_Size_c$lifestyle <- factor(as.character(B_Size_c$lifestyle), levels =c("Aerial", "Aquatic", "Arboreal", "Ground", "Generalist"))
contrasts(B_Size_c$lifestyle) = contr.sum(5)
B_Size_c$Migration <- factor(as.character(B_Size_c$Migration), levels =c("Migratory", "Sedentary", "Partially Migratory"))
contrasts(B_Size_c$Migration) = contr.sum(3)
B_Size_c$activity_cycle <- factor(as.character(B_Size_c$activity_cycle), levels =c("Nocturnal", "Diurnal"))
contrasts(B_Size_c$activity_cycle) = contr.sum(2)

###check for influence of extreme AI values####
#use to build model with winsorized extreme values
#If model results differ use winsorized model
nrow(B_Mass%>%filter(AI > mean(AI)+(3*sd(AI))))/nrow(B_Mass)
nrow(B_Length%>%filter(AI > mean(AI)+(3*sd(AI))))/nrow(B_Length)
nrow(B_Size%>%filter(AI > mean(AI)+(3*sd(AI))))/nrow(B_Size)

B_Mass2 <- B_Mass %>% mutate(AI_win = DescTools::Winsorize(AI, probs = c(0.008,0.992)))
B_Length2 <- B_Length %>% mutate(AI_win = DescTools::Winsorize(AI, probs = c(0.014,0.986)))
B_Size2 <- B_Size %>% mutate(AI_win = DescTools::Winsorize(AI, probs = c(0.004,0.996)))
####for models that are different 
B_Length_c2 <- B_Length2
B_Size_c2 <- B_Size2

B_Length_c2$lifestyle <- factor(as.character(B_Length_c2$lifestyle), levels =c("Aerial", "Aquatic", "Arboreal", "Ground", "Generalist") )
contrasts(B_Length_c2$lifestyle) = contr.sum(5)
B_Length_c2$Migration <- factor(as.character(B_Length_c2$Migration), levels =c("Migratory", "Sedentary", "Partially Migratory"))
contrasts(B_Length_c2$Migration) = contr.sum(3)
B_Length_c2$activity_cycle <- factor(as.character(B_Length_c2$activity_cycle), levels =c("Nocturnal", "Diurnal"))
contrasts(B_Length_c2$activity_cycle) = contr.sum(2)

B_Size_c2$lifestyle <- factor(as.character(B_Size_c2$lifestyle), levels =c("Aerial", "Aquatic", "Arboreal", "Ground", "Generalist"))
contrasts(B_Size_c2$lifestyle) = contr.sum(5)
B_Size_c2$Migration <- factor(as.character(B_Size_c2$Migration), levels =c("Migratory", "Sedentary", "Partially Migratory"))
contrasts(B_Size_c2$Migration) = contr.sum(3)
B_Size_c2$activity_cycle <- factor(as.character(B_Size_c2$activity_cycle), levels =c("Nocturnal", "Diurnal"))
contrasts(B_Size_c2$activity_cycle) = contr.sum(2)

####MASS ANALYSIS####

#spatial variogram
sp_mass_mod_b <- lme(LMass ~ 1,
                     random = ~1|Binomial, 
                     data= B_Mass)
plot(Variogram(sp_mass_mod_b, form=~Lat+Lon|Binomial, maxDist = 5))

#FULL MASS MODEL
mass_model_b <- lmer(LMass ~ TPI_month_max + AI + HLU +
                       lifestyle + activity_cycle + Migration +
                       TPI_month_max:AI + TPI_month_max:HLU +
                       TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:Migration +
                       (1|Binomial),data=B_Mass, REML=F,
                     control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

mass_model_b_2 <- lmer(LMass ~ TPI_month_max + AI_win + HLU +
                            lifestyle + activity_cycle + Migration +
                            TPI_month_max:AI_win + TPI_month_max:HLU +
                            TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:Migration +
                            (1|Binomial),data=B_Mass2, REML=F,
                          control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
tab_model(mass_model_b,mass_model_b_2)
#no difference continue with mass model

mass_model_b_step <- step(mass_model_b)
plot(mass_model_b_step)
mass_model_b_step_top <- get_model(mass_model_b_step)

AIC(mass_model_b,mass_model_b_step_top)

tab_model(mass_model_b,mass_model_b_step_top, digits=5)

#collect coef for figure generation
coef_mass <- coef(summary(mass_model_b_step_top))
write.csv(coef_mass, "./Figure Generation/Bird_Mass_coef.csv")

#after step wise removal based on AIC and BIC
#FINAL MASS MODEL

final_mass_model_b <- lmer(LMass ~ TPI_month_max + AI + HLU +
                             lifestyle + activity_cycle + Migration +
                             TPI_month_max:AI + TPI_month_max:HLU +
                             TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:Migration +
                             (1|Binomial),data=B_Mass, REML=T,
                           control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


#contrast coded complimentary model
final_mass_model_c_b <- lmer(LMass ~ TPI_month_max + AI + HLU +
                               lifestyle + activity_cycle + Migration +
                               TPI_month_max:AI + TPI_month_max:HLU +
                               TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:Migration +
                               (1|Binomial),data=B_Mass_c, REML=T,
                             control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

performance::check_singularity(final_mass_model_b)
performance::check_model(final_mass_model_b)

tab_model(final_mass_model_b,final_mass_model_c_b, digits=5, file = "./Results/Bird_Mass_Results.html")

sink("./Results/Bird_Mass_Results.txt")
"Bird Mass Model LME"
summary(final_mass_model_b)
"R squared"
print(MuMIn::r.squaredGLMM(final_mass_model_b))
""
""
"Contrast"
summary(final_mass_model_c_b)
"R squared"
print(MuMIn::r.squaredGLMM(final_mass_model_c_b))
sink()

####LENGTH ANALYSIS####
#spatial variogram
sp_length_mod_b <- lme(LLength ~ 1,
                       random = ~1|Binomial, 
                       data= B_Length)
plot(Variogram(sp_length_mod_b, form=~Lat+Lon|Binomial, maxDist = 5))

#FULL LENGTH MODEL
length_model_b <- lmer(LLength ~ TPI_month_max + AI + HLU +
                         lifestyle + activity_cycle + Migration +
                         TPI_month_max:AI + TPI_month_max:HLU +
                         TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:Migration +
                         (1|Binomial),data=B_Length,REML=F,
                       control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
length_model_b_2 <- lmer(LLength ~ TPI_month_max + AI_win + HLU +
                              lifestyle + activity_cycle + Migration +
                              TPI_month_max:AI_win + TPI_month_max:HLU +
                              TPI_month_max:lifestyle + TPI_month_max:Migration +
                              (1|Binomial),data=B_Length2,REML=T,
                            control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
tab_model(length_model_b, length_model_b_2)
#DIFFERENCE - extreme AIs overly influence model, continue with length_model_b_2

length_model_b_step <- step(length_model_b_2)
plot(length_model_b_step)
length_model_b_step_top <- get_model(length_model_b_step)

AIC(length_model_b_2,length_model_b_step_top)

tab_model(length_model_b_2,length_model_b_step_top, digits=5)

#collect coef for figure generation
coef_length <- coef(summary(length_model_b_step_top))
write.csv(coef_length, "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Figure Generation/Bird_Length_coef.csv")


#after step wise removal based on AIC and BIC
#FINAL LENGTH MODEL

final_length_model_b <- lmer(LLength ~ TPI_month_max + AI + HLU +
                               lifestyle + activity_cycle + Migration +
                               TPI_month_max:AI + TPI_month_max:HLU +
                               TPI_month_max:lifestyle + TPI_month_max:Migration +
                               (1|Binomial),data=B_Length2,REML=T,
                             control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


#contrast coded complimentary model
final_length_model_c_b <- lmer(LLength ~ TPI_month_max + AI + HLU +
                                 lifestyle + activity_cycle + Migration +
                                 TPI_month_max:AI + TPI_month_max:HLU +
                                 TPI_month_max:lifestyle + TPI_month_max:Migration +
                                 (1|Binomial),data=B_Length_c2,REML=T,
                               control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


performance::check_singularity(final_length_model_b)
performance::check_model(final_length_model_b)

tab_model(final_length_model_b,final_length_model_c_b, digits=5, file = "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter//Results/Bird_Length_Results.html")

sink("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter//Results/Bird_length_LME.txt")
"Bird Length Model LME"
summary(final_length_model_b)
"R squared"
print(MuMIn::r.squaredGLMM(final_length_model_b))
""
""
"Contrast"
summary(final_length_model_c_b)
"R squared"
print(MuMIn::r.squaredGLMM(final_length_model_c_b))
sink()

####MASS:LENGTH ANALYSIS####
#spatial variogram
sp_size_mod_b <- lme(LSize ~ 1,
                     random = ~1|Binomial, 
                     data= B_Size)
plot(Variogram(sp_size_mod_b, form=~Lat+Lon|Binomial, maxDist = 5))

#FULL LENGTH MODEL
#AI < 75 set as filter to deal with outlier variable
size_model_b <- lmer(LSize ~ TPI_month_max + AI + HLU +
                       lifestyle + activity_cycle + Migration +
                       TPI_month_max:AI + TPI_month_max:HLU +
                       TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:Migration +
                       (1|Binomial),data=B_Size,REML=F,
                     control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
size_model_b_2 <- lmer(LSize ~ TPI_month_max + AI_win + HLU +
                            lifestyle + activity_cycle + Migration+
                            TPI_month_max:AI_win + TPI_month_max:HLU+
                            TPI_month_max:lifestyle+ TPI_month_max:Migration+ TPI_month_max:activity_cycle+
                            (1|Binomial),data=B_Size2,REML = T,
                          control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

tab_model(size_model_b,size_model_b_2,digits=4)
#DIFFERENCE - continue with size_model_b_2

size_model_b_step <- step(size_model_b_2)
plot(size_model_b_step)
size_model_b_step_top <- get_model(size_model_b_step)
AIC(size_model_b_2,size_model_b_step_top)
tab_model(size_model_b_2,size_model_b_step_top, digits=5)

#collect coef for figure generation
coef_size <- coef(summary(size_model_b_step_top))
write.csv(coef_size, "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Figure Generation/Bird_Size_coef.csv")
#after step wise removal based on AIC and BIC
#FINAL LENGTH MODEL

final_size_model_b <- lmer(LSize ~ TPI_month_max + AI + HLU +
                             lifestyle + activity_cycle + Migration+
                             TPI_month_max:activity_cycle + TPI_month_max:lifestyle+
                             TPI_month_max:Migration+
                             (1|Binomial),data=B_Size2,REML = T,
                           control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

#contrast coded complimentary model
final_size_model_c_b <- lmer(LSize ~ TPI_month_max + AI + HLU +
                               lifestyle + activity_cycle + Migration+
                               TPI_month_max:activity_cycle + TPI_month_max:lifestyle+
                               TPI_month_max:Migration+
                               (1|Binomial),data=B_Size_c2,REML = T,
                             control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


check_singularity(final_size_model_b)
check_model(final_size_model_b)

tab_model(final_size_model_b,final_size_model_c_b, digits=5, file = "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Bird_Size_Results.html")

sink("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Bird_size_smi_LME.txt")
"Bird Size Model LME"
summary(final_size_model_b)
"R squared"
print(MuMIn::r.squaredGLMM(final_size_model_b))
""
""
"Contrast"
summary(final_size_model_c_b)
"R squared"
print(MuMIn::r.squaredGLMM(final_size_model_c_b))
sink()

#####FIGURE GENERATION######
##Bird Response and Mammal Response to tpi by trait####
#BIRD####
#MASS
tpi_bird_life <- ggpredict(final_mass_model_b, terms=c("TPI_month_max","lifestyle"))
tpi1 <- plot(tpi_bird_life, colors=c("darkorange2","steelblue1", "forestgreen","maroon2", "saddlebrown"), line.size = 1.3)+
  labs(x = "Thermal Position Index",
       y = "Log10 Body Mass (g)",
       title = "",
       color="")+xlim(0,1)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "bottom",
  )
tpi_bird_ac <- ggpredict(final_mass_model_b, terms=c("TPI_month_max","activity_cycle"))
ac1<- plot(tpi_bird_ac, colors=c("goldenrod2", "black"), line.size = 1.3)+
  labs(x = "Thermal Position Index",
       y = "Log10 Body  Mass (g)",
       title = "",
       color="")+xlim(0,1)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "bottom",
  )
ac1
tpi_bird_ht <- ggpredict(final_mass_model_b, terms=c("TPI_month_max","Migration"))
h1<- plot(tpi_bird_ht, colors=c("orangered3","royalblue3","grey30"), line.size = 1.3)+
  labs(x = "Thermal Position Index",
       y = "Log10 Body  Mass (g)",
       title = "",
       color="")+xlim(0,1)+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "bottom",
  )

tpi_bird_life2 <- ggpredict(final_length_model_b, terms=c("TPI_month_max","lifestyle"))
tpi2 <- plot(tpi_bird_life2, colors=c("darkorange2","steelblue1", "forestgreen","maroon2", "saddlebrown"), line.size = 1.3)+
  labs(x = "Thermal Position Index",
       y = "Log10 Body Length (mm)",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "bottom",
  )
tpi_bird_ac2 <- ggpredict(final_length_model_b, terms=c("TPI_month_max","activity_cycle"))
ac2<- plot(tpi_bird_ac2, colors=c("goldenrod2", "black"), line.size = 1.3)+
  labs(x = "Thermal Position Index",
       y = "Log10 Body Length (mm)",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "bottom",
  )
tpi_bird_ht2 <- ggpredict(final_length_model_b, terms=c("TPI_month_max","Migration"))
m2<- plot(tpi_bird_ht2, colors=c("orangered3","royalblue3","grey30"), line.size = 1.3)+
  labs(x = "Thermal Position Index",
       y = "Log10 Body Length (mm)",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "bottom",
  )

tpi_bird_life3 <- ggpredict(final_size_model_b, terms=c("TPI_month_max","lifestyle"))
tpi3 <- plot(tpi_bird_life3, colors=c("darkorange2","steelblue1", "forestgreen","maroon2", "saddlebrown"), line.size = 1.3)+
  labs(x = "Thermal Position Index",
       y = "Mass:Length (g/mm)",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "bottom",
  )
tpi_bird_ac3 <- ggpredict(final_size_model_b, terms=c("TPI_month_max","activity_cycle"))
ac3<- plot(tpi_bird_ac3, colors=c("goldenrod2", "black"), line.size = 1.3)+
  labs(x = "Thermal Position Index",
       y = "Mass:Length (g/mm)",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "bottom",
  )
tpi_bird_ht3 <- ggpredict(final_size_model_b, terms=c("TPI_month_max","Migration"))
m3<- plot(tpi_bird_ht3, colors=c("orangered3","royalblue3","grey30"), line.size = 1.3)+
  labs(x = "Thermal Position Index",
       y = "Mass:Length (g/mm)",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "bottom",
  )
life_bird <- ggpubr::ggarrange(tpi1, tpi2,tpi3,
                  labels = c("A", "B", "C"),
                  ncol = 3, nrow = 1,common.legend = TRUE,legend="bottom",label.x = 0.1)
life_bird

ac_bird <- ggpubr::ggarrange( ac1,ac3,
                               labels = c("A","B"),
                               ncol = 2, nrow = 1,common.legend = TRUE,legend="bottom",label.x = 0.1)

ac_bird
m_bird <- ggpubr::ggarrange(h1, m2,m3,
                               labels = c("A", "B", "C"),
                               ncol = 3, nrow = 1,common.legend = TRUE,legend="bottom",label.x = 0.1)

m_bird
##MAMMAL####
#MASS

tpi_mam_life <- ggpredict(final_mass_model, terms=c("TPI_month_max","lifestyle"))
tpi4<-plot(tpi_mam_life, colors=c("darkorange2", "forestgreen", "saddlebrown"), line.size = 1.3)+
  labs(x = "Thermal Position Index",
       y = "Log10 Body Mass (g)",
       title = "",
       color="")+xlim(0,1)+
  theme(axis.title = element_text(face="bold",size=16, color="black"),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "bottom",
  )
tpi_mam_ac <- ggpredict(final_mass_model, terms=c("TPI_month_max","activity_cycle"))
ac4<-plot(tpi_mam_ac, colors=c("mediumpurple1","goldenrod2", "black"), line.size = 1.3)+
  labs(x = "Thermal Position Index",
       y = "Log10 Body Mass (g)",
       title = "",
       color="")+xlim(0,1)+
  theme(axis.title = element_text(face="bold",size=16, color="black"),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "bottom",
  )
tpi_mam_ht <- ggpredict(final_mass_model, terms=c("TPI_month_max","hibernation_torpor"))
h4<-plot(tpi_mam_ht, colors=c("turquoise3","tan"), line.size = 1.3)+
  labs(x = "Thermal Position Index",
       y = "Log10 Body Mass (g)",
       title = "",
       color="")+xlim(0,1)+
  theme(axis.title = element_text(face="bold",size=16, color="black"),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "bottom",
  )

tpi_mam_life <- ggpredict(final_length_model, terms=c("TPI_month_max","lifestyle"))
tpi5<-plot(tpi_mam_life, colors=c("darkorange2", "forestgreen", "saddlebrown"), line.size = 1.3)+
  labs(x = "Thermal Position Index",
       y = "Log10 Body Length (mm)",
       title = "",
       color="")+xlim(0,1)+
  theme(axis.title = element_text(face="bold",size=16, color="black"),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "bottom",
  )
tpi_mam_ac <- ggpredict(final_length_model, terms=c("TPI_month_max","activity_cycle"))
ac5<-plot(tpi_mam_ac, colors=c("mediumpurple1","goldenrod2", "black"), line.size = 1.3)+
  labs(x = "Thermal Position Index",
       y = "Log10 Body Length (mm)",
       title = "",
       color="")+xlim(0,1)+
  theme(axis.title = element_text(face="bold",size=16, color="black"),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "bottom",
  )
tpi_mam_ht <- ggpredict(final_length_model, terms=c("TPI_month_max","hibernation_torpor"))
h5<-plot(tpi_mam_ht, colors=c("turquoise3","tan"), line.size = 1.3)+
  labs(x = "Thermal Position Index",
       y = "Log10 Body Length (mm)",
       title = "",
       color="")+xlim(0,1)+
  theme(axis.title = element_text(face="bold",size=16, color="black"),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "bottom",
  )

tpi_mam_life <- ggpredict(final_size_mode, terms=c("TPI_month_max","lifestyle"))
tpi6<-plot(tpi_mam_life, colors=c("darkorange2", "forestgreen", "saddlebrown"), line.size = 1.3)+
  labs(x = "Thermal Position Index",
       y = "Mass:Length (g/mm)",
       title = "",
       color="")+xlim(0,1)+
  theme(axis.title = element_text(face="bold",size=16, color="black"),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "bottom",
  )
tpi_mam_ac <- ggpredict(final_size_mode, terms=c("TPI_month_max","activity_cycle"))
ac6<-plot(tpi_mam_ac, colors=c("mediumpurple1","goldenrod2", "black"), line.size = 1.3)+
  labs(x = "Thermal Position Index",
       y = "Mass:Length (g/mm)",
       title = "",
       color="")+xlim(0,1)+
  theme(axis.title = element_text(face="bold",size=16, color="black"),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "bottom",
  )
tpi_mam_ht <- ggpredict(final_size_mode, terms=c("TPI_month_max","hibernation_torpor"))
h6<- plot(tpi_mam_ht, colors=c("turquoise3","tan"), line.size = 1.3)+
  labs(x = "Thermal Position Index",
       y = "Mass:Length (g/mm)",
       title = "",
       color="")+xlim(0,1)+
  theme(axis.title = element_text(face="bold",size=16, color="black"),
        axis.text = element_text(size=14, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "bottom",
  )

life_mam <- ggpubr::ggarrange(tpi4, tpi5,tpi6,
                               labels = c("A", "B", "C"),
                               ncol = 3, nrow = 1,common.legend = TRUE,legend="bottom",label.x = 0.1)
life_mam

ac_mam <- ggpubr::ggarrange( ac4, ac5, ac6,
                              labels = c("A", "B", "C"),
                              ncol = 3, nrow = 1,common.legend = TRUE,legend="bottom",label.x = 0.1)

ac_mam

h_mam <- ggpubr::ggarrange(h4, h5,h6,
                            labels = c("A", "B", "C"),
                            ncol = 3, nrow = 1,common.legend = TRUE,legend="bottom",label.x = 0.1)

h_mam
