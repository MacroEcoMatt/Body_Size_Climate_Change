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
M_Mass <- vroom("./Mammal_Mass.csv")%>%mutate(LMass = log10(Mass), AI = ifelse(Aridity>100,100,Aridity))
M_Length <- vroom("./Mammal_Length.csv")%>%mutate(LLength = log10(Body_Length), AI = ifelse(Aridity>100,100,Aridity))
M_Size <- vroom("./Mammal_Size.csv")%>%mutate(LSize = (log10(Mass)/log10(Body_Length)), AI = ifelse(Aridity>100,100,Aridity))


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

mass_model_step <- step(mass_model)

mass_model_step_top <- get_model(mass_model_step)

AIC(mass_model,mass_model_step_top)

tab_model(mass_model,mass_model_step_top, digits=5)

#collect coef for figure generation
coef_mass <- coef(summary(mass_model_step_top))
write.csv(coef_mass, "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Figure Generation/Mam_Mass_coef.csv")

#after step wise removal based on AIC and BIC
#FINAL MASS MODEL

final_mass_model <- lmer(LMass ~ TPI_month_max + AI + HLU +
                           lifestyle + activity_cycle + hibernation_torpor +
                           TPI_month_max:AI + TPI_month_max:HLU +
                           TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                           (1|Binomial),data=M_Mass)

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

#FULL LENGTH MODEL
length_model <- lmer(LLength ~ TPI_month_max + AI + HLU +
                       lifestyle + activity_cycle + hibernation_torpor +
                       TPI_month_max:AI + TPI_month_max:HLU +
                       TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                       (1|Binomial),data=M_Length,REML = F,
                     control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

length_model_step <- step(length_model)

length_model_step_top <- get_model(length_model_step)

AIC(length_model,length_model_step_top)

tab_model(length_model,length_model_step_top, digits=5)

#collect coef for figure generation
coef_length <- coef(summary(length_model_step_top))
write.csv(coef_length, "./Figure Generation/Mam_Length_coef.csv")

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

size_model_ml <- lmer(LSize ~ TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + hibernation_torpor +
                     TPI_month_max:AI + TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                     (1|Binomial),data=M_Size2, REML=F,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


ml_step <- step(size_model_ml)


ml_top <- get_model(ml_step)

tab_model(ml_top, digits = 5)

AIC(ml_top)


#collect coef for figure generation
coef_size_size <- coef(summary(ml_top))

write.csv(coef_size_size, "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Figure Generation/Mam_masslength_coef.csv")

#after step wise removal based on AIC and BIC
#FINAL MASS:LENGTH MODEL

#contrast coded complimentary model
final_size_modelml <- lmer(LSize ~ TPI_month_max + AI + HLU +
                              lifestyle + activity_cycle + hibernation_torpor +
                              TPI_month_max:AI + TPI_month_max:HLU +
                              TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                              (1|Binomial),data=M_Size, REML=T,
                            control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
final_size_modelml_c <- lmer(LSize ~ TPI_month_max + AI + HLU +
                                lifestyle + activity_cycle + hibernation_torpor +
                                TPI_month_max:AI + TPI_month_max:HLU +
                                TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                                (1|Binomial),data=M_Size_c, REML=T,
                              control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


check_singularity(final_size_modelsmi)
check_outliers(final_size_modelsmi)

qqnorm(residuals(final_size_modelsmi))
plot(final_size_modelsmi)

check_model(final_size_model)

tab_model(final_size_modelsmi,final_size_modelsmi_c, digits=5, 
          file = "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mammal_SMI_Results.html")
tab_model(final_size_modelml,final_size_modelml_c, digits=5, 
          file = "C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mammal_Size_Results.html")

sink("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Results/Mammal_size_LME.txt")
"Mammal SMI Model LME"
summary(final_size_modelsmi)
"R squared"
print(MuMIn::r.squaredGLMM(final_size_modelsmi))
""
""
"Contrast"
summary(final_size_modelsmi_c)
"R squared"
print(MuMIn::r.squaredGLMM(final_size_modelsmi_c))
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
B_Mass <- vroom("./Bird_Mass.csv")
B_Length <- vroom("./Bird_Length.csv")
B_Size <- vroom("./Bird_Size.csv")


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

####MASS ANALYSIS####

#spatial variogram
sp_mass_mod_b <- lme(LMass ~ 1,
           random = ~1|Binomial, 
           data= B_Mass)
plot(Variogram(sp_mass_mod_b, form=~Lat+Lon|Binomial, maxDist = 5))

#FULL MASS MODEL
mass_model_b <- lmer(LMass ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + Migration +
                     TPI_month_max:AI + TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:Migration +
                     (1|Binomial)+(1|Season),data=B_Mass,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

#after step wise removal based on AIC and BIC
#FINAL MASS MODEL

final_mass_model_b <- lmer(LMass ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + Migration +
                     TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:Migration +
                     (1|Binomial)+(1|Season),data=B_Mass,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


#contrast coded complimentary model
final_mass_model_c_b <- lmer(LMass ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + Migration +
                     TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:Migration +
                     (1|Binomial)+(1|Season),data=B_Mass_c,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

performance::check_singularity(final_mass_model_b)
performance::check_model(final_mass_model_b)

tab_model(final_mass_model_b,final_mass_model_c_b, digits=5, file = "Bird_Mass_Results.html")

sink("Bird_mass_LME.txt")
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
length_model_b <- lmer(LLength ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + Migration +
                     TPI_month_max:AI + TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:Migration +
                     (1|Binomial),data=B_Length,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

#after step wise removal based on AIC and BIC
#FINAL LENGTH MODEL

final_length_model_b <- lmer(LLength ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + Migration +
                     TPI_month_max:AI + TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:Migration +
                     (1|Binomial),data=B_Length,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


#contrast coded complimentary model
final_length_model_c_b <- lmer(LLength ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + Migration +
                     TPI_month_max:AI + TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:Migration +
                     (1|Binomial),data=B_Length_c,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

AIC(length_model_b,final_length_model_b)
BIC(length_model_b,final_length_model_b)
performance::check_singularity(final_length_model_b)
performance::check_model(final_length_model_b)

tab_model(final_length_model_b,final_length_model_c_b, digits=5, file = "Bird_Length_Results.html")

sink("Bird_length_LME.txt")
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
size_model_b <- lmer(LSize ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + Migration +
                     TPI_month_max:AI + TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:Migration +
                     (1|Binomial)+(1|Season),data=B_Size,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

#after step wise removal based on AIC and BIC
#FINAL LENGTH MODEL

final_size_model_b <- lmer(LSize ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle +
                     (1|Binomial)+(1|Season),data=B_Size,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


#contrast coded complimentary model
final_size_model_c_b <- lmer(LSize ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle +
                     (1|Binomial)+(1|Season),data=B_Size_c,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

AIC(size_model_b,final_size_model_b)
BIC(size_model_b,final_size_model_b)
performance::check_singularity(final_size_model_b)
performance::check_model(final_size_model_b)

tab_model(final_size_model_b,final_size_model_c_b, digits=5, file = "Bird_Size_Results.html")

sink("Bird_size_LME.txt")
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
