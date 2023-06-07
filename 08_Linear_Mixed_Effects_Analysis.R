#' ---
#' Step 8: Conduct Linear Mixed Effects Modeling
#' Matthew Watson
#' conducts LME models for analysis of body size trends
#' ---
library(vroom)
library(dplyr)
library(ape)
library(phyr)
library(sjmisc)
library(sjPlot)
library(nlme)
library(lme4)
setwd("") #set to where model output files will be stored
###############################MAMMAL ANALYSIS#############################################
#change file path to location of datafiles
M_Mass <- vroom("./Mammal_Mass.csv")
M_Length <- vroom("./Mammal_Length.csv")
M_Size <- vroom("./Mammal_Size.csv")


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
mass_model <- lmer(LMass ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + hibernation_torpor +
                     TPI_month_max:AI + TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                     (1|Binomial)+(1|Season),data=M_Mass,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

#after step wise removal based on AIC and BIC
#FINAL MASS MODEL

final_mass_model <- lmer(LMass ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + hibernation_torpor +
                     TPI_month_max:AI + TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                     (1|Binomial)+(1|Season),data=M_Mass,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


#contrast coded complimentary model
final_mass_model_c <- lmer(LMass ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + hibernation_torpor +
                     TPI_month_max:AI + TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                     (1|Binomial)+(1|Season),data=M_Mass_c,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

performance::check_singularity(mass_model_final)
performance::check_model(mass_model_final)

tab_model(final_mass_model,final_mass_model_c, digits=5, file = "Mammal_Mass_Results.html")

sink("Mammal_mass_LME.txt")
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
length_model <- lmer(LLength ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + hibernation_torpor +
                     TPI_month_max:AI + TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                     (1|Binomial),data=M_Length,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

#after step wise removal based on AIC and BIC
#FINAL LENGTH MODEL

final_length_model <- lmer(LLength ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + hibernation_torpor +
                     TPI_month_max:HLU +
                     TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                     (1|Binomial),data=M_Length,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


#contrast coded complimentary model
final_length_model_c <- lmer(LLength ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + hibernation_torpor +
                     TPI_month_max:HLU +
                     TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                     (1|Binomial),data=M_Length_c,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

AIC(length_model,final_length_model)
BIC(length_model,final_length_model)
performance::check_singularity(final_length_model)
performance::check_model(final_length_model)

tab_model(final_length_model,final_length_model_c, digits=5, file = "Mammal_Length_Results.html")

sink("Mammal_length_LME.txt")
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
           data= M_Size)
plot(Variogram(sp_size_mod, form=~Lat+Lon|Binomial, maxDist = 5))

#FULL LENGTH MODEL
size_model <- lmer(LSize ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + hibernation_torpor +
                     TPI_month_max:AI + TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                     (1|Binomial)+(1|Season),data=M_Size,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

#after step wise removal based on AIC and BIC
#FINAL LENGTH MODEL

final_size_model <- lmer(LSize ~ Year_sc + TPI_month_max + AI + HLU +
                     activity_cycle + hibernation_torpor +
                     TPI_month_max:activity_cycle +
                     (1|Binomial)+(1|Season),data=M_Size,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


#contrast coded complimentary model
final_size_model_c <- lmer(LSize ~ Year_sc + TPI_month_max + AI + HLU +
                     activity_cycle + hibernation_torpor +
                     TPI_month_max:activity_cycle +
                     (1|Binomial)+(1|Season),data=M_Size_c,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

AIC(size_model,final_size_model)
BIC(size_model,final_size_model)
performance::check_singularity(final_size_model)
performance::check_model(final_size_model)

tab_model(final_size_model,final_size_model_c, digits=5, file = "Mammal_Size_Results.html")

sink("Mammal_size_LME.txt")
"Mammal Size Model LME"
summary(final_size_model)
"R squared"
print(MuMIn::r.squaredGLMM(final_size_model))
""
""
"Contrast"
summary(final_size_model_c)
"R squared"
print(MuMIn::r.squaredGLMM(final_size_model_c))
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
sp_mass_mod <- lme(LMass ~ 1,
           random = ~1|Binomial, 
           data= M_Mass)
plot(Variogram(sp_mass_mod, form=~Lat+Lon|Binomial, maxDist = 5))

#FULL MASS MODEL
mass_model <- lmer(LMass ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + hibernation_torpor +
                     TPI_month_max:AI + TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                     (1|Binomial)+(1|Season),data=M_Mass,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

#after step wise removal based on AIC and BIC
#FINAL MASS MODEL

final_mass_model <- lmer(LMass ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + hibernation_torpor +
                     TPI_month_max:AI + TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                     (1|Binomial)+(1|Season),data=M_Mass,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


#contrast coded complimentary model
final_mass_model_c <- lmer(LMass ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + hibernation_torpor +
                     TPI_month_max:AI + TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                     (1|Binomial)+(1|Season),data=M_Mass_c,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

performance::check_singularity(mass_model_final)
performance::check_model(mass_model_final)

tab_model(final_mass_model,final_mass_model_c, digits=5, file = "Mammal_Mass_Results.html")

sink("Mammal_mass_LME.txt")
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
length_model <- lmer(LLength ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + hibernation_torpor +
                     TPI_month_max:AI + TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                     (1|Binomial),data=M_Length,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

#after step wise removal based on AIC and BIC
#FINAL LENGTH MODEL

final_length_model <- lmer(LLength ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + hibernation_torpor +
                     TPI_month_max:HLU +
                     TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                     (1|Binomial),data=M_Length,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


#contrast coded complimentary model
final_length_model_c <- lmer(LLength ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + hibernation_torpor +
                     TPI_month_max:HLU +
                     TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                     (1|Binomial),data=M_Length_c,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

AIC(length_model,final_length_model)
BIC(length_model,final_length_model)
performance::check_singularity(final_length_model)
performance::check_model(final_length_model)

tab_model(final_length_model,final_length_model_c, digits=5, file = "Mammal_Length_Results.html")

sink("Mammal_length_LME.txt")
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
           data= M_Size)
plot(Variogram(sp_size_mod, form=~Lat+Lon|Binomial, maxDist = 5))

#FULL LENGTH MODEL
size_model <- lmer(LSize ~ Year_sc + TPI_month_max + AI + HLU +
                     lifestyle + activity_cycle + hibernation_torpor +
                     TPI_month_max:AI + TPI_month_max:HLU +
                     TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                     (1|Binomial)+(1|Season),data=M_Size,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

#after step wise removal based on AIC and BIC
#FINAL LENGTH MODEL

final_size_model <- lmer(LSize ~ Year_sc + TPI_month_max + AI + HLU +
                     activity_cycle + hibernation_torpor +
                     TPI_month_max:activity_cycle +
                     (1|Binomial)+(1|Season),data=M_Size,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


#contrast coded complimentary model
final_size_model_c <- lmer(LSize ~ Year_sc + TPI_month_max + AI + HLU +
                     activity_cycle + hibernation_torpor +
                     TPI_month_max:activity_cycle +
                     (1|Binomial)+(1|Season),data=M_Size_c,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

AIC(size_model,final_size_model)
BIC(size_model,final_size_model)
performance::check_singularity(final_size_model)
performance::check_model(final_size_model)

tab_model(final_size_model,final_size_model_c, digits=5, file = "Mammal_Size_Results.html")

sink("Mammal_size_LME.txt")
"Mammal Size Model LME"
summary(final_size_model)
"R squared"
print(MuMIn::r.squaredGLMM(final_size_model))
""
""
"Contrast"
summary(final_size_model_c)
"R squared"
print(MuMIn::r.squaredGLMM(final_size_model_c))
sink()


