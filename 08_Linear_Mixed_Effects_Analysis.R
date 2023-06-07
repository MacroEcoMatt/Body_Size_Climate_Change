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
                     (1|Binomial)+(1|Season),data=M_Mass,
                   control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

