#' ---
#' step 12: Conduct Robust Linear Regression on Change in Body size metrics against change in environmental variables.
#' Matthew Watson
#' 
#' ---

library(vroom)
library(dplyr)
library(sjmisc)
library(sjPlot)
library(nlme)
library(lme4)
library(lmerTest)
library(ggthemes)
library(ggplot2)
library(performance)

setwd("") #set to where model output files will be stored
###############################MAMMAL ANALYSIS#############################################
#change file path to location of datafiles
Year_data <- read.csv("ALL_YEAR_DATA.csv"

#MASS ANALYSIS
mass_year <- Year_data%>%filter(Metric=="Mass")

lm_normal <- lm(lm_estimate ~ TPI_trend+AI_trend+HLU_trend+Class, 
                data=mass_year)
lm_huber <- rlm(lm_estimate ~ TPI_trend+AI_trend+HLU_trend+Class, 
                data=mass_year)
lm_bisqr <- rlm(lm_estimate ~ TPI_trend+AI_trend+HLU_trend+Class, 
                data=mass_year,psi = psi.bisquare)

####MASSMODEL
all_mass_lm <- rlm(lm_estimate ~ TPI_trend + AI_trend + HLU_trend+ Class +
                   TPI_trend:Class + AI_trend:Class + HLU_trend:Class, 
                   data=mass_year,psi = psi.bisquare)
step(all_mass_lm)
all_mass_lm <- rlm(lm_estimate ~ TPI_trend + Class
                   +TPI_trend:Class, 
                   data=mass_year,psi = psi.bisquare)

tab_model(all_mass_lm, digits=5)

resid_weights <- data.frame(Binomial = mass_year$Binomial, resid = all_mass_lm$resid, weight = all_mass_lm$w)
resid_weigths <- resid_weights[order(all_mass_lm$w),]
fitted_val <- fitted(all_mass_lm)
Binomial <- mass_year$Binomial
cb <- as.data.frame(cbind(Binomial,fitted_val))
resid_weights <- left_join(resid_weights,cb)
plot((resid_weights$resid*resid_weights$weight),resid_weights$fitted_val)
hist((resid_weights$resid*resid_weights$weight))
hist(scale((resid_weights$resid*resid_weights$weight)))
#LENGTH ANALYSIS
length_year <- Year_data%>%filter(Metric=="Length")

l_normal <- lm(lm_estimate ~ TPI_trend+AI_trend+HLU_trend+Class
               +TPI_trend:Class+AI_trend:Class+HLU_trend:Class, 
               data=length_year)
l_huber <- rlm(lm_estimate ~ TPI_trend+AI_trend+HLU_trend+Class
               +TPI_trend:Class+AI_trend:Class+HLU_trend:Class, 
               data=length_year)
l_bisqr <- rlm(lm_estimate ~ TPI_trend+AI_trend+HLU_trend+Class
               +TPI_trend:Class+AI_trend:Class+HLU_trend:Class, 
               data=length_year,psi = psi.bisquare)
tab_model(l_normal,l_huber,l_bisqr,digits=5)

all_length_lm <- rlm(lm_estimate ~ TPI_trend+AI_trend+HLU_trend+Class
                     +TPI_trend:Class+AI_trend:Class+HLU_trend:Class, 
                     data=length_year,psi = psi.bisquare)
step(all_length_lm)
#take best model and create below
all_length_lm <- rlm(lm_estimate ~ TPI_trend+AI_trend+HLU_trend+Class
                     +TPI_trend:Class+AI_trend:Class+HLU_trend:Class, 
                     data=length_year,psi = psi.bisquare)

tab_model(all_length_lm, digits=5)
#Check model assumptions
resid_l <- data.frame(Binomial = length_year$Binomial, resid = all_length_lm$resid, weight = all_length_lm$w)
resid_l <- resid_l[order(all_length_lm$w),]
fitted_vall <- fitted(all_length_lm)
Binomial <- length_year$Binomial
cbl <- as.data.frame(cbind(Binomial,fitted_vall))
resid_l <- left_join(resid_l,cbl)
plot((resid_l$resid*resid_l$weight),resid_l$fitted_val)
hist((resid_l$resid*resid_l$weight))
hist(scale((resid_l$resid*resid_l$weight)))

###SIZE ANALYSIS
size_year <- Year_data%>%filter(Metric=="Size")

s_normal <- lm(lm_estimate ~ TPI_trend+AI_trend+HLU_trend+Class
               +TPI_trend:Class+AI_trend:Class+HLU_trend:Class, 
               data=size_year)
s_huber <- rlm(lm_estimate ~ TPI_trend+AI_trend+HLU_trend+Class
               +TPI_trend:Class+AI_trend:Class+HLU_trend:Class, 
               data=size_year)
s_bisqr <- rlm(lm_estimate ~ TPI_trend+AI_trend+HLU_trend+Class
               +TPI_trend:Class+AI_trend:Class+HLU_trend:Class, 
               data=size_year,psi = psi.bisquare)
tab_model(s_normal,s_huber,s_bisqr,digits=5)

all_size_lm <- rlm(lm_estimate ~ TPI_trend+AI_trend+HLU_trend+Class
                   +TPI_trend:Class+AI_trend:Class+HLU_trend:Class, 
                   data=size_year,psi = psi.bisquare)
step(all_size_lm)
tab_model(all_size_lm, digits=5)

all_size_lm <- rlm(lm_estimate ~ TPI_trend+AI_trend+HLU_trend+Class
                   +TPI_trend:Class+AI_trend:Class+HLU_trend:Class, 
                   data=size_year,psi = psi.bisquare)

resid_s <- data.frame(Binomial = size_year$Binomial, resid = all_size_lm$resid, weight = all_size_lm$w)
resid_s <- resid_s[order(all_size_lm$w),]
fitted_vals <- fitted(all_size_lm)
Binomial <- size_year$Binomial
cbs <- as.data.frame(cbind(Binomial,fitted_vals))
resid_s <- left_join(resid_s,cbs)
plot((resid_s$resid*resid_s$weight),resid_s$fitted_val)
hist((resid_s$resid*resid_s$weight))
hist(scale((resid_s$resid*resid_s$weight)))

#####OUTPUT
tab_model(all_mass_lm, all_length_lm, all_size_lm, digits=5,
          file="ALL_YEAR_Results.html")
