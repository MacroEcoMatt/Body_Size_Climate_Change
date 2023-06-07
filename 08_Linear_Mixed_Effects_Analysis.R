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

#change file path to location of datafiles
M_Mass <- vroom("./Mammal_Mass.csv")
M_Length <- vroom("./Mammal_Length.csv")
Mass_Length <- vroom("./Mammal_Size.csv")
