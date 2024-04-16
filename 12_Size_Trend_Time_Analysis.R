
#Year Analaysis
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
library(MASS)
library(ggeffects)
library(ggiraphExtra)
setwd("") #set to where model output files will be stored
###############################MAMMAL ANALYSIS#############################################
#change file path to location of datafiles
Year_data <- read.csv("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/A_SUBMISSION_UPDATED/Data_S9_Body_Size_Time_Trend_Data.csv")
bird <- Year_data %>% filter(Class=="Aves")
mammal <- Year_data %>% filter(Class=="Mammalia")

bm <-   bird%>%filter(Metric=="Mass")
bl <- bird%>%filter(Metric=="Length")
bs <- bird%>%filter(Metric=="Size")

mm <-   mammal%>%filter(Metric=="Mass")
ml <- mammal%>%filter(Metric=="Length")
ms <- mammal%>%filter(Metric=="Size")


r2ww <- function(x){
  SSe <- sum(x$w*(x$resid)^2)
  observed <- x$resid+x$fitted
  SSt <- sum(x$w*(observed-weighted.mean(observed,x$w))^2)
  value <- 1-SSe/SSt;
  return(value);
}

b_m<- lm(lm_estimate ~ TPI_trend+API_trend+HLU_trend, 
                 data=bm)
plot(b_m)
b_l <- lm(lm_estimate ~ TPI_trend+API_trend+HLU_trend, 
                 data=bl)
plot(b_l)

b_s <- lm(lm_estimate ~ TPI_trend+API_trend+HLU_trend, 
                 data=bs)
plot(b_s)
hist(resid(b_m))
hist(resid(b_l))
hist(resid(b_s))
#bird
b_m_bisqr <- rlm(lm_estimate ~ TPI_trend+API_trend+HLU_trend, 
                 data=bm,psi = psi.bisquare)

b_l_bisqr <- rlm(lm_estimate ~ TPI_trend+API_trend+HLU_trend, 
                 data=bl,psi = psi.bisquare)

b_s_bisqr <- rlm(lm_estimate ~ TPI_trend+API_trend+HLU_trend, 
                 data=bs,psi = psi.bisquare)

tab_model(b_m_bisqr,b_l_bisqr,b_s_bisqr, digits=4)

tab_model(b_m,b_m_bisqr,digits=4)
tab_model(b_l,b_l_bisqr,digits=4)
tab_model(b_s,b_s_bisqr,digits=4)

r2ww(b_m_bisqr)
r2ww(b_l_bisqr)
r2ww(b_s_bisqr)

resid_weights <- data.frame(Binomial = bm$Binomial, resid = b_m_bisqr$resid, weight = b_m_bisqr$w)
resid_weigths <- resid_weights[order(b_m_bisqr$w),]
fitted_val <- fitted(b_m_bisqr)
Binomial <- bm$Binomial
cb <- as.data.frame(cbind(Binomial,fitted_val))
resid_weights <- left_join(resid_weights,cb)
plot((resid_weights$resid*resid_weights$weight),resid_weights$fitted_val)
hist((resid_weights$resid*resid_weights$weight))
hist(scale((resid_weights$resid*resid_weights$weight)))

resid_l <- data.frame(Binomial = bl$Binomial, resid = b_l_bisqr$resid, weight = b_l_bisqr$w)
resid_l <- resid_l[order(b_l_bisqr$w),]
fitted_vall <- fitted(b_l_bisqr)
Binomial <- bl$Binomial
cbl <- as.data.frame(cbind(Binomial,fitted_vall))
resid_l <- left_join(resid_l,cbl)
plot((resid_l$resid*resid_l$weight),resid_l$fitted_val)
hist((resid_l$resid*resid_l$weight))
hist(resid(b_l))
hist(scale((resid_l$resid*resid_l$weight)))

resid_s <- data.frame(Binomial = bs$Binomial, resid = b_s_bisqr$resid, weight = b_s_bisqr$w)
resid_s <- resid_s[order(b_s_bisqr$w),]
fitted_vals <- fitted(b_s_bisqr)
Binomial <- bs$Binomial
cbs <- as.data.frame(cbind(Binomial,fitted_vals))
resid_s <- left_join(resid_s,cbs)
plot((resid_s$resid*resid_s$weight),resid_s$fitted_val)
hist((resid_s$resid*resid_s$weight))
hist(scale((resid_s$resid*resid_s$weight)))
hist(resid(b_s))

#Mammal
mm2 <- lm(lm_estimate ~ TPI_trend+API_trend+HLU_trend, 
                 data=mm)

ml2 <- lm(lm_estimate ~ TPI_trend+API_trend+HLU_trend, 
                 data=ml)

ms2 <- lm(lm_estimate ~ TPI_trend+API_trend+HLU_trend, 
                 data=ms)

m_m_bisqr <- rlm(lm_estimate ~ TPI_trend+API_trend+HLU_trend, 
                 data=mm,psi = psi.bisquare)

m_l_bisqr <- rlm(lm_estimate ~ TPI_trend+API_trend+HLU_trend, 
                 data=ml, psi = psi.bisquare)

m_s_bisqr <- rlm(lm_estimate ~ TPI_trend+API_trend+HLU_trend, 
                 data=ms,psi = psi.bisquare)

tab_model(m_m_bisqr,mm2,digits=4)
tab_model(m_l_bisqr,ml2,digits=4)
tab_model(m_s_bisqr,ms2,digits=4)

r2ww(m_m_bisqr)
r2ww(m_l_bisqr)
r2ww(m_s_bisqr)

resid_weights <- data.frame(Binomial = mm$Binomial, resid = m_m_bisqr$resid, weight = m_m_bisqr$w)
resid_weigths <- resid_weights[order(m_m_bisqr$w),]
fitted_val <- fitted(m_m_bisqr)
Binomial <- mm$Binomial
cb <- as.data.frame(cbind(Binomial,fitted_val))
resid_weights <- left_join(resid_weights,cb)
plot((resid_weights$resid*resid_weights$weight),resid_weights$fitted_val)
hist((resid_weights$resid*resid_weights$weight))
hist(scale((resid_weights$resid*resid_weights$weight)))

resid_l <- data.frame(Binomial = ml$Binomial, resid = m_l_bisqr$resid, weight = m_l_bisqr$w)
resid_l <- resid_l[order(m_l_bisqr$w),]
fitted_vall <- fitted(m_l_bisqr)
Binomial <- ml$Binomial
cbl <- as.data.frame(cbind(Binomial,fitted_vall))
resid_l <- left_join(resid_l,cbl)
plot((resid_l$resid*resid_l$weight),resid_l$fitted_vall)
hist((resid_l$resid*resid_l$weight))
hist(scale((resid_l$resid*resid_l$weight)))

resid_s <- data.frame(Binomial = ms$Binomial, resid = m_s_bisqr$resid, weight = m_s_bisqr$w)
resid_s <- resid_s[order(m_s_bisqr$w),]
fitted_vals <- fitted(m_s_bisqr)
Binomial <- ms$Binomial
cbs <- as.data.frame(cbind(Binomial,fitted_vals))
resid_s <- left_join(resid_s,cbs)
plot((resid_s$resid*resid_s$weight),resid_s$fitted_val)
hist((resid_s$resid*resid_s$weight))
hist(resid(ms2))

###plots

#bird
#mass
options(scipen = 999)
#bird
#mass
bmt <- ggpredict(b_m_bisqr, terms=c("TPI_trend"))
bmt_f <- plot(bmt, colors=c("cornflowerblue"), line.size = 1.3)+
  labs(x = "TPI Trend",
       y = "Body Mass Trend",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5)
  )
plot(bmt_f)

bma <- ggpredict(b_m_bisqr, terms=c("API_trend  [-0.1:0.1 by=0.01]"))
bma_f <- plot(bma, colors=c("cornflowerblue"), line.size = 1.3)+
  labs(x = "API Trend",
       y = "Body Mass Trend",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5)
  )
plot(bma_f)

bmh <- ggpredict(b_m_bisqr, terms=c("HLU_trend[-0.02:0.02 by=0.001]"))
bmh_f <- plot(bmh, colors=c("cornflowerblue"), line.size = 1.3)+
  labs(x = "HLU Trend",
       y = "Body Mass Trend",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5)
  )
plot(bmh_f)

#length

blt <- ggpredict(b_l_bisqr, terms=c("TPI_trend  [-0.1:0.1 by=0.01]"))
blt_f <- plot(blt, colors=c("cornflowerblue"), line.size = 1.3)+
  labs(x = "TPI Trend",
       y = "Body Length Trend",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5)
  )
plot(blt_f)

bla <- ggpredict(b_l_bisqr, terms=c("API_trend [-0.1:0.1 by=0.01]"))
bla_f <- plot(bla, colors=c("cornflowerblue"), line.size = 1.3)+
  labs(x = "API Trend",
       y = "Body Length Trend",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5)
  )
plot(bla_f)

blh <- ggpredict(b_l_bisqr, terms=c("HLU_trend [-0.1:0.1 by=0.01]"))
blh_f <- plot(blh, colors=c("cornflowerblue"), line.size = 1.3)+
  labs(x = "HLU Trend",
       y = "Body Length Trend",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5)
  )
plot(blh_f)

#size
bst <- ggpredict(b_s_bisqr, terms=c("TPI_trend [-0.1:0.1 by=0.01]"))
bst_f <- plot(bst, colors=c("cornflowerblue"), line.size = 1.3)+
  labs(x = "TPI Trend",
       y = "Mass:Length Trend",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5)
  )
plot(bst_f)

bsa <- ggpredict(b_s_bisqr, terms=c("API_trend [-0.1:0.1 by=0.01]"))
bsa_f <- plot(bsa, colors=c("cornflowerblue"), line.size = 1.3)+
  labs(x = "API Trend",
       y = "Mass:Length Trend",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5)
  )
plot(bsa_f)

bsh <- ggpredict(b_s_bisqr, terms=c("HLU_trend [-0.1:0.1 by=0.01]"))
bsh_f <- plot(bsh, colors=c("cornflowerblue"), line.size = 1.3)+
  labs(x = "HLU Trend",
       y = "Mass:Length Trend",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5)
  )
plot(bsh_f)

#Mammals


#mass
mmt <- ggpredict(m_m_bisqr, terms=c("TPI_trend [-0.04:0.04 by=0.01]"))
mmt_f <- plot(mmt, colors=c("chocolate4"), line.size = 1.3)+
  labs(x = "TPI Trend",
       y = "",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5)
  )
plot(mmt_f)

mma <- ggpredict(m_m_bisqr, terms=c("API_trend [-0.1:0.1 by=0.01]"))
mma_f <- plot(mma, colors=c("chocolate4"), line.size = 1.3)+
  labs(x = "API Trend)",
       y = "",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5)
  )
plot(mma_f)

mmh <- ggpredict(m_m_bisqr, terms=c("HLU_trend [-0.1:0.1 by=0.01]"))
mmh_f <- plot(mmh, colors=c("chocolate4"), line.size = 1.3)+
  labs(x = "HLU Trend",
       y = "",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5)
  )
plot(mmh_f)

#length

mlt <- ggpredict(m_l_bisqr, terms=c("TPI_trend [-0.1:0.1 by=0.01]"))
mlt_f <- plot(mlt, colors=c("chocolate4"), line.size = 1.3)+
  labs(x = "TPI Trend",
       y = "",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5)
  )
plot(mlt_f)

mla <- ggpredict(m_l_bisqr, terms=c("API_trend [-0.1:0.1 by=0.01]"))
mla_f <- plot(mla, colors=c("chocolate4"), line.size = 1.3)+
  labs(x = "API Trend",
       y = "",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5)
  )
plot(mla_f)

mlh <- ggpredict(m_l_bisqr, terms=c("HLU_trend"))
mlh_f <- plot(mlh, colors=c("chocolate4"), line.size = 1.3)+
  labs(x = "HLU Trend",
       y = "",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5)
  )
plot(mlh_f)

#size
mst <- ggpredict(m_s_bisqr, terms=c("TPI_trend [-0.1:0.1 by=0.01]"))
mst_f <- plot(mst, colors=c("chocolate4"), line.size = 1.3)+
  labs(x = "TPI Trend",
       y = "",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5)
  )
plot(mst_f)

msa <- ggpredict(m_s_bisqr, terms=c("API_trend [-0.1:0.1 by=0.01]"))
msa_f <- plot(msa, colors=c("chocolate4"), line.size = 1.3)+
  labs(x = "API Trend",
       y = "",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5)
  )
plot(msa_f)

msh <- ggpredict(m_s_bisqr, terms=c("HLU_trend [-0.1:0.1 by=0.001"))
msh_f <- plot(msh, colors=c("chocolate4"), line.size = 1.3)+
  labs(x = "HLU Trend",
       y = "",
       title = "",
       color="")+
  theme(axis.title = element_text(face="bold",size=14, color="black"),
        axis.text = element_text(size=12, color="black"),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", size=16, color = "black",hjust = 0.5)
  )
plot(msh_f)

tpi <- ggpubr::ggarrange(bmt_f, blt_f, bst_f,mmt_f, mlt_f, mst_f,
                         ncol = 3,
                         nrow = 2,
                         labels = "AUTO")
plot(tpi)

ai <- ggpubr::ggarrange(bma_f, bla_f, bsa_f,mma_f, mla_f, msa_f,
                        ncol = 3,
                        nrow = 2,
                        labels = "AUTO")
plot(ai)

hlu <- ggpubr::ggarrange(bmh_f, blh_f, bsh_f,mmh_f, mlh_f, msh_f,
                         ncol = 3,
                         nrow = 2,
                         labels = "AUTO")
plot(hlu)

##figures plots
plot(bmt_f)
plot(bma_f)
plot(bmh_f)

plot(mmt_f)
plot(mma_f)
plot(mmh_f)

plot(blt_f)
plot(bla_f)
plot(blh_f)

plot(mlt_f)
plot(mla_f)
plot(mlh_f)

#####OUTPUT

tab_model(m_m_bisqr, m_l_bisqr, m_s_bisqr, digits=5,
          file="ALL_YEAR_Results_Mammal.html")
tab_model(b_m_bisqr, b_l_bisqr, b_s_bisqr, digits=5,
          file="ALL_YEAR_Results_Bird.html")
