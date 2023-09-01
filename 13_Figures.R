library(ggplot2)
library(ggthemes)
library(ggeffects)
library(dplyr)
setwd("C:/Users/matth/OneDrive/Documents/PhD/Thesis/Body Size Chapter/Figure Generation/")
Env_data <- read.csv("ALL_ENV_FIG_Data.csv")[,-c(11,12)]
#MASS FIGURES Data####
#x and y are values for generating confint boxes
#geome segement creates regression lines
mam_intercept_mass <- Env_data[75,5]
mam_int_ci <- 1.9599*Env_data[75,6]
mam_tpi_slope_mass<- Env_data[76,5]
mam_tpi_ci <- 1.9599*Env_data[76,6]
mam_ai_slope_mass<- Env_data[77,5]
mam_ai_ci <- 1.9599*Env_data[77,6]
mam_hlu_slope_mass <- Env_data[78,5]
mam_hlu_ci <- 1.9599*Env_data[78,6]

bird_intercept_mass<- Env_data[1,5]
bird_int_ci <- 1.9599*Env_data[1,6]
bird_tpi_slope_mass<- Env_data[2,5]
bird_tpi_ci <- 1.9599*Env_data[2,6]
bird_ai_slope_mass<- Env_data[3,5]
bird_ai_ci <- 1.9599*Env_data[3,6]
bird_hlu_slope_mass <- Env_data[4,5]
bird_hlu_ci <- 1.9599*Env_data[4,6]

mass_fig=data.frame(x=c(0,0,1,1, 0,0,100,100, 0,0,1,1, 0,0,1,1, 0,0,100,100, 0,0,1,1), 
                             y=c(mam_intercept_mass+mam_int_ci,mam_intercept_mass-mam_int_ci,(mam_intercept_mass-mam_int_ci)+(mam_tpi_slope_mass-mam_tpi_ci),(mam_intercept_mass+mam_int_ci)+(mam_tpi_slope_mass+mam_tpi_ci),
                                 mam_intercept_mass+mam_int_ci,mam_intercept_mass-mam_int_ci,(mam_intercept_mass-mam_int_ci)+(100*(mam_ai_slope_mass-mam_ai_ci)),(mam_intercept_mass+mam_int_ci)+(100*(mam_ai_slope_mass+mam_ai_ci)),
                                 mam_intercept_mass+mam_int_ci,mam_intercept_mass-mam_int_ci,(mam_intercept_mass-mam_int_ci)+(mam_hlu_slope_mass-mam_hlu_ci),(mam_intercept_mass+mam_int_ci)+(mam_hlu_slope_mass+mam_hlu_ci),
                                 bird_intercept_mass+bird_int_ci,bird_intercept_mass-bird_int_ci,(bird_intercept_mass-bird_int_ci)+(bird_tpi_slope_mass-bird_tpi_ci),(bird_intercept_mass+bird_int_ci)+(bird_tpi_slope_mass+bird_tpi_ci),
                                 bird_intercept_mass+bird_int_ci,bird_intercept_mass-bird_int_ci,(bird_intercept_mass-bird_int_ci)+(100*(bird_ai_slope_mass-bird_ai_ci)),(bird_intercept_mass+bird_int_ci)+(100*(bird_ai_slope_mass+bird_ai_ci)),
                                 bird_intercept_mass+bird_int_ci,bird_intercept_mass-bird_int_ci,(bird_intercept_mass-bird_int_ci)+(bird_hlu_slope_mass-bird_hlu_ci),(bird_intercept_mass+bird_int_ci)+(bird_hlu_slope_mass+bird_hlu_ci)
                                 ),
                             var=c(rep("TPI",4),rep("AI",4),rep("HLU",4),rep("TPI",4),rep("AI",4),rep("HLU",4)),
                             sp = c(rep("Mammal",12),rep("Bird",12)),
                             intercept_mass = c(rep(mam_intercept_mass,12),rep(bird_intercept_mass,12)),
                             slope = c(rep(mam_tpi_slope_mass,4),rep((mam_ai_slope_mass*100),4),rep(mam_hlu_slope_mass,4),rep(bird_tpi_slope_mass,4),rep((100*bird_ai_slope_mass),4),rep(bird_hlu_slope_mass,4))) Da

#Length FIGURES Data####
#x and y are values for generating confint boxes
#geome segement creates regression lines
mam_intercept_length <- Env_data[97,5]
mam_int_ci <- 1.9599*Env_data[97,6]

mam_tpi_slope_length<- Env_data[98,5]
mam_tpi_ci <- 1.9599*Env_data[98,6]

mam_ai_slope_length<- Env_data[99,5]
mam_ai_ci <- 1.9599*Env_data[99,6]

mam_hlu_slope_length <- Env_data[100,5]
mam_hlu_ci <- 1.9599*Env_data[100,6]

bird_intercept_length<- Env_data[27,5]
bird_int_ci <- 1.9599*Env_data[27,6]

bird_tpi_slope_length<- Env_data[28,5]
bird_tpi_ci <- 1.9599*Env_data[28,6]

bird_ai_slope_length<- Env_data[29,5]
bird_ai_ci <- 1.9599*Env_data[29,6]

bird_hlu_slope_length <- Env_data[30,5]
bird_hlu_ci <- 1.9599*Env_data[30,6]

length_fig=data.frame(x=c(0,0,1,1, 0,0,100,100, 0,0,1,1, 0,0,1,1, 0,0,100,100, 0,0,1,1), 
                    y=c(mam_intercept_length+mam_int_ci,mam_intercept_length-mam_int_ci,(mam_intercept_length-mam_int_ci)+(mam_tpi_slope_length-mam_tpi_ci),(mam_intercept_length+mam_int_ci)+(mam_tpi_slope_length+mam_tpi_ci),
                        mam_intercept_length+mam_int_ci,mam_intercept_length-mam_int_ci,(mam_intercept_length-mam_int_ci)+(100*(mam_ai_slope_length-mam_ai_ci)),(mam_intercept_length+mam_int_ci)+(100*(mam_ai_slope_length+mam_ai_ci)),
                        mam_intercept_length+mam_int_ci,mam_intercept_length-mam_int_ci,(mam_intercept_length-mam_int_ci)+(mam_hlu_slope_length-mam_hlu_ci),(mam_intercept_length+mam_int_ci)+(mam_hlu_slope_length+mam_hlu_ci),
                        bird_intercept_length+bird_int_ci,bird_intercept_length-bird_int_ci,(bird_intercept_length-bird_int_ci)+(bird_tpi_slope_length-bird_tpi_ci),(bird_intercept_length+bird_int_ci)+(bird_tpi_slope_length+bird_tpi_ci),
                        bird_intercept_length+bird_int_ci,bird_intercept_length-bird_int_ci,(bird_intercept_length-bird_int_ci)+(100*(bird_ai_slope_length-bird_ai_ci)),(bird_intercept_length+bird_int_ci)+(100*(bird_ai_slope_length+bird_ai_ci)),
                        bird_intercept_length+bird_int_ci,bird_intercept_length-bird_int_ci,(bird_intercept_length-bird_int_ci)+(bird_hlu_slope_length-bird_hlu_ci),(bird_intercept_length+bird_int_ci)+(bird_hlu_slope_length+bird_hlu_ci)
                    ),
                    var=c(rep("TPI",4),rep("AI",4),rep("HLU",4),rep("TPI",4),rep("AI",4),rep("HLU",4)),
                    sp = c(rep("Mammal",12),rep("Bird",12)),
                    intercept_length = c(rep(mam_intercept_length,12),rep(bird_intercept_length,12)),
                    slope = c(rep(mam_tpi_slope_length,4),rep((mam_ai_slope_length*100),4),rep(mam_hlu_slope_length,4),rep(bird_tpi_slope_length,4),rep((100*bird_ai_slope_length),4),rep(bird_hlu_slope_length,4)))


#size FIGURES Data####
#x and y are values for generating confint boxes
#geome segement creates regression lines
mam_intercept_size <- Env_data[119,5]
mam_int_ci <- 1.9599*Env_data[119,6]
mam_tpi_slope_size<- Env_data[120,5]
mam_tpi_ci <- 1.9599*Env_data[120,6]
mam_ai_slope_size<- Env_data[121,5]
mam_ai_ci <- 1.9599*Env_data[121,6]
mam_hlu_slope_size <- Env_data[122,5]
mam_hlu_ci <- 1.9599*Env_data[122,6]
bird_intercept_size<- Env_data[51,5]
bird_int_ci <- 1.9599*Env_data[51,6]
bird_tpi_slope_size<- Env_data[52,5]
bird_tpi_ci <- 1.9599*Env_data[52,6]
bird_ai_slope_size<- Env_data[53,5]
bird_ai_ci <- 1.9599*Env_data[53,6]
bird_hlu_slope_size <- Env_data[54,5]
bird_hlu_ci <- 1.9599*Env_data[54,6]

size_fig=data.frame(x=c(0,0,1,1, 0,0,100,100, 0,0,1,1, 0,0,1,1, 0,0,100,100, 0,0,1,1), 
                      y=c(mam_intercept_size+mam_int_ci,mam_intercept_size-mam_int_ci,(mam_intercept_size-mam_int_ci)+(mam_tpi_slope_size-mam_tpi_ci),(mam_intercept_size+mam_int_ci)+(mam_tpi_slope_size+mam_tpi_ci),
                          mam_intercept_size+mam_int_ci,mam_intercept_size-mam_int_ci,(mam_intercept_size-mam_int_ci)+(100*(mam_ai_slope_size-mam_ai_ci)),(mam_intercept_size+mam_int_ci)+(100*(mam_ai_slope_size+mam_ai_ci)),
                          mam_intercept_size+mam_int_ci,mam_intercept_size-mam_int_ci,(mam_intercept_size-mam_int_ci)+(mam_hlu_slope_size-mam_hlu_ci),(mam_intercept_size+mam_int_ci)+(mam_hlu_slope_size+mam_hlu_ci),
                          bird_intercept_size+bird_int_ci,bird_intercept_size-bird_int_ci,(bird_intercept_size-bird_int_ci)+(bird_tpi_slope_size-bird_tpi_ci),(bird_intercept_size+bird_int_ci)+(bird_tpi_slope_size+bird_tpi_ci),
                          bird_intercept_size+bird_int_ci,bird_intercept_size-bird_int_ci,(bird_intercept_size-bird_int_ci)+(100*(bird_ai_slope_size-bird_ai_ci)),(bird_intercept_size+bird_int_ci)+(100*(bird_ai_slope_size+bird_ai_ci)),
                          bird_intercept_size+bird_int_ci,bird_intercept_size-bird_int_ci,(bird_intercept_size-bird_int_ci)+(bird_hlu_slope_size-bird_hlu_ci),(bird_intercept_size+bird_int_ci)+(bird_hlu_slope_size+bird_hlu_ci)
                      ),
                      var=c(rep("TPI",4),rep("AI",4),rep("HLU",4),rep("TPI",4),rep("AI",4),rep("HLU",4)),
                      sp = c(rep("Mammal",12),rep("Bird",12)),
                      intercept_size = c(rep(mam_intercept_size,12),rep(bird_intercept_size,12)),
                      slope = c(rep(mam_tpi_slope_size,4),rep((mam_ai_slope_size*100),4),rep(mam_hlu_slope_size,4),rep(bird_tpi_slope_size,4),rep((100*bird_ai_slope_size),4),rep(bird_hlu_slope_size,4)))


##Bird Response and Mammal Response to environment####
####BIRD####
birmassfigdata <- mass_fig %>%mutate(x = ifelse(x==100, 1,x))%>%filter(sp=="Bird")
birdmass_plot <- ggplot(birmassfigdata, aes(x=x, y=y))

birdmass_plot_final <- birdmass_plot + geom_blank() + 
  geom_segment(linetype= "solid",aes(x = 0, xend = 1, y = intercept_mass[5], yend = intercept_mass[5] + slope[5]),color="steelblue1",linewidth=1.2)+
  geom_segment(linetype= "solid", aes(x = 0, xend = 1, y = intercept_mass[9], yend = intercept_mass[9] + slope[9]),color="forestgreen",linewidth=1.2)+
  geom_segment(linetype= "solid", aes(x = 0, xend = 1, y = intercept_mass[1], yend = intercept_mass[1] + slope[1]),color="darkorange2",linewidth=1.2)+
  scale_fill_manual(values = c("steelblue1", "forestgreen", "darkorange2"))+
  geom_polygon(aes(x=x,y=y, group=var, fill=var),alpha=0.2)+
  xlim(0,1)+
  ylab("Log Body Mass (g)")+
  xlab("Scaled Variable Range")+
  theme_fivethirtyeight()+
  labs(fill="")+
  theme(axis.title.y = element_text(vjust = 3,size = 14, face="bold", color="black"),
        axis.title.x = element_text(vjust = -2,size = 14, face="bold", color="black"),
        axis.text.x = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        axis.text.y = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        legend.position = c(0.77,.995),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill ="white",color="white")
  )

birdmass_plot_final

birdlengthfigdata <- length_fig %>%mutate(x = ifelse(x==100, 1,x))%>%filter(sp=="Bird")
birdlength_plot <- ggplot(birdlengthfigdata, aes(x=x, y=y))

birdlength_plot_final <- birdlength_plot + geom_blank() + 
  geom_segment(linetype="solid",aes(x = 0, xend = 1, y = intercept_length[5], yend = intercept_length[5] + slope[5]),color="steelblue1",linewidth=1.2)+
  geom_segment(linetype="solid", aes(x = 0, xend = 1, y = intercept_length[9], yend = intercept_length[9] + slope[9]),color="forestgreen",linewidth=1.2)+
  geom_segment(linetype= "solid", aes(x = 0, xend = 1, y = intercept_length[1], yend = intercept_length[1] + slope[1]),color="darkorange2",linewidth=1.2)+
  scale_fill_manual(values = c("steelblue1", "forestgreen", "darkorange2"))+
  geom_polygon(aes(x=x,y=y, group=var, fill=var),alpha=0.2)+
  xlim(0,1)+
  ylab("Log Body Length (mm)")+
  xlab("Scaled Variable Range")+
  theme_fivethirtyeight()+
  labs(fill="")+
  theme(axis.title.y = element_text(vjust = 3,size = 14, face="bold", color="black"),
        axis.title.x = element_text(vjust = -2,size = 14, face="bold", color="black"),
        axis.text.x = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        axis.text.y = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        legend.position = c(0.77,.995),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill ="white",color="white")
  )

birdlength_plot_final

birdsizefigdata <- size_fig %>%mutate(x = ifelse(x==100, 1,x))%>%filter(sp=="Bird")
birdsize_plot <- ggplot(birdsizefigdata, aes(x=x, y=y))

birdsize_plott_final <- birdsize_plot + geom_blank() + 
  geom_segment(linetype= "solid",aes(x = 0, xend = 1, y = intercept_size[5], yend = intercept_size[5] + slope[5]),color="steelblue1",linewidth=1.2)+
  geom_segment(linetype="solid", aes(x = 0, xend = 1, y = intercept_size[9], yend = intercept_size[9] + slope[9]),color="forestgreen",linewidth=1.2)+
  geom_segment(linetype= "solid", aes(x = 0, xend = 1, y = intercept_size[1], yend = intercept_size[1] + slope[1]),color="darkorange2",linewidth=1.2)+
  scale_fill_manual(values = c("steelblue1", "forestgreen", "darkorange2"))+
  geom_polygon(aes(x=x,y=y, group=var, fill=var),alpha=0.2)+
  xlim(0,1)+
  ylab("Log Mass:Length (g/mm)")+
  xlab("Scaled Variable Range")+
  theme_fivethirtyeight()+
  labs(fill="")+
  theme(axis.title.y = element_text(vjust = 3,size = 14, face="bold", color="black"),
        axis.title.x = element_text(vjust = -2,size = 14, face="bold", color="black"),
        axis.text.x = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        axis.text.y = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        legend.position = c(0.77,.995),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill ="white",color="white")
  )
birdsize_plott_final

figure <- ggpubr::ggarrange(birdmass_plot_final, birdlength_plot_final, birdsize_plott_final,
                            labels = c("A", "B", "C"),
                            ncol = 3,common.legend = TRUE,legend="bottom",label.x = 0)
figure

####MAMMAL####
mammassfigdata <- mass_fig %>%mutate(x = ifelse(x==100, 1,x))%>%filter(sp=="Mammal")
mammass_plot <- ggplot(mammassfigdata, aes(x=x, y=y))

mammass_plot_final <- mammass_plot + geom_blank() + 
  geom_segment(linetype= "solid",aes(x = 0, xend = 1, y = intercept_mass[5], yend = intercept_mass[5] + slope[5]),color="steelblue1",linewidth=1.2)+
  geom_segment(linetype= "solid", aes(x = 0, xend = 1, y = intercept_mass[9], yend = intercept_mass[9] + slope[9]),color="forestgreen",linewidth=1.2)+
  geom_segment(linetype= "solid", aes(x = 0, xend = 1, y = intercept_mass[1], yend = intercept_mass[1] + slope[1]),color="darkorange2",linewidth=1.2)+
  scale_fill_manual(values = c("steelblue1", "forestgreen", "darkorange2"))+
  geom_polygon(aes(x=x,y=y, group=var, fill=var),alpha=0.2)+
  xlim(0,1)+
  ylab("Log Body Mass (g)")+
  xlab("Scaled Variable Range")+
  theme_fivethirtyeight()+
  labs(fill="")+
  theme(axis.title.y = element_text(vjust = 3,size = 14, face="bold", color="black"),
        axis.title.x = element_text(vjust = -2,size = 14, face="bold", color="black"),
        axis.text.x = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        axis.text.y = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        legend.position = c(0.77,.995),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill ="white",color="white")
  )

mammass_plot_final

mamlengthfigdata <- length_fig %>%mutate(x = ifelse(x==100, 1,x))%>%filter(sp=="Mammal")
mamlength_plot <- ggplot(mamlengthfigdata, aes(x=x, y=y))

mamlength_plot_final <- mamlength_plot + geom_blank() + 
  geom_segment(linetype="solid",aes(x = 0, xend = 1, y = intercept_length[5], yend = intercept_length[5] + slope[5]),color="steelblue1",linewidth=1.2)+
  geom_segment(linetype="solid", aes(x = 0, xend = 1, y = intercept_length[9], yend = intercept_length[9] + slope[9]),color="forestgreen",linewidth=1.2)+
  geom_segment(linetype= "solid", aes(x = 0, xend = 1, y = intercept_length[1], yend = intercept_length[1] + slope[1]),color="darkorange2",linewidth=1.2)+
  scale_fill_manual(values = c("steelblue1", "forestgreen", "darkorange2"))+
  geom_polygon(aes(x=x,y=y, group=var, fill=var),alpha=0.2)+
  xlim(0,1)+
  ylab("Log Body Length (mm)")+
  xlab("Scaled Variable Range")+
  theme_fivethirtyeight()+
  labs(fill="")+
  theme(axis.title.y = element_text(vjust = 3,size = 14, face="bold", color="black"),
        axis.title.x = element_text(vjust = -2,size = 14, face="bold", color="black"),
        axis.text.x = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        axis.text.y = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        legend.position = c(0.77,.995),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill ="white",color="white")
  )

mamlength_plot_final

mamsizefigdata <- size_fig %>%mutate(x = ifelse(x==100, 1,x))%>%filter(sp=="Mammal")
mamsize_plot <- ggplot(mamsizefigdata, aes(x=x, y=y))

mamsize_plot_final <- mamsize_plot + geom_blank() + 
  geom_segment(linetype= "solid",aes(x = 0, xend = 1, y = intercept_size[5], yend = intercept_size[5] + slope[5]),color="steelblue1",linewidth=1.2)+
  geom_segment(linetype="solid", aes(x = 0, xend = 1, y = intercept_size[9], yend = intercept_size[9] + slope[9]),color="forestgreen",linewidth=1.2)+
  geom_segment(linetype= "solid", aes(x = 0, xend = 1, y = intercept_size[1], yend = intercept_size[1] + slope[1]),color="darkorange2",linewidth=1.2)+
  scale_fill_manual(values = c("steelblue1", "forestgreen", "darkorange2"))+
  geom_polygon(aes(x=x,y=y, group=var, fill=var),alpha=0.2)+
  xlim(0,1)+
  ylab("Log Mass:Length (g/mm)")+
  xlab("Scaled Variable Range")+
  theme_fivethirtyeight()+
  labs(fill="")+
  theme(axis.title.y = element_text(vjust = 3,size = 14, face="bold", color="black"),
        axis.title.x = element_text(vjust = -2,size = 14, face="bold", color="black"),
        axis.text.x = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        axis.text.y = element_text(color=c(c("black","black","black")[c(1,2,2,2,3)]), size=12),
        legend.position = c(0.77,.995),
        legend.text = element_text(color="black",size=12, face="bold"),
        legend.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = 'white', colour = 'white'),
        panel.background = element_rect(fill ="white",color="white")
  )

mamsize_plot_final

figure2 <- ggpubr::ggarrange(mammass_plot_final, mamlength_plot_final, mamsize_plot_final,
                            labels = c("A", "B", "C"),
                            ncol = 3,common.legend = TRUE,legend="bottom",label.x = 0)

figure2
