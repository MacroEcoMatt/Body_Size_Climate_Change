library(vroom)
library(dplyr)

Mass <- vroom("", delim = ",")#set "" to where files are stored from previous step
Length <- vroom("", delim = ",")
Size <- vroom("", delim = ",")

###Create Bird Data Files
Bird_Mass <- Mass %>% filter(Class=="Aves")
Bird_Length <- Length %>% filter(Class=="Aves")
Bird_Size <- Size %>% filter(Class=="Aves")

bt <- vroom("E:/Coding Files/Chapter 2 Validation/Extra files for process/aves_traits.csv")
rl <- vroom("E:/Coding Files/Chapter 2 Validation/Extra files for process/iucnclass_birds.csv")

BM_trait <- Bird_Mass %>%left_join(bt)%>%left_join(rl)%>%
  filter(!is.na(TPI_month_max))%>%filter(!is.na(AI))%>%filter(!is.na(HLU))%>%
  filter(!is.na(lifestyle))%>%filter(!is.na(Migration))%>%
  mutate(activity_cycle = ifelse(is.na(Order), "Diurnal",
                                 ifelse(Order=="Strigiformes", "Nocturnal",
                                        ifelse(Order=="Caprimulgiformes", "Nocturnal", "Diurnal"))))

BL_trait <- Bird_Length %>%left_join(bt)%>%left_join(rl)%>%
  filter(!is.na(TPI_month_max))%>%filter(!is.na(AI))%>%filter(!is.na(HLU))%>%
  filter(!is.na(lifestyle))%>%filter(!is.na(Migration))%>%
  mutate(activity_cycle = ifelse(is.na(Order), "Diurnal",
                                 ifelse(Order=="Strigiformes", "Nocturnal",
                                        ifelse(Order=="Caprimulgiformes", "Nocturnal", "Diurnal"))))

BS_trait <- Bird_Size %>%left_join(bt)%>%left_join(rl)%>%
  filter(!is.na(TPI_month_max))%>%filter(!is.na(AI))%>%filter(!is.na(HLU))%>%
  filter(!is.na(lifestyle))%>%filter(!is.na(Migration))%>%
  mutate(activity_cycle = ifelse(is.na(Order), "Diurnal",
                                 ifelse(Order=="Strigiformes", "Nocturnal",
                                        ifelse(Order=="Caprimulgiformes", "Nocturnal", "Diurnal"))))

vroom_write(BM_trait, "D:/Matt/Extra files for process/Bird_Mass.csv", delim = ",")
vroom_write(BL_trait, "D:/Matt/Extra files for process/Bird_Length.csv", delim = ",")
vroom_write(BS_trait, "D:/Matt/Extra files for process/Bird_Size.csv", delim = ",")

###Create Mammal Data files

Mammal_Mass <- Mass %>% filter(Class=="Mammalia")
Mammal_Length <- Length %>% filter(Class=="Mammalia")
Mammal_Size <- Size %>% filter(Class=="Mammalia")

mamtrait <- vroom("E:/Coding Files/Chapter 2 Validation/Extra files for process/Mammal traits.csv")
mamtrait <- mamtrait %>% distinct(Binomial, .keep_all = TRUE)
iucn_m <- vroom("E:/Coding Files/Chapter 2 Validation/Extra files for process/iucnclass_mammals.csv")

M_Mammal <- Mammal_Mass %>%left_join(mamtrait)%>%left_join(iucn)%>%
filter(!is.na(TPI_month_max))%>%filter(!is.na(AI))%>%filter(!is.na(HLU))%>%
  filter(!lifestyle=="Scansorial")%>%droplevels()%>%
  filter(!is.na(lifestyle))%>%filter(!is.na(activity_cycle))%>%filter(!is.na(hibernation_torpor))

L_Mammal <- Mammal_Length %>%left_join(mamtrait)%>%left_join(iucn)%>%
  filter(!is.na(TPI_month_max))%>%filter(!is.na(AI))%>%filter(!is.na(HLU))%>%
  filter(!lifestyle=="Scansorial")%>%droplevels()%>%
  filter(!is.na(lifestyle))%>%filter(!is.na(activity_cycle))%>%filter(!is.na(hibernation_torpor))

S_Mammal <- Mammal_Size %>%left_join(mamtrait)%>%left_join(iucn)%>%
  filter(!is.na(TPI_month_max))%>%filter(!is.na(AI))%>%filter(!is.na(HLU))%>%
  filter(!lifestyle=="Scansorial")%>%droplevels()%>%
  filter(!is.na(lifestyle))%>%filter(!is.na(activity_cycle))%>%filter(!is.na(hibernation_torpor))

vroom_write(M_Mammal, "D:/Matt/Extra files for process/Mammal_Mass.csv", delim = ",")
vroom_write(L_Mammal, "D:/Matt/Extra files for process/Mammal_Length.csv", delim = ",")
vroom_write(S_Mammal, "D:/Matt/Extra files for process/Mammal_Size.csv", delim = ",")
