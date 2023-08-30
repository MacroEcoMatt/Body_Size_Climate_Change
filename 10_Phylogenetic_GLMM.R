#' ---
#' step 10: Conduct Phylogenetic GLMM Analyses
#' Matthew Watson
#' Utilize phylogenetic trees to conduct PGLMM to test if Phylogentic distance impacts results
#' ---

library(vroom)
library(dplyr)
library(ape)
library(phyr)

setwd("") #set to where model output files will be stored
###############################MAMMAL ANALYSIS#############################################
#change file path to location of datafiles
M_Mass <- vroom("./Data_S4_Mammal_Mass.csv")
M_Length <- vroom("./Data_S5_Mammal_Length.csv")
M_Size <- vroom("./Data_S6_Mammal_Size.csv")%>%mutate(LSize = (log10(Mass)/log10(Body_Length)), AI = ifelse(Aridity >100),100,Aridity))

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

###Phylogenetic Tree###
mammaltree <- read.tree("E:/Coding Files/Chapter 2 Validation/Extra files for process/mammalTree.newick")

oldname_m <- mammaltree$tip.label
newname_m <- mammaltree$tip.label
for (n in 1:length(newname_m)){
  newname_m[n] <- sub("([A-Za-z]+_[A-Za-z]+).*", "\\1", newname_m[n])
  newname_m[n] <- sub("_"," ", newname_m[n])
}
DF_names_m <- as.data.frame(cbind(oldname_m,newname_m))
mammaltree$tip.label<-DF_names_m[[2]][match(mammaltree$tip.label, DF_names_m[[1]])]

binom_mass <- unique(M_Mass$Binomial)
mam_tree_mass <-drop.tip(mammaltree, mammaltree$tip.label[-na.omit(match(binom_mass,mammaltree$tip.label))])
mt <- unique(mam_tree_mass$tip.label)
mamdif <- setdiff(binom_mass,mt)
M_Mass_tree <- M_Mass %>% filter(!Binomial %in% mamdif)
M_Mass_tree_c <- M_Mass_c %>% filter(!Binomial %in% mamdif)

binom_length <- unique(M_Length$Binomial)
mam_tree_length <-drop.tip(mammaltree, mammaltree$tip.label[-na.omit(match(binom_length,mammaltree$tip.label))])
ml <- unique(mam_tree_length$tip.label)
mamdif_l <- setdiff(binom_length,ml)

M_Length$Binomial[M_Length$Binomial=="Neotamias amoenus"] <- "Tamias amoenus"
M_Length$Binomial[M_Length$Binomial=="Neotamias siskiyou"] <- "Tamias siskiyou"
M_Length$Binomial[M_Length$Binomial=="Neotamias minimus"] <- "Tamias minimus"
M_Length$Binomial[M_Length$Binomial=="Neotamias dorsalis"] <- "Tamias dorsalis"
M_Length$Binomial[M_Length$Binomial=="Neotamias speciosus"] <- "Tamias speciosus"
M_Length$Binomial[M_Length$Binomial=="Neotamias quadrivittatus"] <- "Tamias quadrivittatus"
M_Length$Binomial[M_Length$Binomial=="Neotamias senex"] <- "Tamias senex"
M_Length$Binomial[M_Length$Binomial=="Neotamias townsendii"] <- "Tamias townsendii"
M_Length$Binomial[M_Length$Binomial=="Neotamias umbrinus"] <- "Tamias umbrinus"
M_Length_tree <- M_Length %>% filter(!Binomial %in% mamdif_l)

M_Length_c$Binomial[M_Length_c$Binomial=="Neotamias amoenus"] <- "Tamias amoenus"
M_Length_c$Binomial[M_Length_c$Binomial=="Neotamias siskiyou"] <- "Tamias siskiyou"
M_Length_c$Binomial[M_Length_c$Binomial=="Neotamias minimus"] <- "Tamias minimus"
M_Length_c$Binomial[M_Length_c$Binomial=="Neotamias dorsalis"] <- "Tamias dorsalis"
M_Length_c$Binomial[M_Length_c$Binomial=="Neotamias speciosus"] <- "Tamias speciosus"
M_Length_c$Binomial[M_Length_c$Binomial=="Neotamias quadrivittatus"] <- "Tamias quadrivittatus"
M_Length_c$Binomial[M_Length_c$Binomial=="Neotamias senex"] <- "Tamias senex"
M_Length_c$Binomial[M_Length_c$Binomial=="Neotamias townsendii"] <- "Tamias townsendii"
M_Length_c$Binomial[M_Length_c$Binomial=="Neotamias umbrinus"] <- "Tamias umbrinus"
M_Length_tree_c <- M_Length_c %>% filter(!Binomial %in% mamdif_l)

binom_size <- unique(M_Size$Binomial)
mam_tree_size <-drop.tip(mammaltree, mammaltree$tip.label[-na.omit(match(binom_size,mammaltree$tip.label))])
ms <- unique(mam_tree_size$tip.label)
mamdif_s <- setdiff(binom_size,ms)

M_Size$Binomial[M_Size$Binomial=="Neotamias amoenus"] <- "Tamias amoenus"
M_Size$Binomial[M_Size$Binomial=="Neotamias siskiyou"] <- "Tamias siskiyou"
M_Size$Binomial[M_Size$Binomial=="Neotamias minimus"] <- "Tamias minimus"
M_Size$Binomial[M_Size$Binomial=="Neotamias dorsalis"] <- "Tamias dorsalis"
M_Size$Binomial[M_Size$Binomial=="Neotamias speciosus"] <- "Tamias speciosus"
M_Size$Binomial[M_Size$Binomial=="Neotamias quadrivittatus"] <- "Tamias quadrivittatus"
M_Size$Binomial[M_Size$Binomial=="Neotamias senex"] <- "Tamias senex"
M_Size$Binomial[M_Size$Binomial=="Neotamias townsendii"] <- "Tamias townsendii"
M_Size$Binomial[M_Size$Binomial=="Neotamias umbrinus"] <- "Tamias umbrinus"
M_Size_tree <- M_Size %>% filter(!Binomial %in% mamdif_s)

M_Size_c$Binomial[M_Size_c$Binomial=="Neotamias amoenus"] <- "Tamias amoenus"
M_Size_c$Binomial[M_Size_c$Binomial=="Neotamias siskiyou"] <- "Tamias siskiyou"
M_Size_c$Binomial[M_Size_c$Binomial=="Neotamias minimus"] <- "Tamias minimus"
M_Size_c$Binomial[M_Size_c$Binomial=="Neotamias dorsalis"] <- "Tamias dorsalis"
M_Size_c$Binomial[M_Size_c$Binomial=="Neotamias speciosus"] <- "Tamias speciosus"
M_Size_c$Binomial[M_Size_c$Binomial=="Neotamias quadrivittatus"] <- "Tamias quadrivittatus"
M_Size_c$Binomial[M_Size_c$Binomial=="Neotamias senex"] <- "Tamias senex"
M_Size_c$Binomial[M_Size_c$Binomial=="Neotamias townsendii"] <- "Tamias townsendii"
M_Size_c$Binomial[M_Size_c$Binomial=="Neotamias umbrinus"] <- "Tamias umbrinus"
M_Size_tree_c <- M_Size_c %>% filter(!Binomial %in% mamdif_s)

####MASS ANALYSIS####
Non_phylo_m <- pglmm(LMass ~ TPI_month_max + AI + HLU +
                       lifestyle + activity_cycle + hibernation_torpor +
                       TPI_month_max:AI + TPI_month_max:HLU +
                       TPI_month_max:lifestyle + TPI_month_max:activity_cycle + 
                       TPI_month_max:hibernation_torpor +
                       (1|Binomial), 
                     data=M_Mass_tree,
                     bayes = TRUE)

phylo_m <- pglmm(LMass ~ TPI_month_max + AI + HLU +
                   lifestyle + activity_cycle + hibernation_torpor +
                   TPI_month_max:AI + TPI_month_max:HLU +
                   TPI_month_max:lifestyle + TPI_month_max:activity_cycle + 
                   TPI_month_max:hibernation_torpor +
                   (1|Binomial__), 
                 data=M_Mass_tree, cov_ranef = list(Binomial = mam_tree_mass), 
                 bayes = TRUE)

sink("Mammal_mass_nonphylo_and_phylo.csv")
"Non Phylogenetic Model"
summary(Non_phylo_m)
"R squared"
print(rr2::R2_pred(Non_phylo_m))
""
""
"Phylogenetic Model"
summary(phylo_m)
"R squared"
print(rr2::R2_pred(phylo_m))
sink()

####LENGTH ANALYSIS####
Non_phylo_l <- pglmm(LLength ~ TPI_month_max + AI + HLU +
                       lifestyle + activity_cycle + hibernation_torpor +
                       TPI_month_max:AI + TPI_month_max:HLU +
                       TPI_month_max:lifestyle + TPI_month_max:activity_cycle + 
                       TPI_month_max:hibernation_torpor +
                       (1|Binomial),data=M_Length_tree, cov_ranef = list(Binomial = mam_tree_length), 
                     bayes = TRUE)


phylo_l <- pglmm(LLength ~ TPI_month_max + AI + HLU +
                   lifestyle + activity_cycle + hibernation_torpor +
                   TPI_month_max:AI + TPI_month_max:HLU +
                   TPI_month_max:lifestyle + TPI_month_max:activity_cycle + 
                   TPI_month_max:hibernation_torpor +
                   (1|Binomial__),data=M_Length_tree, cov_ranef = list(Binomial = mam_tree_length), 
                 bayes = TRUE)

sink("Mammal_length_nonphylo_and_phylo.csv")
"Non Phylogenetic Model"
summary(Non_phylo_l)
"R squared"
print(rr2::R2_pred(Non_phylo_l))
""
""
"Phylogenetic Model"
summary(phylo_l)
"R squared"
print(rr2::R2_pred(phylo_l))
sink()

####SIZE ANALYSIS####

Non_phylo_s <- pglmm(LSize ~ TPI_month_max + AI + HLU +
                       lifestyle + activity_cycle + hibernation_torpor +
                       TPI_month_max:AI + TPI_month_max:HLU +
                       TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                       (1|Binomial),data=M_Size_tree
                     ,bayes = TRUE)

phylo_s <- pglmm(LSize ~ TPI_month_max + AI + HLU +
                   lifestyle + activity_cycle + hibernation_torpor +
                   TPI_month_max:AI + TPI_month_max:HLU +
                   TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:hibernation_torpor +
                   (1|Binomial__), 
                 data=M_Size_tree, cov_ranef = list(Binomial = mam_tree_size), 
                 bayes = TRUE)

sink("Mammal_size_nonphylo_and_phylo.csv")
"Non Phylogenetic Model"
summary(Non_phylo_s)
"R squared"
print(rr2::R2_pred(Non_phylo_s))
""
""
"Phylogenetic Model"
summary(phylo_s)
"R squared"
print(rr2::R2_pred(phylo_s))
sink()


###############################BIRD ANALYSIS#############################################
#change file path to location of datafiles
B_Mass <- vroom("./Data_S1_Bird_Mass.csv")%>%mutate(LMass = log10(Mass), AI = ifelse(Aridity >100,100,Aridity))
B_Length <- vroom("./Data_S2_Bird_Length.csv")%>%mutate(LLength = log10(Body_Length), AI = ifelse(Aridity >100,100,Aridity))%>%filter(AI<75)
B_Size <- vroom("./Data_S3_Bird_Size.csv")%>%mutate(LSize = (log10(Mass)/log10(Body_Length)), AI = ifelse(Aridity >100,100,Aridity))%>%filter(AI<75)


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

###Phylogenetic Tree###
Birdtree <- read.tree("E:/Coding Files/Chapter 2 Validation/Extra files for process/FinalBirdTree_analysis.tre")

oldname_m <- Birdtree$tip.label
newname_m <- Birdtree$tip.label
for (n in 1:length(newname_m)){
  newname_m[n] <- sub("([A-Za-z]+_[A-Za-z]+).*", "\\1", newname_m[n])
  newname_m[n] <- sub("_"," ", newname_m[n])
}
DF_names_m <- as.data.frame(cbind(oldname_m,newname_m))
Birdtree$tip.label<-DF_names_m[[2]][match(Birdtree$tip.label, DF_names_m[[1]])]

m100 <- unique(B_Mass$Binomial)
l100 <- unique(B_Length$Binomial)
s100 <- unique(B_Size$Binomial)

bird_tree_mass <-drop.tip(Birdtree, Birdtree$tip.label[-na.omit(match(m100,Birdtree$tip.label))])
bird_tree_length <-drop.tip(Birdtree, Birdtree$tip.label[-na.omit(match(l100,Birdtree$tip.label))])
bird_tree_size <-drop.tip(Birdtree, Birdtree$tip.label[-na.omit(match(s100,Birdtree$tip.label))])


treem <- unique(bird_tree_mass$tip.label)
treel <- unique(bird_tree_length$tip.label)
trees <- unique(bird_tree_size$tip.label)

diff_m <- setdiff(m100,treem)
diff_l<- setdiff(l100,treel)
diff_s<- setdiff(s100,trees)

B_Mass_tree <- B_Mass %>% filter(!Binomial %in% diff_m)
B_Mass_c_tree <- B_Mass_c %>% filter(!Binomial %in% diff_m)
B_Length_tree <- B_Length %>% filter(!Binomial %in% diff_l)
B_Length_c_tree <- B_Length_c %>% filter(!Binomial %in% diff_l)
B_Size_tree <- B_Size %>% filter(!Binomial %in% diff_s)
B_Size_c_tree <- B_Size_c %>% filter(!Binomial %in% diff_s)

####MASS ANALYSIS####
Non_phylo_b <- pglmm(LMass ~ TPI_month_max + AI + HLU +
                       lifestyle + activity_cycle + Migration +
                       TPI_month_max:AI + TPI_month_max:HLU +
                       TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:Migration +
                       (1|Binomial), 
                     data=B_Mass_tree,
                     bayes = TRUE)

phylo_b <- pglmm(LMass ~ TPI_month_max + AI + HLU +
                   lifestyle + activity_cycle + Migration +
                   TPI_month_max:AI + TPI_month_max:HLU +
                   TPI_month_max:lifestyle + TPI_month_max:activity_cycle + TPI_month_max:Migration +
                   (1|Binomial__), 
                 data=B_Mass_tree, cov_ranef = list(Binomial = bird_tree_mass), 
                 bayes = TRUE)

sink("Bird_mass_nonphylo_and_phylo.csv")
"Non Phylogenetic Model"
summary(Non_phylo_b)
"R squared"
print(rr2::R2_pred(Non_phylo_b))
""
""
"Phylogenetic Model"
summary(phylo_b)
"R squared"
print(rr2::R2_pred(phylo_b))
sink()

####LENGTH ANALYSIS####
Non_phylo_lb <- pglmm(LLength ~ TPI_month_max + AI + HLU +
                        lifestyle + activity_cycle + Migration +
                        TPI_month_max:AI + TPI_month_max:HLU +
                        TPI_month_max:lifestyle + TPI_month_max:Migration +
                        (1|Binomial),data=B_Length_tree,
                      bayes = TRUE)


phylo_lb <- pglmm(LLength ~ TPI_month_max + AI + HLU +
                    lifestyle + activity_cycle + Migration +
                    TPI_month_max:AI + TPI_month_max:HLU +
                    TPI_month_max:lifestyle + TPI_month_max:Migration +
                    (1|Binomial__), data= B_Length_tree, cov_ranef = list(Binomial = bird_tree_length), 
                  bayes = TRUE)

sink("Bird_length_nonphylo_and_phylo.csv")
"Non Phylogenetic Model"
summary(Non_phylo_lb)
"R squared"
print(rr2::R2_pred(Non_phylo_lb))
""
""
"Phylogenetic Model"
summary(phylo_lb)
"R squared"
print(rr2::R2_pred(phylo_lb))
sink()

####SIZE ANALYSIS####

Non_phylo_sb <- pglmm(LSize ~ TPI_month_max + AI + HLU +
                        lifestyle + activity_cycle + Migration+
                        TPI_month_max:AI + TPI_month_max:lifestyle+
                        TPI_month_max:Migration+
                        (1|Binomial),data=B_Size_tree
                      ,bayes = TRUE)

phylo_sb <- pglmm(LSize  ~ TPI_month_max + AI + HLU +
                    lifestyle + activity_cycle + Migration+
                    TPI_month_max:AI + TPI_month_max:lifestyle+
                    TPI_month_max:Migration+
                    (1|Binomial__), 
                  data=B_Size_tree, cov_ranef = list(Binomial = bird_tree_size), 
                  bayes = TRUE)

sink("Bird_size_nonphylo_and_phylo.csv")
"Non Phylogenetic Model"
summary(Non_phylo_sb)
"R squared"
print(rr2::R2_pred(Non_phylo_sb))
""
""
"Phylogenetic Model"
summary(phylo_sb)
"R squared"
print(rr2::R2_pred(phylo_sb))
sink()
