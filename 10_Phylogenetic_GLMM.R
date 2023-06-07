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
M_Mass_tree+c <- M_Mass_c %>% filter(!Binomial %in% mamdif)

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
Non_phylo_m <- pglmm(LMass ~ Year_sc + TPI_month_max + AI + HLU +
                          lifestyle + activity_cycle + hibernation_torpor +
                          TPI_month_max:AI + TPI_month_max:HLU+
                          TPI_month_max:lifestyle + TPI_month_max:activity_cycle +
                          TPI_month_max:hibernation_torpor +
                           (1|Binomial)+(1|Season), 
                         data=M_Mass_tree,
                         bayes = TRUE)

phylo_m <- pglmm(LMass ~ Year_sc + TPI_month_max + AI_cor + HLU +
                      lifestyle + activity_cycle + hibernation_torpor +
                      TPI_month_max:AI_cor + TPI_month_max:HLU+
                      TPI_month_max:lifestyle + TPI_month_max:activity_cycle +
                      TPI_month_max:hibernation_torpor +
                      (1|Binomial__)+(1|Season), 
                      data=M_Mass_tree, cov_ranef = list(Binomial = mam_tree_100), 
                      bayes = TRUE)

sink("Mammal_mass_nonphylo_and_phylo.txt")
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
Non_phylo_l <- pglmm(LLength ~ Year_sc + TPI_month_max + AI + HLU +
                       lifestyle + activity_cycle + hibernation_torpor +
                       TPI_month_max:HLU+
                       TPI_month_max:activity_cycle +
                       TPI_month_max:hibernation_torpor +
                       (1|Binomial),data=M_Length_tree, cov_ranef = list(Binomial = mam_tree_100),
                         bayes = TRUE)


phylo_l <- pglmm(LLength ~ Year_sc + TPI_month_max + AI + HLU +
                   lifestyle + activity_cycle + hibernation_torpor +
                   TPI_month_max:HLU+
                   TPI_month_max:activity_cycle +
                   TPI_month_max:hibernation_torpor +
                        (1|Binomial__),data=M_Length_tree, cov_ranef = list(Binomial = mam_tree_100), 
                      bayes = TRUE)

sink("Mammal_length_nonphylo_and_phylo.txt")
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

Non_phylo_s <- pglmm(LSize ~ Year_sc + TPI_month_max + AI + HLU +
                     activity_cycle + hibernation_torpor +
                     TPI_month_max:activity_cycle +
                       (1|Binomial)+(1|Season),data=M_Size_tree
                        ,bayes = TRUE)

phylo_s <- pglmm(LSize ~ Year_sc + TPI_month_max + AI + HLU +
                     activity_cycle + hibernation_torpor +
                     TPI_month_max:activity_cycle +
                        (1|Binomial__)+(1|Season), 
                      data=M_Size_tree, cov_ranef = list(Binomial = mam_tree_100), 
                      bayes = TRUE)

sink("Mammal_size_nonphylo_and_phylo.txt")
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

