#' ---
#' step 8: create bird phylogeny
#' Matthew Watson
#' data is downloaded birdtree.org
#' only change needed in out <- location and all_clim <- location
#' ---

library(ape)
library(tracerer)
library(treeio)

t1 <- read.tree("E:/Coding Files/Phylogenetic Trees/Final Birds/AllBirdsHackett1.tre")
t2 <- read.tree("E:/Coding Files/Phylogenetic Trees/Final Birds/BirdzillaHackett2.tre")
t3 <- read.tree("E:/Coding Files/Phylogenetic Trees/Final Birds/BirdzillaHackett3.tre")
t4 <- read.tree("E:/Coding Files/Phylogenetic Trees/Final Birds/BirdzillaHackett4.tre")

t5 <- c(t1,t2,t3,t4)
save_beast_trees(t5, "E:/Coding Files/Phylogenetic Trees/Final Birds/birdtrees_4000.trees")

#from here use treeAnnotator from BEAST to generate MCC tree

library(tidytree)
tf <- read.beast("E:/Coding Files/Phylogenetic Trees/Final Birds/FinalBirdTree_4000.tree")
tf <- as.phylo(tf)
write.tree(tf, file = "E:/Coding Files/Phylogenetic Trees/Final Birds/FinalBirdTree_analysis.tre")
