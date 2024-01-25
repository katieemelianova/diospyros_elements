library(sf)
library(mapview)
library(readr)
library(dplyr)
library(magrittr)
library(RColorBrewer)
library(tidyr)
library(ggtree)
library(ape)
library(stringr)
library(reshape2)
library(ggnewscale)
library(ggtreeExtra)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(grid)
library(ggpubr)
library("stringr")      


# read in tree
nwk<-ape::read.tree("/Users/katieemelianova/Desktop/Diospyros/diospyros_elements/318_thin8000_treein.contree")
nwk<-ape::drop.tip(nwk, nwk$tip.label[grepl("BT", nwk$tip.label)])
nwk<-ape::drop.tip(nwk, nwk$tip.label[grepl("JM", nwk$tip.label)])

genomic_tips<-nwk$tip.label[grepl("G", nwk$tip.label)]
genomic_tips<-genomic_tips[!genomic_tips == "sandwicensisG"]
nwk<-ape::drop.tip(nwk, genomic_tips)
nwk<-ape::drop.tip(nwk, c("ferreaSD2012", 
                          "ferreaSD2013", 
                          "minimifoliaD4G", 
                          "calciphilaYP124", 
                          "impolitaVH3809", 
                          "parvXyahoN2071",
                          "yahouensisH3637",
                          "flavocarpaPig5223",
                          "trisulcaM3179",
                          "perplexaH3614",
                          "fastidiosa1046"))


sample_id <- sub("[A-Za-z]$", "", nwk$tip.label) %>% str_sub(- 4, - 1)
species <- sub("[A-Za-z]$", "", nwk$tip.label) %>% str_sub(1, - 5)


nwk$sample_id<-sample_id
nwk$species<-species

nwk<-root(nwk, which(nwk$tip.label == "sandwicensisG0000"))


dd <- data.frame(taxa=nwk$tip.label)
p<-ggtree(nwk, size=2.3)
p <- p %<+% dd

p + geom_tiplab(size=3)




