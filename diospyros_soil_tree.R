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

nwk<-root(nwk, which(nwk$tip.label == "sandwicensisG"))

#nwk$tip.label <- nwk$sample_id

dd <- data.frame(taxa=nwk$tip.label)
circ<-ggtree(nwk, size=2.3, layout = "circular")
circ <- circ %<+% dd
circ + geom_tiplab(size=3)


test<-leaf_soil_species %>% 
  dplyr::select(demandeur, Mg_leaf, Ca_soil, Mg_soil, Na_soil, K_soil, P_soil, Al_soil, Fe_soil, Mn_soil, Cr_soil, Co_soil, Ni_soil, Cu_soil, Cd_soil, Mo_soil, Pb_soil, Zn_soil, S_soil)

row_means<-test %>% dplyr::select(-demandeur) %>% rowMeans()
row_sds<-test %>% dplyr::select(-demandeur) %>% as.matrix %>% rowSds()
demandeur<- test %>% dplyr::select(demandeur)
test<-test %>% dplyr::select(-demandeur)
test <- test/row_means
# z score: test <- (test - row_means)/row_sds


test<-cbind(demandeur, test)

test<-inner_join(test, data.frame(sample_id=nwk$sample_id,
                            species = nwk$tip.label), by=c("demandeur"="sample_id"), multiple = "all")
species_localities<-read_delim("species_localities.tsv")

test<-inner_join(test, species_localities, by=c("demandeur"="Ind ID"))
test<-column_to_rownames(test, var="species.x")

test$Soil[test$Soil ==  "Sedimentary (Black clays)"] = "Sedimentary"
test$Soil[test$Soil ==  "ultramafic"] = "Ultramafic"

test



p1 <- gheatmap(circ + geom_tiplab(size=3), test[, c("Mn_soil", "Cr_soil", "Co_soil", 
                                                    "Ni_soil", "Cu_soil", "Cd_soil", 
                                                    "P_soil"), drop=F], offset=.05, width=0.3,
               colnames_angle=90, low = "cornsilk", high = "mediumorchid3")

p1 <- gheatmap(circ + geom_tiplab(size=3), test[, c("Mn_soil"), drop=F], offset=.05, width=0.3,
               colnames_angle=90, low = "cornsilk", high = "mediumorchid3")

library(ggnewscale)
p2 <- p1 + new_scale_fill()

cols=c("white", "mediumblue", "darkmagenta", "darkgreen", "seagreen2", "magenta", "white")

gheatmap(p2, test[, c("Soil"), drop=F], offset=.015, width=.25,
         colnames_angle=90, colnames_offset_y = 0.015) + scale_fill_manual(values=cols, na.value = "white")















gheatmap(circ + geom_tiplab(size=3), test[c("Mg_leaf", "Ca_soil", "Mg_soil", "Na_soil", "K_soil", "P_soil", "Al_soil", "Fe_soil"), drop=F], offset=.025, width=.5,
         colnames_angle=90, colnames_offset_y = .15)

gheatmap(circ + geom_tiplab(size=3), test[, c("Mn_soil", "Cr_soil", "Co_soil", "Ni_soil", "Cu_soil", "Cd_soil", "Mo_soil"), drop=F], offset=.0001, width=.5,
         colnames_angle=90, colnames_offset_y = .15)

gheatmap(circ + geom_tiplab(size=3), test[, c("Mn_soil", "Ni_soil", "Cr_soil"), drop=F], offset=.0001, width=.5,
         colnames_angle=90, colnames_offset_y = .15)






circ$data$label %in% rownames(test)
