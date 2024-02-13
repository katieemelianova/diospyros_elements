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
library(stringr)   
library(ggfortify)
library(ggplot2)
library(matrixStats)
library(factoextra)


# read in element data
leaf<-read_delim("leaf_chemistry_nosymbols.txt", locale=locale(decimal_mark = ","), trim_ws=TRUE)
soil<-read_delim("soil_chemistry_nosymbols.txt", locale=locale(decimal_mark = ","), trim_ws=TRUE)
species_localities<-read_delim("species_localities.tsv")


#   label colnames according to tissue and join datasets
leaf_soil<-inner_join(leaf, soil, by="demandeur")
new_colnames<-leaf_soil %>% colnames() %>% str_replace(".x", "_leaf") %>% str_replace(".y", "_soil")
leaf_soil %<>% set_colnames(new_colnames)
leaf_soil_species<-inner_join(leaf_soil, species_localities, by=c("demandeur"="Ind ID"))
leaf_soil_species$demandeur[leaf_soil_species$demandeur == "1023h"] = "1023"

# read in tree
nwk<-ape::read.tree("/Users/katieemelianova/Desktop/Diospyros/diospyros_elements/318_thin8000_treein.contree")
nwk<-ape::drop.tip(nwk, nwk$tip.label[grepl("BT", nwk$tip.label)])
nwk<-ape::drop.tip(nwk, nwk$tip.label[grepl("JM", nwk$tip.label)])

# clean up tip names + remove those which are not in our dataset
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

# get the sample ID and species name from the tip labels
sample_id <- sub("[A-Za-z]$", "", nwk$tip.label) %>% str_sub(- 4, - 1)
species <- sub("[A-Za-z]$", "", nwk$tip.label) %>% str_sub(1, - 5)

# set te sample names and species names within the tree object
nwk$sample_id<-sample_id
nwk$species<-species

# roo the tree using dsandwicensis as the outgroup
nwk<-root(nwk, which(nwk$tip.label == "sandwicensisG"))

#nwk$tip.label <- nwk$sample_id

# make the tree
dd <- data.frame(taxa=nwk$tip.label)
circ<-ggtree(nwk, size=0.9, layout = "circular")
circ <- circ %<+% dd
circ + geom_tiplab(size=3)


root_samples<-leaf_soil_species %>% 
  dplyr::select(demandeur, `N (g/kg)_soil`, `C (g/kg)_soil`, Ca_soil, Mg_soil, Na_soil, K_soil,  P_soil, Al_soil, Fe_soil, Mn_soil, Cr_soil, Co_soil, Ni_soil, Cu_soil, Cd_soil, Mo_soil, Pb_soil, Zn_soil, S_soil)

leaf_samples<-leaf_soil_species %>% 
  dplyr::select(demandeur, `C (g/kg)_leaf`, `N (g/kg)_leaf`, Ca_leaf,  Mg_leaf, Na_leaf, K_leaf,  P_leaf, Co_leaf, Cr_leaf, Fe_leaf, Mn_leaf, Ni_leaf, Al_leaf, Cu_leaf, Zn_leaf, Cd_leaf, Mo_leaf, Pb_leaf, S_leaf)

# plot leaf samples elements
leaf_samples<-leaf_soil_species %>% 
  dplyr::select(demandeur, `C (g/kg)_leaf`, `N (g/kg)_leaf`, Ca_leaf,  Mg_leaf, Na_leaf, K_leaf,  P_leaf, Co_leaf, Cr_leaf, Fe_leaf, Mn_leaf, Ni_leaf, Al_leaf, Cu_leaf, Zn_leaf, Cd_leaf, Mo_leaf, Pb_leaf, S_leaf)


demandeur<- leaf_samples %>% dplyr::select(demandeur)
leaf_samples<-leaf_samples %>% dplyr::select(-demandeur)
leaf_samples<-scale(leaf_samples)
leaf_samples<-cbind(demandeur, leaf_samples)
leaf_samples<-inner_join(leaf_samples, data.frame(sample_id=nwk$sample_id, species = nwk$tip.label), by=c("demandeur"="sample_id"), multiple = "all")
species_localities<-read_delim("species_localities.tsv")
leaf_samples<-inner_join(leaf_samples, species_localities, by=c("demandeur"="Ind ID"))
leaf_samples<-column_to_rownames(leaf_samples, var="species.x")
leaf_samples$Soil[leaf_samples$Soil ==  "Sedimentary (Black clays)"] = "Sedimentary"
leaf_samples$Soil[leaf_samples$Soil ==  "ultramafic"] = "Ultramafic"

# not sure why I wrote this, commentiung out until I can debug
#leaf_samples<-inner_join(leaf_samples, data.frame(sample_id=nwk$sample_id,
#                                  species = nwk$tip.label), by=c("demandeur"="sample_id"), multiple = "all")



p1 <- gheatmap(circ + geom_tiplab(size=5), leaf_samples[, c("Co_leaf", "Fe_leaf", "Cr_leaf"), drop=F], offset=.05, width=0.3,
               colnames_angle=90, low = "dodgerblue3", high = "red", font.size=6)
library(ggnewscale)
p2 <- p1 + new_scale_fill()
cols=c("white", "mediumblue", "darkmagenta", "darkgreen", "seagreen2", "magenta", "white")
png("leaf_chem_tree_debug.png", width = 1000, height = 1000)
gheatmap(p2, leaf_samples[, c("Soil"), drop=F], offset=.035, width=.1,
         colnames_angle=90, colnames_offset_y = 0.2) + scale_fill_manual(values=cols, na.value = "white") +
  theme(text = element_text(size = 30), axis.title = element_text(size = 30), axis.text = element_text(size = 30))
dev.off()



# plot soil samples elements
soil_samples<-leaf_soil_species %>% 
  dplyr::select(demandeur, `N (g/kg)_soil`, `C (g/kg)_soil`, Ca_soil, Mg_soil, Na_soil, K_soil,  P_soil, Al_soil, Fe_soil, Mn_soil, Cr_soil, Co_soil, Ni_soil, Cu_soil, Cd_soil, Mo_soil, Pb_soil, Zn_soil, S_soil)

demandeur<- soil_samples %>% dplyr::select(demandeur)
soil_samples<-soil_samples %>% dplyr::select(-demandeur)
soil_samples<-scale(soil_samples)
soil_samples<-cbind(demandeur, soil_samples)
soil_samples<-inner_join(soil_samples, data.frame(sample_id=nwk$sample_id, species = nwk$tip.label), by=c("demandeur"="sample_id"), multiple = "all")
species_localities<-read_delim("species_localities.tsv")
soil_samples<-inner_join(soil_samples, species_localities, by=c("demandeur"="Ind ID"))
soil_samples<-column_to_rownames(soil_samples, var="species.x")
soil_samples$Soil[soil_samples$Soil ==  "Sedimentary (Black clays)"] = "Sedimentary"
soil_samples$Soil[soil_samples$Soil ==  "ultramafic"] = "Ultramafic"

# not sure why I wrote this, commentiung out until I can debug
#soil_samples<-inner_join(leaf_samples, data.frame(sample_id=nwk$sample_id,
#                                  species = nwk$tip.label), by=c("demandeur"="sample_id"), multiple = "all")


p1 <- gheatmap(circ + geom_tiplab(size=5), soil_samples[, c("Co_soil", "Fe_soil", "Cr_soil"), drop=F], offset=.05, width=0.3,
               colnames_angle=90, low = "dodgerblue3", high = "red", font.size=6)
library(ggnewscale)
p2 <- p1 + new_scale_fill()
cols=c("white", "mediumblue", "darkmagenta", "darkgreen", "seagreen2", "magenta", "white")
png("leaf_chem_tree_debug.png", width = 1000, height = 1000)
gheatmap(p2, soil_samples[, c("Soil"), drop=F], offset=.035, width=.1,
         colnames_angle=90, colnames_offset_y = 0.2) + scale_fill_manual(values=cols, na.value = "white") +
  theme(text = element_text(size = 30), axis.title = element_text(size = 30), axis.text = element_text(size = 30))
dev.off()


# plot soil samples elements
soil_samples<-leaf_soil_species %>% 
  dplyr::select(demandeur, `N (g/kg)_soil`, `C (g/kg)_soil`, Ca_soil, Mg_soil, Na_soil, K_soil,  P_soil, Al_soil, Fe_soil, Mn_soil, Cr_soil, Co_soil, Ni_soil, Cu_soil, Cd_soil, Mo_soil, Pb_soil, Zn_soil, S_soil)
p1 <- gheatmap(circ + geom_tiplab(size=5), soil_samples[, c("Co_leaf", "Fe_leaf", "Cr_leaf"), drop=F], offset=.05, width=0.3,
               colnames_angle=90, low = "dodgerblue3", high = "red", font.size=6)
library(ggnewscale)
p2 <- p1 + new_scale_fill()
cols=c("white", "mediumblue", "darkmagenta", "darkgreen", "seagreen2", "magenta", "white")
png("soil_chem_tree.png", width = 1000, height = 1000)
gheatmap(p2, soil_samples[, c("Soil"), drop=F], offset=.035, width=.1,
         colnames_angle=90, colnames_offset_y = 0.2) + scale_fill_manual(values=cols, na.value = "white") +
  theme(text = element_text(size = 30), axis.title = element_text(size = 30), axis.text = element_text(size = 30))
dev.off()


# plot PCA loadings
res.pca <- soil_samples %>% 
  dplyr::select(-c(demandeur, 
                   species.y, 
                   `Latitude (pink cells are estimated)`, 
                   Longitude, 
                   Locality, 
                   Vegetation, 
                   Soil)) %>%
  prcomp(scale=TRUE)
res.pca$species <- soil_samples$Soil
autoplot(res.pca, loadings=TRUE, loadings.label=TRUE)

# plot PCA of soil chem coloured by soiltype
fviz_pca_ind(res.pca, col.ind = soil_samples$Soil, title = "Soil Chemistry", labelsize = 6) +
  theme(text = element_text(size = 30),
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 30))










