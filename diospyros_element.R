library(dplyr)
library(magrittr)
library(readr)
library(stringr)
library(tidyverse)
library(ggplot2)


############################
#       read in data       #
############################

leaf<-read_delim("leaf_chemistry_nosymbols.txt", locale=locale(decimal_mark = ","), trim_ws=TRUE)
soil<-read_delim("soil_chemistry_nosymbols.txt", locale=locale(decimal_mark = ","), trim_ws=TRUE)
species_localities<-read_delim("species_localities.tsv")


#############################################################
#   label colnames according to tissue and join datasets    #
#############################################################

leaf_soil<-inner_join(leaf, soil, by="demandeur")
new_colnames<-leaf_soil %>% colnames() %>% str_replace(".x", "_leaf") %>% str_replace(".y", "_soil")
leaf_soil %<>% set_colnames(new_colnames)
leaf_soil_species<-inner_join(leaf_soil, species_localities, by=c("demandeur"="Ind ID"))
leaf_soil_species$demandeur[leaf_soil_species$demandeur == "1023h"] = "1023"

colnames(leaf_soil_species)[2:46]

leaf_soil_species %>% 
  dplyr::select(colnames(leaf_soil_species)[2:46]) %>% 
  dplyr::select(-c("_15N_leaf", "_13C_leaf", "_15N_soil", "_13C_soil", "...8")) %>% 
  as.matrix() %>%
  cor() %>%
  heatmap()


leaf_soil_species %>% dplyr::select("Cu_leaf") %>% pull()

plot(leaf_soil_species$P_leaf, leaf_soil_species$K_leaf)

###########################################################################
#     make basic function to plot an individual variable across species   #
###########################################################################

plot_single_variable<-function(variable){
  variable<-sym(variable)
  leaf_soil_species %>% 
    dplyr::select("species", variable) %>% 
    ggplot(aes(x = species, y = !!variable)) + 
    geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
}

plot_single_variable("Cu_soil")
leaf_soil_species %>% colnames()







