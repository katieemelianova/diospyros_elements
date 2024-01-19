library(dplyr)
library(magrittr)
library(readr)
library(stringr)
library(tidyverse)


############################
#       read in data       #
############################

leaf<-read_delim("leaf_chemistry_nosymbols.txt", locale=locale(decimal_mark = ","))
soil<-read_delim("soil_chemistry_nosymbols.txt", locale=locale(decimal_mark = ","))
species<-read_delim("species_localities.tsv")

#############################################################
#   label colnames according to tissue and join datasets    #
#############################################################

leaf_soil<-inner_join(leaf, soil, by="demandeur")
new_colnames<-leaf_soil %>% colnames() %>% str_replace(".x", "_leaf") %>% str_replace(".y", "_soil")
leaf_soil %<>% set_colnames(new_colnames)
leaf_soil_species<-inner_join(leaf_soil, species, by=c("demandeur"="Ind ID"))


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

plot_single_variable("Ni_soil")



