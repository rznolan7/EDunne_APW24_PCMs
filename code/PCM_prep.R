# ******************************************************
#
#   Introduction to Phylogenetic Comparative Methods
#
#   29.02.2024
#
#   Emma Dunne (emma.dunne@fau.de)
#
# ______________________________________________________
#
#   Prep data and try out code
# 
# ******************************************************


## Load packages:
library(tidyverse)
library(ape)
library(geiger)
library(phytools)



# 01. Phylogenetic signal -------------------------------------------------


## Load tree
frog_tree <- read.nexus("./data/frog-tree.nex")
str(frog_tree)

## Load data
frog_eyes <- read_csv("data/frog-eyes.csv")
View(frog_eyes)







