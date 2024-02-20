# *********************************************************
#
#   Introduction to Phylogenetic Comparative Methods
#
#   Date:   29.02.2024
#
#   Author: Emma Dunne (emma.dunne@fau.de)
#
# _________________________________________________________
#
#   Cleaning of data for PCM tutorial
# 
# *********************************************************


## Load packages:
library(tidyverse)
library(geiger)

## This short script was used to clean up our phylogenetic and trait data
## This is an important step in any form of data analysis

## The following tree and trait data are from Thomas et al. (2020)
## (https://doi.org/10.1098/rspb.2020.1393)
## If you want to reuse this data, please remember to cite this paper 
## And don't forget to always cite your R packages!


## Load tree file (in nexus format):
frog_tree <- read.nexus("data/frog-tree.nex")

## Load the trait data:
frog_data <- read_csv("data/frog-eyes.csv")

## Full list of variables and their descriptions:
# - Binomial - the species binomial name
# - Family - the family the species belongs to
# - Genus - the genus the species belongs to
# - Adult_habitat - habitat of adults. Categories are: Ground-dwelling, Subfossorial, Scansorial (i.e. tree-dwelling), Semiaquatic, Aquatic, or Fossorial (i.e. burrowers).
# - Life_history - whether the larvae are free-living (Free-living larvae) or not (No free-living larvae).
# - Sex_dichromatism - whether different sexes are different colours (Present) or not (Absent).
# - SVL - snout vent length (in mm). This is a common way to measure body size in amphibians.
# - mass - body mass (in g)
# - rootmass - cube root of the body mass.
# - eyesize - eye size (in mm) for the species. This is an everage across left and right eyes from three individuals per species


## Next we need to make sure our trait data matches to our phylogeny
## First, add underscores to data file
frog_data <- frog_data %>%
  mutate(Binomial = str_replace(string = Binomial, pattern = " ", replacement = "_"))
## Check that it worked
head(frog_data$Binomial)

## Check whether the names match in the data and the tree
check <- name.check(phy = frog_tree, data = frog_data, 
                    data.names = frog_data$Binomial)
check # open the object

## Now, let's remove those mismatched species from the tree
mytree <- drop.tip(frog_tree, check$tree_not_data) 
str(mytree) # check (tips and nodes)

## And the same for the trait data, using a slightly different function
matches <- match(frog_data$Binomial, check$data_not_tree, nomatch = 0)
mydata <- subset(frog_data, matches == 0)
glimpse(mydata) # check (rows)

## Check that everything matches
check_final <- name.check(phy = mytree, data = mydata, 
                    data.names = mydata$Binomial)
check_final # should say ok!


## Finally, convert to a data frame
mydata <- as.data.frame(mydata)
# Check this is now a data.frame
class(mydata)

# Write the cleaned data to a new file
write_csv(mydata, file = "data/clean-frog-data.csv")

# Write the cleaned tree to a new file
write.nexus(mytree, file = "data/clean-frog-tree.nex")

