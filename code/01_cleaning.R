# *********************************************************
#
#   Introduction to Phylogenetic Comparative Methods
#
#   Emma Dunne (emma.dunne@fau.de)
#   Late updated: August 2024
#
# _________________________________________________________
#
#   1. Cleaning and visualising tree + trait data
# 
# *********************************************************


## Load packages:
library(tidyverse)
library(geiger)
library(ape)
library(phytools)
library(viridis)

## This short script cleans up our phylogenetic and trait data
## This is an important step in any form of data analysis

## The following tree and trait data are from Thomas et al. (2020)
## (https://doi.org/10.1098/rspb.2020.1393)
## If you want to reuse this data, please remember to cite this paper 
## And don't forget to always cite your R packages!



# 1. Load and explore data + tree -----------------------------------------


## Load tree file (in nexus format):
frog_tree <- read.nexus("data/frog-tree.nex")

## Load the trait data:
frog_data <- read.csv("data/frog-eyes.csv")

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
# - eyesize - eye size (in mm) for the species. This is an average across left and right eyes from three individuals per species



# 2. Clean up data + tree -------------------------------------------------

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




# 3. Data organisation + visualisation ------------------------------------


## Before any analyses, its good to check the structure of your tree
## We need it to dichotomous, i.e. has no polytomies, rooted, and ultrametric.
## Check whether the tree is binary
is.binary(mytree)
## Check whether the tree is rooted 
is.rooted(mytree)
## Check whether the tree is ultrametric
is.ultrametric(mytree)


## First, let's take a look at the tree:
## Plot the tree as a circular/fan phylogeny with small labels
plot(mytree, cex = 0.2, typ = "fan", no.margin = TRUE)

## Look at the tree summary:
mytree

## _________________________________________
## Q: How many frog species are on the tree?
## _________________________________________


## B. Load the trait data:
mydata <- read_csv("data/clean-frog-data.csv")

## Convert to a data frame
mydata <- as.data.frame(mydata)
class(mydata) # check

## Take a look at the variables
glimpse(mydata) # columns in a list
#View(mydata) # open a new tab in RStudio

## (You can see the full list of variables and their descriptions in the 01_cleaning.R script)


## Let's do a quick check of the distribution of our data
## First, the raw continuous data:
p1 <- ggplot(mydata, aes(x = eyesize)) +
  geom_histogram(bins = 20, fill = "turquoise4") +
  theme_bw(base_size = 14)
p1 

## And now with it log-tranformed:
p2 <- ggplot(mydata, aes(x = log(eyesize))) +
  geom_histogram(bins = 20, fill = "chartreuse4") +
  theme_bw(base_size = 14)
p2

## _______________________________________________
## Q: Why might we log-transform continuous data?
## _______________________________________________
#Making the data log-transformed helps make it less skewed, which can be useful for statistical tests.
#choose which trait to plot
trait <- "eyesize"
trait <- "SVL"
trait <- "mass"

## Take our trait of interest (i.e. eyesize) and log transform it:
logTrait <- log(pull(mydata, trait))
names(logTrait) <- mydata$Binomial # give these values names (i.e. species names)
head(logTrait) # look at the first few rows



## Next, let's plot these trait data onto the phylogeny!
## The contMap() function in the R package 'phytools' projects the observed and 
##    reconstructed values of a continuous trait onto the edges of a tree using a 
##    color gradient. We will use the colour gradients from another R package, 
##    'viridis' to ensure the output is as readable as possible.


## Create "contMap" object using this log-transformed data:
frog_contMap <- contMap(mytree, logTrait, 
                        plot=FALSE, res=200)

## Set up the colour gradient: uses visidis package
n <- length(frog_contMap$cols) # number of colour breaks
frog_contMap$cols[1:n] <- viridis(n) # using the gradient 'viridis' ( = yellow to purple)

## Plot the tree:
plot(frog_contMap, fsize = c(0.4, 1), outline = FALSE, lwd = c(3, 7), leg.txt = "Eye size (log)")


## _______________________________________________________________
## Q: Do you notice any particular trends across the phylogeny?
## Bonus Q: What other traits could you plot on a tree like this?
## _______________________________________________________________



