# *********************************************************
#
#   Introduction to Phylogenetic Comparative Methods
#
#   Emma Dunne (emma.dunne@fau.de)
#   Late updated: August 2024
#
# _________________________________________________________
#
#   2. Tutorial using data modified from Thomas et al. (2020)
#       (https://doi.org/10.1098/rspb.2020.1393)
# 
# *********************************************************


## Load packages:
library(ape)
library(phytools)
library(viridis)



# Part 1: Data organisation + visualisation ------------------------------------

## First, let's load the data files that we need for this exercise:

## A. Import the tree file (phylogeny):
mytree <- read.nexus("data/clean-frog-tree.nex")

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
View(mydata) # open a new tab in RStudio

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


## Take our trait of interest (i.e. eyesize) and log transform it:
logEye <- log(pull(mydata, eyesize))
names(logEye) <- mydata$Binomial # give these values names (i.e. species names)
head(logEye) # look at the first few rows



## Next, let's plot these trait data onto the phylogeny!
## The contMap() function in the R package 'phytools' projects the observed and 
##    reconstructed values of a continuous trait onto the edges of a tree using a 
##    color gradient. We will use the colour gradients from another R package, 
##    'viridis' to ensure the output is as readable as possible.


## Create "contMap" object using this log-transformed data:
frog_contMap <- contMap(mytree, logEye, 
                        plot=FALSE, res=200)

## Set up the colour gradient:
n <- length(frog_contMap$cols) # number of colour breaks
frog_contMap$cols[1:n] <- viridis(n) # using the gradient 'viridis' ( = yellow to purple)

## Plot the tree:
plot(frog_contMap, fsize = c(0.4, 1), outline = FALSE, lwd = c(3, 7), leg.txt = "Eye size (log)")


## _______________________________________________________________
## Q: Do you notice any particular trends across the phylogeny?
## Bonus Q: What other traits could you plot on a tree like this?
## _______________________________________________________________


