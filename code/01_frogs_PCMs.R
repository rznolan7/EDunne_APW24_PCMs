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
#   Tutorial using data modified from Thomas et al. (2020)
#       (https://doi.org/10.1098/rspb.2020.1393)
# 
# *********************************************************


## Load packages:
library(tidyverse)
library(ape)
library(geiger)
library(phytools)
library(viridis)



# Data preparation --------------------------------------------------------

## First, we need to load the data that was cleaned in script 00_frogs_cleaning.R

## Tree file:
mytree <- read.nexus("data/clean-frog-tree.nex")

## Plot the tree as a circular/fan phylogeny with small labels
plot(mytree, cex = 0.2, typ = "fan", no.margin = TRUE)

## Look at the tree summary:
mytree

## Q: How many frog species are on the tree?


## Load the trait data:
mydata <- read_csv("data/clean-frog-data.csv")

## Convert to a data frame
mydata <- as.data.frame(mydata)
class(mydata) # check

## Take a look at the variables
glimpse(mydata) # columns in a list
View(mydata) # open a new tab in RStudio

## The key variables and brief explanations:
#   - Binomial - the species binomial name        
#   - Family - the family the species belongs to        
#   - eyesize- eye size (in mm) for the species. This is an average across left and right eyes from three individuals per species
# (You can see the full list of variables in the 00_frogs_cleaning.R script)




# Evolutionary model fitting analyses -------------------------------------

## Let's set up an evolutionary model-fitting analysis to characterize the 
## evolutionary mode of frog eye size across the phylogeny

## We'll fit two commonly used evolutionary models to the data: 
##      (1) the Brownian motion (BM) model 
##      (2) the single peak Ornstein-Uhlenbeck (OU) model

## First we need to prepare the data for the R package 'geiger'
## Create logES containing just log eye size values:
logES <- log(pull(mydata, eyesize))
## Give these data names (i.e. the species names at the tips of the phylogeny)
names(logES) <- mydata$Binomial
## Look at the first few rows
head(logES)

## Next, let's reorder the tree and trait data so that they match:
mydata <- mydata[match(mytree$tip.label, mydata$Binomial), ]

## Now let's fit our models!

## (1) Fit the Brownian model:
BM <- fitContinuous(mytree, logES, model = c("BM"))
BM # check the output 

## Q: Can you see the 2 parameters, sigma^2 and z0?


## (2) Fit the Ornstein-Uhlenbeck (OU) model
OU <- fitContinuous(mytree, logES, model = c("OU"))
OU # check the output


## Now, let's compare the models using AIC (lower values = preferred model)
AICscores <- setNames(c(BM$opt$aic, OU$opt$aic), c("BM","OU"))
aicw(AICscores)

## Q: Which is the preferred model?
## Q: What does this tell you about frog eye size evolution?




# Mapping trait evolution on tree -----------------------------------------

## This bonus exercise makes a pretty tree showing frog eye size!

## The contMap() function in the R package 'phytools' projects the observed and 
## reconstructed values of a continuous trait onto the edges of a tree using a 
## color gradient. We will use the colour gradients from another R package, 
## 'viridis' to ensure the output is as readable as possible.


## Load the cleaned data again (in case it has been tinkered with above!),
##  while assigning the first column (Binomial) to be the rownames:
mydata <- read.csv("data/clean-frog-data.csv", row.names = 1)

## Extract character of interest (i.e. eyesize) and log transform it
lnEyesize <- log(setNames(mydata$eyesize, rownames(mydata)))

## Create "contMap" object
frog_contMap <- contMap(mytree, lnEyesize, 
                        plot=FALSE, res=200)

## Set up the colour gradient
n <- length(frog_contMap$cols) # number of colour breaks
frog_contMap$cols[1:n] <- viridis(n) # using the gradient 'viridis' ( = yellow to purple)

## Plot the tree:
plot(frog_contMap, fsize = c(0.4, 1), outline = FALSE, lwd = c(3, 7), leg.txt = "Eye size (log)")

## Q: Do you notice any particular trends across the phylogeny?
## Q: What other traits could you plot on a tree like this?

