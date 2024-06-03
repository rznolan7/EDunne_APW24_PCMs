# *********************************************************
#
#   Introduction to Phylogenetic Comparative Methods
#
#   Date:   04.06.2024
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
library(geiger)
library(ape)
library(phytools)
library(viridis)



# Part 1: Data organisation + visualisation ------------------------------------

## First, we need to load the data files that we need for this exercise:

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

## (You can see the full list of variables and their descriptions in the 00_frogs_cleaning.R script)


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




# Part 2: Phylogenetic signal --------------------------------------------------

## Let's check the phylogenetic signal of frog eye size using Pagel’s λ (lambda)

## Estimate Pagel’s λ (lambda) using the function phylosig() in the package 'phytools'
lambdaEye <- phylosig(mytree, logEye, method = "lambda", test = TRUE)
lambdaEye # take a look at the output

## Interpreting the output:
##    P-value is the p value from a likelihood ratio test testing whether  λ 
##    is significantly different from 0 (no phylogenetic signal). 
##    Here λ = 0.814 and P < 0.001. We can interpret this as  λ being significantly 
##    different from 0, i.e. there is significant phylogenetic signal in log eye size


##______________________________________
## Bonus exercise for independent work:
##  - What is λ for snout vent length?
##______________________________________




# Evolutionary model fitting analyses -------------------------------------

## Let's set up an evolutionary model-fitting analysis to characterize the 
##    evolutionary mode of frog eye size across the phylogeny

## We'll fit two commonly used evolutionary models to the data: 
##      (1) the Brownian motion (BM) model 
##      (2) the single peak Ornstein-Uhlenbeck (OU) model

## We will use the same log-transformed eye size data from above:
head(logEye) # Look at the first few rows

## Next, let's reorder the tree and trait data so that they match:
mydata <- mydata[match(mytree$tip.label, mydata$Binomial), ]

## Now let's fit our models!

## (1) Fit the Brownian model:
BM <- fitContinuous(mytree, logEye, model = c("BM"))
BM # check the output 

##________________________________________________________________
## Q: Can you see the model's 2 parameters (i.e. sigma^2 and z0)?
##________________________________________________________________


## (2) Fit the Ornstein-Uhlenbeck (OU) model
OU <- fitContinuous(mytree, logEye, model = c("OU"))
OU # check the output


## Now, let's compare the models using AIC 
## A lower 'fit' value indicates the preferred, or 'best' model:
AICscores <- setNames(c(BM$opt$aic, OU$opt$aic), c("BM","OU"))
aicw(AICscores)

## ___________________________________________________________
## Q: Which is the 'best' model?
## Q: What does this tell you about frog eye size evolution?
## ___________________________________________________________




# Bonus independent work --------------------------------------------------

## In the data folder there is another tree (primate-tree.nex) and trait 
##    dataset (primate-data.csv) for investigating the evolution of primate 
##    life-history variables, These data come from the PanTHERIA database 
##    (https://esajournals.onlinelibrary.wiley.com/doi/10.1890/08-1494.1) 
##    and 10kTrees (https://10ktrees.nunn-lab.org/)

## Take the code from above and modify it to examine the phylogenetic signal
##    of body mass evolution and/or gestation length in primates





