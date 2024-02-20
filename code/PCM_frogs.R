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
#   Tutorial 
# 
# ******************************************************


## Load packages:
library(tidyverse)
library(ape)
library(geiger)
library(phytools)
library(viridis)


# 01. Data preparation ----------------------------------------------------

## Load tree
frog_tree <- read.nexus("data/frog-tree.nex")
# Look at the tree summary
frog_tree
# Plot the tree as a circular/fan phylogeny with small labels
plot(frog_tree, cex = 0.2, typ = "fan", no.margin = TRUE)

## How many species? - 214
## How many internal nodes? - 213

# Look at the first 5 tip labels
frog_tree$tip.label[1:5]

# Check whether the tree is rooted - We want this to be TRUE
is.rooted(frog_tree)

# Check the tree is ultrametric - Most functions will assume this
is.ultrametric(frog_tree)


## Load data
frog_data <- read_csv("data/frog-eyes.csv")
glimpse(frog_data)
View(frog_data)

## What variables?

# * `Binomial` - the species binomial name.        
# * `Family` - the family the species belongs to.        
# * `Genus` - the genus the species belongs to.              
# * `tiplabel` - the name used for the species in the phylogeny.       
# * `Adult_habitat` - habitat of adults. Categories are: Ground-dwelling, Subfossorial, Scansorial (i.e. tree-dwelling), Semiaquatic, Aquatic, or Fossorial (i.e. burrowers).       
# * `Life_history` - whether the larvae are free-living (Free-living larvae) or not (No free-living larvae).     
# * `Sex_dichromatism` - whether different sexes are different colours (Present) or not (Absent).
# * `SVL` - snout vent length (in mm). This is a common way to measure body size in amphibians.            
# * `mass` - body mass (in g).
# * `rootmass` - cube root of the body mass.        
# * `eyesize` - eye size (in mm) for the species. This is an everage across left and right eyes from three individuals per species.


## Matching your data to your phylogeny
## Add underscores to data file
frog_data <- frog_data %>%
  mutate(Binomial = str_replace(string = Binomial, pattern = " ", replacement = "_"))
# Check it worked
head(frog_data$Binomial)


## Check whether the names match in the data and the tree
## We can use the `geiger` function `name.check`
check <- name.check(phy = frog_tree, data = frog_data, 
                    data.names = frog_data$Binomial)
check # open the object

## Matching the tree and the data
mytree <- drop.tip(frog_tree, check$tree_not_data) 
str(mytree) # check (tips and nodes)

## To remove species from the data which are not in the tree you can use `match` and `subset` as follows:
matches <- match(frog_data$tiplabel, check$data_not_tree, nomatch = 0)
mydata <- subset(frog_data, matches == 0)
glimpse(mydata) # check (rows)

## Check that everything matches
check_final <- name.check(phy = mytree, data = mydata, 
                    data.names = mydata$Binomial)
check_final # should say ok!


## Check data object class
class(mydata)
# Convert to a data frame
mydata <- as.data.frame(mydata)
# Check this is now a data frame
class(mydata)

# Write the cleaned data to a new file
write_csv(mydata, file = "data/clean-frog-data.csv")

# Write the cleaned tree to a new file
write.nexus(mytree, file = "data/clean-frog-tree.nex")




# 02. Phylogenetic signal -------------------------------------------------


## As is common in R, there are a number of ways to estimate Pagel’s λ and 
## Blomberg’s K. I’ve chosen to show  the way implemented in the package 
## "phytools" because it allows you to use the same function for both.

# Create logEye containing just log eye size length values
logEye <- log(pull(mydata, eyesize))
# Look at the first few rows
head(logEye)

# Give log Eye names = species names at the tips of the phylogeny
names(logEye) <- mydata$tiplabel
# Look at the first few rows
head(logEye)


### Pagel’s λ (lambda)

## Estimate lambda - using the function phylosig():
lambdaEye <- phylosig(mytree, logEye, method = "lambda", test = TRUE)
lambdaEye # take a look at the output

## The  λ estimate for log eye size is around 0.803. logL is the log-likelihood, 
## LR(lambda=0) is the log-likelihood for λ of 0, and P-value is the p value 
## from a likelihood ratio test testing whether  λ is significantly different 
## from 0 (no phylogenetic signal). Here P < 0.001. We interpret this as  λ
## being significantly different from 0, i.e. there is significant phylogenetic 
## signal in log eye size.


### Blomberg’s K - method = "k"
KEye <- phylosig(mytree, logEye, method = "K", test = TRUE, nsim = 1000)
KEye # take a look at the output


## K for log eye size is 0.283. The p value tells us how many times out of 
## 1000, a randomised value of K is more extreme than the observed value. 
## If this number is low, the p value is low (e.g. if 5 out of 1000 randomised 
## values of K are more extreme than the observed value p = 5/1000 = 0.005). 
## Here p = 0.001, suggesting that only 1 randomised value of K was more 
## extreme than the observed value. We interpret this as K being significantly 
## different from 0, i.e. there is significant phylogenetic signal in log eye size.


## What is λ for log gestation length? (GestationLen_d)
## What is K for log gestation length? (GestationLen_d)





# 03. Evolutionary model fitting analyses -------------------------------------

## Check the distribution of our data
p1 <- ggplot(mydata, aes(x = eyesize)) +
  geom_histogram(bins = 20, fill = "darkblue") +
  theme_bw(base_size = 14)
p1 

## and now with it log-tranformed:
p2 <- ggplot(mydata, aes(x = log(eyesize))) +
  geom_histogram(bins = 20, fill = "orangered") +
  theme_bw(base_size = 14)
p2

## Q: How would you describe this pattern?


#### Fitting the Brownian motion and OU models of evolution using `fitContinuous`

## We'll fit two commonly used evolutionary models to the data; the Brownian 
## motion (BM) model and the single peak Ornstein-Uhlenbeck (OU) model. 

## First we need to prepare the data for geiger:
# Create logHL containing just log eye size values
logES <- log(pull(mydata, eyesize))
# Look at the first few rows
head(logES)


# Check first few tip labels and species
mytree$tip.label[1:5]
mydata$Binomial[1:5]
# These are different so we reorder the data by the tips:
mydata <- mydata[match(mytree$tip.label, mydata$Binomial), ]
# Check this now matches the tip label order
mydata$Binomial[1:5]

# Give log head length names = species names at the tips of the phylogeny
names(logES) <- mydata$Binomial
# Look at the first few rows
head(logES)

## Fit the Brownian model:
BM <- fitContinuous(mytree, logES, model = c("BM"))
BM # check the output


## The maximum likelihood estimates of the model parameters are found near the 
## top of the output. In a Brownian motion (BM) model we estimate the Brownian 
## rate parameter, sigma^2 or 'sigsq' in the output above, which is `0.037` 
## and the value of the trait at the root of the tree, 'z0' in the 
## output above, which is 0.575704
## Other useful things in the output are the maximum-likelihood estimate of the 
## model (log-likelihood) and the Akaike Information Criterion (`AIC`), 
##  - we will return to the AIC values below.


## Fit an Ornstein-Uhlenbeck (OU) model
OU <- fitContinuous(mytree, logES, model = c("OU"))
OU # check the output


## Like for Brownian motion, the maximum likelihood estimates of the model 
## parameters are found near the top of the output. In an Ornstein-Uhlenbeck 
## (OU) model we estimate the Brownian rate parameter, sigma^2 or 'sigsq',
## the value of the trait at the root of the tree, 'z0' and the "rubber-band" 
## parameter, 'alpha' in the output above. 

## As alpha = 9.014 here, it suggests that there is evolution towards a particular head length..


## Compare the models using AIC
aic.scores <- setNames(c(BM$opt$aic, OU$opt$aic), c("BM","OU"))
aicw(aic.scores)




# 04. Mapping trait evolution on tree -----------------------------------------


## The contMap() function in the R package 'phytools' projects the observed and 
## reconstructed values of a continuous trait onto the edges of a tree using a 
## color gradient. We will use the colour gradients from another R package, 
## 'viridis' to ensure the output is as readable as possible.


## Import the cleaned data while assigning the first column (Binomial) to be the rownames
mydata <- read.csv("data/clean-frog-data.csv", row.names = 1)

## Extract character of interest (i.e. eyesize) and log transform it
lnEyesize <- log(setNames(mydata$eyesize,
                          rownames(mydata)))

## Create "contMap" object
frog_contMap <- contMap(mytree, lnEyesize, 
                        plot=FALSE, res=200)

## Set up the colour gradient
n <- length(frog_contMap$cols) # number of colour breaks
frog_contMap$cols[1:n] <- viridis(n) # using the gradient 'viridis' ( = yellow to purple)

## Plot the tree:
plot(frog_contMap, fsize = c(0.4, 1), outline = FALSE, lwd = c(3, 7), leg.txt = "Eye size (log)")


