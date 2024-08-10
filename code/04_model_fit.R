# *********************************************************
#
#   Introduction to Phylogenetic Comparative Methods
#
#   Emma Dunne (emma.dunne@fau.de)
#   Late updated: August 2024
#
# _________________________________________________________
#
#   4. Evolutionary model fitting analyses
# 
# *********************************************************


## Let's set up an evolutionary model-fitting analysis to characterize the 
##    evolutionary mode of frog eye size across the phylogeny

## We'll fit two commonly used evolutionary models to the data: 
##      (1) the Brownian motion (BM) model 
##      (2) the single peak Ornstein-Uhlenbeck (OU) model

## We will use the same log-transformed eye size data from above:
head(logTrait) # Look at the first few rows

## Next, let's reorder the tree and trait data so that they match:
mydata <- mydata[match(mytree$tip.label, mydata$Binomial), ]

## Now let's fit our models!

## (1) Fit the Brownian model:
BM <- fitContinuous(mytree, logTrait, model = c("BM"))
BM # check the output 

##________________________________________________________________
## Q: Can you see the model's 2 parameters (i.e. sigma^2 and z0)?
##________________________________________________________________


## (2) Fit the Ornstein-Uhlenbeck (OU) model
OU <- fitContinuous(mytree, logTrait, model = c("OU"))
OU # check the output


## Now, let's compare the models using AIC 
## A lower 'fit' value indicates the preferred, or 'best' model:
AICscores <- setNames(c(BM$opt$aic, OU$opt$aic), c("BM","OU"))
aicw(AICscores)

# GEIGER-fitted comparative model of continuous data
# fitted ‘OU’ model parameters:
#   alpha = 1.268335                  ##the alpha is positive and therefore the optimum is nonzero
# sigsq = 0.657174
# z0 = 1.635270
# 
# model summary:
#   log-likelihood = -106.224403
# AIC = 218.448807
# AICc = 218.576466
# free parameters = 3
# 
# Convergence diagnostics:
#   optimization iterations = 100
# failed iterations = 0
# number of iterations with same best fit = 41
# frequency of best fit = 0.410
# 
# object summary:
#   'lik' -- likelihood function
# 'bnd' -- bounds for likelihood search
# 'res' -- optimization iteration summary
# 'opt' -- maximum likelihood parameter estimates

  ## ___________________________________________________________
  ## Q: Which is the 'best' model?
  ## Q: What does this tell you about frog eye size evolution?
  ## ___________________________________________________________