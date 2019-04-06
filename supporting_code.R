#####################################################################################
# Computing information criteria for multilevel models using different sample sizes #
# Wendy Christensen, University of California, Los Angeles                          #
# SatRday Los Angeles, 2019 - Lightning talk supporting code                        #
#####################################################################################

library(nlme)

# Read data and attach dataset

alcohol1 <- read.table("https://stats.idre.ucla.edu/stat/r/examples/alda/data/alcohol1_pp.txt", header=T, sep=",")
attach(alcohol1)

# Three parameter model from Singer & Willett (2003) "Applied Longitudinal Data Analysis" (Chapter 4, p. 94-95)
# Model specification in nlme from https://stats.idre.ucla.edu/r/examples/alda/r-applied-longitudinal-data-analysis-ch-4/

model <- lme(alcuse~ 1, alcohol1, random= ~1 |id, method="ML")

# Print output

summary(model)

# Examining the structure of the model's output to find necessary elements (loglikelihood, sample sizes)

names(model)
str(model)

# Isolating model log likelihood from output to compute model deviance

logLikelihood <- model$logLik
deviance <- -2*logLikelihood

# Isolating the number of clusters (m) and total number of observations (N) from output

groups <- head(model$dims$ngrps,n=1)
m <- unname(groups, force = FALSE)

N <- model$dims$N

# Computing BIC using m (number of clusters) and N for 3 parameters

BIC_m <- deviance+log(m)*3

BIC_N <- deviance+log(N)*3 # This matches the nlme output!