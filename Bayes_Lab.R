## Week 10: Bayesian Analysis

#=================================================
## PART 1 :  ESTIMATE A PROPORTION
# What's the proportion of people who like pizza? It's got to be pretty high, hasn't it?  
# You have a rough idea that the most likely value is around 0.85,
# but you also think that the proportion is unlikely to be smaller than 0.60, or bigger than 0.95.

# 1.1. You're a Bayesianista, so you'll want to find the best "Beta prior" to use by specifying that 
# the median (50% percentile) of the prior is 0.85, 
# and the 0.001% percentile is 0.60.
# Make lists for these quantiles (percentile and proportion)

quantile1 = list(p=.5,x=.85)
quantile2 = list(p=.00001,x=.6)

#1.2. Now, load the LearnBayes package, and use beta.select() to find the best "Beta prior"
#     for the proportion of all people who like pizza

#library(LearnBayes)

beta.select(quantile1,quantile2)

#1.3. Plot the prior density by using the "curve" function

a  = 52.22
b = 9.49 

curve(dbeta(x,a,b),from=0, to=1, lty=3,lwd=4, col="red", xlab="p",ylab="Pr. Density")

# You want to refine your estimate the proportion of people who like pizza with some hard data,
# so you've carried out a survey of 60 people, and found that 50 say that they like pizza

# Since you're a budding Bayesian, you can compute the posterior probability density function, 
# which tells you how likely the possible values of the proportion are, given the observed data

#1.4. Create a function that computes the beta prior and posterior probability density functions and plots a curve for each

Bayes = function(s, f){
  curve(dbeta(x,a+s,b+f), from=0, to=1, xlab="p",ylab="Pr. Density",lty=1,lwd=4)
  curve(dbeta(x,a,b),add=TRUE,lty=3,lwd=4, col="red")
  legend(.5,4,c("Posterior (updated with data)","Prior (based on belief)"), lty=c(3,2),lwd=c(3,3), col=c("black","red"))
}

#1.5. Use this function to plot the prior and posterior probability density functions for your survey results

Bayes(s = 50, f = 10)

#=================================================
## PART 2 : ESTIMATE A PROPORTION AND PREDICT THE FUTURE
# A study has reported on the effects of exposure to low levels of Arsenic in the amphipod, Gammarus elvirae
# Researchers analyzed DNA for damage related to exposure to Arsenic
# Of the organisms studied that had a concentration greater than 1.55 mg/L, 32 survived and 17 did not 

# 2.1. Suppose your prior density for p, the proportion of all such organisms who will survive, is beta(1, 1), 
# and so your posterior density is beta(33, 18).
# Use the function qbeta to find a 90% interval estimate for prior density, p.

qbeta(p = 0.9, 1,1)

# 2.2 Use the function pbeta to find the probability that p exceeds .66

1 - pbeta(q = 0.66, 1, 1)

# 2.3 Use the function rbeta to take a simulated sample of size 1000 from
# the posterior distribution of p.

pprime = rbeta(1000, 33, 18)

# 2.4. Congratulations! You've been awarded a grant by the Ecotoxicology Research Council to continue the study. 
#      You measure 21 more organisms with an Arsenic concentration greater than 1.55 mg/L
#      Find the predictive probability that exactly ten of them will survive
#      (Hint: use your simulated sample from part 2.3 and the rbinom function 
#       to take a simulated sample from the predictive distribution.)

Ntrials = 21
y = rbinom(1000, Ntrials, pprime)
freq = table(y)

predprob = freq/sum(freq)

predprob["10"]

# 2.5. Make a plot of proportion or organisms versus predictive distribution for survival probability

m = 21
ys = as.integer(names(freq))

plot(ys/m, predprob, type = "l", ylab="f(y)", xlab="proportion of organisms")

#=================================================
## PART 3 : ESTIMATE THE MEAN FROM NORMALLY DISTRIBUTED DATA, WITH A DISCRETE PRIOR
# You live in the Kipengere range in Tanzania 
# and you're interested in estimating the mean total rainfall per year (in mm).
# Individual yearly rain totals y1, ..., yn are collected from a population
# that is assumed to be normally distributed with mean MU

# Before collecting any data, you think that the mean rainfall MU
# can be the values 635,  889, 1143, 1397, 1651, 1905 mm with the following probabilities: .05, .15, .25, .3, .15, .1
# 3.1. Create vectors for MU and Pr(MU)

MU = c(635,  889, 1143, 1397, 1651, 1905)
Pr.MU = c(.05, .15, .25, .3, .15, .1)

# You observe the yearly rainfall totals of
# 980, 1076, 1460, 1028, 1313, 1704,  848, 1546, 1628, 1018, 1033, and 162 mm. 
# 3.2. Enter these data into a vector y and compute, ybar, the sample mean.

y = c(980, 1076, 1460, 1028, 1313, 1704,  848, 1546, 1628, 1018, 1033, 162)
ybar = mean(y)

# The likelihood function, L(MU) is given by
# exp(-(n/2 sigma^2) * (MU - ybar)^2 )
# where ybar is the sample mean, sigma^2 is the sample variance, and n is the sample size 
# This is your likelihood of N means 

# 3.3. Compute the likelihood on the list of values in mu and place the likelihood values in a vector called L

n = length(y)
ybar = mean(y)
sigma = sd(MU)
L = exp(-(n/(2 * sigma^2)) * (MU - ybar)^2)

# One can compute the posterior probabilities for MU using the following formula:
# posterior = (prior*like)/sum(prior*like)
# 3.4. Compute the posterior probabilities of MU for this example.

posterior = (MU*L)/sum(MU*L)

# 3.5. Find a 75% probability interval for MU (hint: use the function discint)

dist = cbind(MU, posterior)
discint(dist, .75)
#interval is between 1143 to 1397 mm

##=================================================
## PART 4 : ESTIMATE MEAN AND VARIANCE OF NORMALLY DISTRIBUTED DATA
# We are (still) interested in learning about the sleeping habits of students at NAU. 
#           You collect y1 through yn, the sleeping times (in hours) for n = 15 randomly selected students 
#           in an Earth and Environmental Data Analysis course. 
#           Here are the observations:

sleepytime = c(9.0, 8.5, 7.0, 8.5, 6.0, 12.5, 6.0, 9.0, 8.5, 7.5, 8.0, 6.0, 9.0, 8.0, 6.0, 7.0)

# You asssume that these observations represent a random sample from a
# normal population with mean MU and variance SIGMA^2
# Placing the prior on (MU, SIGMA^2), simulate a sample of 1000 draws
# from the joint posterior distribution.

# let's break this problem down:
# 4.1. First, you'll need to compute the sum of squares

SS = sum((sleepytime - mean(sleepytime))^2)

# 4.2. Next, compute SIGMA^2, which is a 1000 random draw computed as the sum of squares divided by draws from the 
# chi-square distribution with n-1 degrees of freedom

var = SS/ rchisq(1000, df = length(sleepytime)-1)

# 4.3. Now compute MU, which is a 1000 random draw from a Normal(mean, sd) distribution

MU = rnorm(1000, mean(sleepytime), sd(sleepytime))

# 4.4. Use the simulated sample to find the 90% interval estimates for the mean
#      MU and the standard deviation SIGMA.
#      Complete the sentence: A 90% credible interval for the mean sleep is (XX, XX) hours
#      Complete the sentence: A 90% credible interval for the the standard deviation in sleep is (XX, XX) hours
#      (hint: we're looking for a vector of quantiles of MU and SIGMA)
#      (hint 2: you can do this the regular frequentist way, or use the normpostsim function from the LearnBayes library)

quantile(MU, probs = c(0.1, 0.9))
quantile(sqrt(var), probs = c(0.1, 0.9))

#A 90% credible interval for the mean sleep is (5:42, 10:03) hours
#A 90% credible interval for the the standard deviation in sleep is (1:23, 2:15) hours

# 4.5. You are interested in estimating the 4th quartile (p75) of the normal population of sleep times. 
# Using the fact that p75 = MU + (0.674 * SIGMA), find the posterior mean and posterior standard deviation of p75.

p75 = MU + (0.674 * sqrt(var))

mean = mean(p75) #maybe the answer?
sigma = sd(p75) #answer?

n = length(p75)
ybar = mean(p75)
sigma = sd(p75)
L = exp(-(n/(2 * sigma^2)) * (MU - ybar)^2)

posterior.MU = (MU*L)/sum(MU*L)
posterior.sd = (sqrt(var)*L)/sum(sqrt(var)*L)

dist.MU = cbind(MU, posterior)
dist.sd = cbind(sqrt(var), posterior)
discint(dist.MU, .75) #distribution?
discint(dist.sd, .75) #distribution?
