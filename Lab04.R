
## PROBABILITY

#####################
##lab 04 - PART 1: transition probabilities

## Weather in the Land of Nod is very strange place, mostly because of the weather. You see, in the Land of Nod there are never two sunny days in a row. If it rains today, there is a 50% chance it will rain tomorrow. Likewise for snow. Right now, it is raining, so the probability of it snowing tomorrow is 25%. If it is snowing now, the probability that it is sunny tomorrow is 25%. But if it is sunny now, the probability of it being snowing or raining tomorrow is 50%!

##2a. Create the 1-step transition probability matrix for the weather in the Land of Nod, in terms of the 3 states, 'rain', 'sunny', and 'snow'. Give the matrix column and row names.
s.r = 0.5
s.sn = 0.5
s.s = 0
r.s = 0.25
r.r = 0.5
r.sn = 0.25
sn.s = 0.25
sn.sn = 0.5
sn.r = 0.25

p = matrix (c(s.s,r.s,sn.s,s.r,r.r,sn.r,s.sn,r.sn,sn.sn), nrow=3, ncol=3)
colnames(p) = c("To Sunny", "To Rainy", "To Snowy")
rownames(p) = c("From Sunny", "From Rainy", "From Snowy")

##2b. Given that it is raining today, using a markov chain model, what is the probability of it snowing in exactly 3 days?

library(expm)

p%^%3
#probability of it snowing in 3 days is 0.390625

##2c. Given that it is sunny today, show that the long-range forecast is for sun, snow or rain with equal likelihood

p%^%14
p%^%20

#probabibility for snow or rain in the long term remains constant at 40%, while the probability for sun remains at 20% 

#####################
##lab 04 - PART 2: binomial theory

## The State of California has spent $45 million over 15 years to control water hyacinth (Eichhornia crassipes) in the Sacramento-San Joaquin Delta. 

## You are sampling for aquatic plants in the Sacramento-San Joaquin Delta. Previous studies have suggested that during this time of year, 2% of your samples will be water hyacinth. You realize that you can use the binomial theorem to figure out the probability of sampling m plants in n trials.

##1a. You have just sampled 100 plants. Write a function to calculate the probability of your sample containing exactly 3 water hyacinth plants?

h_proba = function(p=0.02, m=3, n=100){
  Cmn = choose(n, m)
  return(print(Cmn*(p^m) * ((1-p)^(n-m))))
}

h_proba(p=0.02, m =3, n=100)


##1b. Write a function to calculate the probability of your sample containing 10 *or fewer* water hyacinth plants?

h_proba2 = function(p = 0.02, m= 0:10, n=100){
  Cmn = choose(n, m)
  proba= (Cmn*(p^m) * ((1-p)^(n-m)))
  return(sum(proba))
}

h_proba2(p = 0.02, m= 0:10, n=100)

##1c. You intend to sample 100 water hyacinth plants today. Your over sample rate (of all plants) is 500 plants per hour. If you start at 9am, use the function you wrote above (for 1b) in a for-loop to show what time will you likely have collected 100 water hyacinths and be able to go home? 



#####################
##lab 04 - PART 3: confidence intervals, abd return intervals (using Poisson statistics)

## We're going to use Colorado River data like we used in class
## first lets call the dataRetrieval package

library(dataRetrieval)

# we'll use a 10-year record of Lees Ferry daily mean water temperature

siteNo = "09380000"
pCode = "00010"
start.date = "2007-01-01"
end.date = "2017-01-01"

lf = readNWISdata(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date,
                     service = "dv")

lf = renameNWISColumns(lf)

# let's see what the data look like
head(lf)

##3a. Using ggplot, make a plot of dateTime versus water temperature


##3b. Compute the 99% upper and lower confidence intervals for this water temperature data. Complete the sentence: "The true mean water temperature has a probability of 99% of being in the interval between ____________ and ______________ degrees Celsius"


##3c. What is the probability of one day per year where the temperature is above 12.5 deg C?
## Note that your lambda is going to be the average number of daily temperature readings above 12.5 deg C per year 
## We only want to know the likelihood that 1 day per year is above 12.5 C, so k = 1


##3d. How does this probability change for k = 1 to 20 days per year, inclusive? Show this graphically as a scatter or line plot(hint: You'll probably need to write a function for this)


## Next we'll see how the probability that water temperature is above 12.5 deg C on k=1 day per year changes as a function of time (length of data)

##3e. Compute the probability that water temperature is above 12.5 deg C on k=1 day per year for data sets 1 to 20 years in length, in increments of 1 year


##3f. graphically show the differences the probabilities computed in 3d as a function of data length (1 to 20 years). What do you notice about the trend in time?
















