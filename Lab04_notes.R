shell.exec(link) # open web browser through R

## Types of probabilities

##1. simple probabilities
##2. conditional probabilities
##3. transitional probabilities (not common but useful in E sci)

#1 Simple proba
x = sample(c("tail","head"), size=10000, replace=TRUE) #sampling with replacement
est_prob = sum(x=="head")/length(x) #vector of trues and falses
est_prob

library(ggplot2)

cum_prob = cumsum(x=="head")/ (1:length(x)) #counting the number of trues
qplot(1:length(x), cum_prob , geom='line', ylim=c(0,1), 
      xlab="Number of experiments", ylab)

#law of large numbers represented in the graph


#2 conditional probability
P.R = 0.8 #probab of rain
P.LR = 0.25 #probab of lightning given rain

P.LandR = P.R * P.LR

#3 dependent events

P.S = 0.8 #proba of snow tomorrow
P.C = 0.4 #proba of closing the road given the proba of snow
P.Closed4MoreThan2Hrs = 0.8 #probab of the road being closed for longer than 2 hrs if the road is closed if it snows

Pcond3Events = P.S * P.C * P.Closed4MoreThan2Hrs


#3 ransition probabilities
#found in e sys sci models
#proba of going from one thing/state to another, particularly if the sys has discreet states

#we're in a classroom where students can be in 1 of 2 states, either 'bored' or 'alert'
# P(bored) = 0.2
# P(alert) = 0.25

Nalert = 6 #0.4
Nbored = 9 #0.6

#given the probabilities above, how many alert students, and how many bored students, will there be? (in the next time step)

# how do you solve this problem using linear algebra?

init_prop = c(Nalert/15, Nbored/15) #initial proportions when t=1

#t = t+1

#create a transition proba matrix

p = t(matrix(c(1-0.2, 0.2, 1-0.25, 0.25), nrow = 2, ncol=2)) #2x2 transi proba matrix
p = matrix(c(1-0.2, 0.2, 0.25, 1-0.25), nrow = 2, ncol=2)

# long handed way of solving for how many bored and alert students we have in the next time step

ab = (6/15) * 0.2
ba = (9/15) * 0.25

#ab = init_prop[1]* 0.2
#ba = init_prop[2]* 0.25

aa = (6/15) * (1-.2)
bb = (9/15) * (1-.25)

cat("number of bored = ", 15*(ab + bb))

cat("number of alert = ", 15*(aa + ba))

tplus1_prop = init_prop %*% p
number_of_bored_and_alert_students_now = 15*tplus1_prop

## Markov chain

#You're doing sequence stratigraphy and you're counting the transitions between rock strata
#3 rock groups (sandstone, shale, limestone)

p = matrix(c(70, 20, 12, 26, 50, 32, 50, 25, 50), byrow=TRUE, nrow = 3)

rownames(p) = c("sandstone", "shale", "limestone")
colnames(p) = rownames(p)

p

rowSums(p)

p = p / rowSums(p)
p

#given that we're in a sandstone layer, what is the prob. that we'll be back in a sandstone layer in exactly 2 strata

# sand -> sandstone
a = p[1,1] * p[1,1]

# shale -> sandstone
b = p[2,1] * p[1,2]

#limestone -> sandstone
c = p[3,1] * p[1,3]

# prob. sandstone in 2 trips
a + b + c

p %*% p

#element [1,1]

#http://setosa.io/ev/markov-chains


# install libaray for matrix pow 

install.packages("expm")
library(expm)

# given that we are in a shale layer, what is the probability of being in a limestone in exactly 3 strata?

p3 = p %^%3
#liklelihood to limestone
p3[2,3]

# transition matrix, p^n, will converge as n increases

p %^% 20

# after a lot of time steps, when the starting location is completely irrelevant

# we call this the stationary distribution

## probability distributions

## binomial probability
#the probability of getting *M* successes in *N* trials

p = 0.5 #probab of possibility
m = c(0,1,2,3,4,5) #possibilities
n = 5 #number of tries

Cmn = factorial(n) / (factorial(m) * factorial(n-m))

pr = data.frame(outcome=m, binom_prob = Cmn * (p^m)*((1-p)^(n-m)))
print(pr)

#short cuts available to calculate this:

binom_prob = dbinom(m, size=5, prob=0.5)

out = data.frame(outcome=m, binom_prob = binom_prob )

help("Binomial")

## Yahtzee!!

# you have 5 die. What is the probability of rolling of 1 six?

x = sample(c("1","2","3","4","5","6"), size=5, replace=TRUE)

print(x)
sum(x=='6')

no_of_experiments = 50000
no_of_events = rep(0, no_of_experiments) # initializing an output vector, i.e. creating an empty vector to contain the output, same as no_of_events = c()

for(i in 1:no_of_experiments){
  x = sample(c('1','2','3','4','5','6'), size=5, replace=TRUE)
  no_of_events[i] = sum(x=='6')
}
  
prob_of_6 = mean(no_of_events==1)

prob_of_6

#probability of rolling 5 sixes?

rel_freq = table(no_of_events)/length(no_of_events)
barplot(rel_freq, ylab="Probability", xlabel="Number of sixes")

# get at the same answer theoretically
p = 1/6 #chances of rolling a specific 'pip'
q = 1-p # chances of failing

m = c(0,1,2,3,4,5) #number of 6's that can be rolled
n = 5

Cmn = choose(n,m)


binom_prob = Cmn*(p^m) * ((1-p)^(n-m))

print(binom_prob * 100)

#comparing to the other experiment method, should be close to the same
print(rel_freq * 100)

## dnorm = theoretical p.d.f
x = seq(-20, 20, by=.1)
y = dnorm(x, mean=2.5, sd=1)

plot(x,y)

## rnorm = random numbers from a normal distribution 
data = rnorm(10000, mean = 80, sd = 5)

hist(data, nclass=20, col="blue") #nclass is the number of bins

qqnorm(data)
qqline(data)

#
mean = 25
data = rexp(1000, rate=1/mean)

qqnorm(data)
qqline(data)








