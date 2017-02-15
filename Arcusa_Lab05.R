## In this week's assignment, we'll *transition* seamlessly from probability to linear algebra, using what we've learnt about eigenvectors and solving systems of linear equations. You're gonna love it.

# We're going to set up and explore a Markov chain model describing changes in ocean beaches. One paper on this topic is:
# Mason and Hansom (1989) "A MARKOV MODEL FOR BEACH CHANGES ON THE HOLDERNESS COAST OF ENGLAND", EARTH SURFACE PROCESSES AND LANDFORMS, VOL. 14,731-743

## First, read the paper. It is fairly short and easy to understand (you don't need to know much about beaches, or geomorphology for that matter, to understand the main ideas and analyses).

## OK, now we can begin. Remember that the idea behind a Markov chain is that present state depends on some probability function of the immediately preceding state, but not on any state before that. The systems they describe are said to be 'memoryless' for this reason.

## Ocean beaches are very dynamic. The central idea in this paper is that, where t is time, the beach state at t=2, depends upon t=1, but not on t=0


#=============================================================
#=============================================================
## Table IIa in Mason and Hansom (1989) is the frequency transition matrix, between 7 beach 'states' ('M' through 'S') constructed using data collected April 1984 to September 1984 ('Summer').
## 1. Replicate this matrix and call it 'summer'

summer = matrix(c(10,4,8,NA,NA,2,NA, 3, 31, 8, NA, NA, NA, 3,6,5,48,6,2,5,3,1,NA,6,10,NA,2,NA,NA,NA,1,1,20,2,1,1,NA,10,1,NA,8,NA,1,3,1,NA,NA,1,1), nrow = 7, ncol =7, byrow = TRUE)
colnames(summer) = c("M", "N", "O", "P", "Q", "R", "S")
rownames(summer) = c("M", "N", "O", "P", "Q", "R", "S")

#=============================================================
#=============================================================
## 2. Turn this into a transition *probability* matrix by overwriting the variable 'summer'. Give it 'rownames' and 'colnames'

r.tot = rowSums(summer, na.rm = T)
summer = round(summer/r.tot, digits = 2)

#=============================================================
#=============================================================
## Table IV in Mason and Hansom (1989) is the transition probability matrix, between 9 beach 'states' ('M' through 'Y') in 'Winter'
## 3. Replicate this matrix and call it 'winter'. Give it 'rownames' and 'colnames'

winter = matrix(c(0.35,0.35,0.18,0.06,NA,0.06,NA,NA,NA,
                  0.21,0.40,0.12,0.03,NA,0.03,0.12,0.09,NA,
                  0.11,0.20,0.40,0.12,0.03,0.14,NA,NA,NA,
                  0.05,0.11,0.26,0.32,0.11,NA,NA,0.05,0.10,
                  NA,NA,0.08,0.15,0.54,NA,0.08,NA,0.15,
                  0.12,NA,0.38,NA,NA,0.13,0.12,NA,0.25,
                  NA,0.43,0.29,NA,0.14,NA,0.14, NA, NA,
                  NA, 0.25,0.08,0.08,0.09, NA, NA, 0.33,0.17,
                  NA,NA,NA,0.11,0.22,0.11,NA,0.45,0.11), nrow= 9, ncol = 9, byrow = T)
rownames(winter) = c("M", "N", "O", "P", "Q", "R", "S", "X", "Y")
colnames(winter) = c("M", "N", "O", "P", "Q", "R", "S", "X", "Y")


# (check for mistakes by ensuring all rows sum to 1)

r.tot = rowSums(winter, na.rm = T)

## This is probability. What does this have to do with linear systems? To find out, we're going to do some analyses that Mason and Hansom didn't do in their paper.

## Well, we know from last week the the states will approach a steady state. This system is in a state of dynamic equilibrium such that the net movement into and out of a given state is zero

## If p is the steady state vector, and T is the transition probability matrix, we can write

## Tp = p

## This is an eigenvalue equation of the form Ax = lambda x, with lambda = 1
## In our notation, we say Tp = lambda p
## lambda = 1 is the solution to the eigenvalue equation, and is therefore an eigenvalue of T

#=============================================================
#=============================================================
## 4. Compute the steady state probability vectors for a) summer, and b) winter from their respective matrices of transition probabilities. Do this by computing the left-eigenvectors of each matrix, and normalizing each left-eigenvector by its sum. Note that you may get both the real and imaginary parts but we are only interested in the absolute of the vectors (use the abs() command). Also note that Mason and Holmes did not do this analysis in their paper

library(expm)

#summer

sen = (summer %^% 15)[1,]
#summer[is.na(summer)] <- 0
#se = eigen(t(summer))
#abs(se$vectors)


#r.tot = rowSums(abs(se$vectors))
#sen = round(abs(se$vectors)/r.tot, digits = 2)
#colnames(sen) = c("M", "N", "O", "P", "Q", "R", "S")
#rownames(sen) = c("M", "N", "O", "P", "Q", "R", "S")

#winter

wen = (winter %^% 15)[1,]
#winter[is.na(winter)] <- 0
#we = eigen(t(winter))
#abs(we$vectors)

#r.tot = colSums(abs(we$vectors))
#wen = round(abs(we$vectors)/r.tot, digits = 2)
#rownames(wen) = c("M", "N", "O", "P", "Q", "R", "S", "X", "Y")
#colnames(wen) = c("M", "N", "O", "P", "Q", "R", "S", "X", "Y")

#=============================================================
# 5. Make a plot showing the steady state probability vectors for summer and winter as a function of state. This can be any style of plot that you choose.
# label the axes appropriately. 

library(ggplot2)
install.packages("reshape")
library(reshape)
install.packages("cowplot")
library(cowplot)

sen.m <- melt(sen)
wen.m <- melt(wen)

sen.plot <- ggplot(data = sen.m, aes(x =sen.m$X1, y = sen.m$X2 ))+
  geom_tile(aes(fill = sen.m$value), colour= "white")+
  scale_fill_gradient(low= "white", high = "steelblue", name = "Probability")+
  labs(x = "State", y = "State")+
  ggtitle("Summer")

sen.plot

wen.plot <- ggplot(data = wen.m, aes(x =wen.m$X1, y = wen.m$X2 ))+
  geom_tile(aes(fill = wen.m$value), colour= "white")+
  scale_fill_gradient(low= "white", high = "steelblue", name = "Probability")+
  labs(x = "State", y = "State")+
  ggtitle("Winter steady state probability")

wen.plot

plot_grid(wen.plot, sen.plot, labels=c("A", "B"), ncol = 2, nrow = 1)

#=============================================================
#=============================================================
#6. Recall that these vectors show the long term probability of being in each state irrespective of what state the system started in. The state associated with the largest probability is the most 'dominant' for that system. What is the dominant state for summer in steady state? 
# Note, I want you to show this using R, so that involves indexing a vector (hint: you will need to retrieve an element of one vector corresponding to the max of another vector)

dominant = which(sen == max(sen), arr.ind = TRUE)
dominant
# State o --> M

#=============================================================
#=============================================================
#7. There are 2 dominant states in winter - what are they?  (hint: you will need to retrieve an element of one vector corresponding to the biggest, and second biggest, of another vector. Another hint: you may find the command "order" useful). 

dominant = wen.m[order(-wen.m$value, wen.m$X1, wen.m$X2),]
dominant.2 = dominant[c(1,2),]
dominant.2
# P --> Q and M --> R are the two dominant states

#=============================================================
## (optional, non-graded, bonus, non-R, question). Look at those dominant states in Figure 3 of Mason and Hansom (1989). Can you offer a physical reason for the difference in dominant states in summer and winter?

#Possibly related to winter storm erosion.

## Table 1 of Mason and Hansom (1989) shows the characteristics of the different beach states, encoded in a 5-element vector
## We'll just look at the mean values of V1, V2, V3, V4 and V5 (metrics computed from beach profiles) for states M -- Y, and ignore the standard deviations

##From Table 1 ...
##M (V1 -- V5)
M = c(0.9928, 0.7118, 1.5434, 0.0975, 0.0173)

##N (V1 -- V5)
N = c(0.9725, 1.3727, 0.7829, 0.0932, 0.0069)

##O (V1 -- V5)
O = c(0.9717, 1.0016, 0.9980, 0.0811, 0.0213)

##P (V1 -- V5)
P = c(0.7487, 0.9853, 0.7703, 0.1039, 0.0245)

##Q (V1 -- V5)
Q = c(0.8542, 1.0007, 0.8664, 0.0467, 0.0272)

##R (V1 -- V5)
R = c(1.0266, 0.9892, 1.0492, 0.0851, 0.0355)

##X (V1 -- V5)
X = c(0.7690, 1.4360, 0.5450, 0.0729, 0.0233)

##Y (V1 -- V5)
Y = c(0.0501, 1.0020, 0.5000, 0.0490, 0.0385)

# Note that state 'S' is not included because that was the 'miscellaneous' state (Mason and Hansom 1989, p. 734)

#You have just been to the beach and worked up your beach elevation profile data. You have calculated V1, V2, V3, V4, and V5 ("beach profile metrics") from the data:

Sample = c(0.7982, 1.0624, 0.8819, 0.0786, 0.0243)

#=============================================================
#=============================================================
#7. Construct a data frame and use ggplot to make a line plot of 
## M, N, O, P, Q, R, X, Y, and your "sample", as a function of v (1 through 5) (all on 1 graph)
## give them different colours and/or symbols to distinguish them, and a legend. Make your 'sample' line stand out (different size, or something)

char = rbind(M, N, O, P, Q, R, X, Y, Sample)
char.m = melt(char)
colnames(char.m)= c("State", "Metric", "Value")

profile = ggplot(data = char.m, aes(x = char.m$Metric, y = char.m$Value, colour = State))+
  geom_line(aes(size = State))+
  labs(x = "Beach profile metrics", y = "Mean")+
  ggtitle("Beach profile characteristics")+
  scale_size_manual(values = c(1,1,1,1,1,1,2,1,1))
  
profile

#=============================================================
##=============================================================
##8. Set up and solve a system of linear equations that estimates the state that best describes the current state of the beach. 
## Note 1: it is winter right now, so you'll need all 8 states
## Note 2: this is an OVER-DETERMINED system (there are more equations than unknowns)
## Note 3: The solution (x) to Ax = b will be a vector of length 8 representing the proportion of each of the 8 states. I want to know, what one state 'best describes' the current state. 


