#Earth and Environmental Data Analysis
#Lab 07

#Regression.

#Let's go back to our old friend the faithful dataset.
#1. Use ggplot to create a scatter plot between eruptions and waittimes. Calculate the correlation of these two timeseries, and their p-value (after adjusting N for autocorrelation). Add the values for r and p to the plot using geom_text.

library(ggplot2)

mydata= faithful

AR1 = function(x, lag = 1){
  auto.corr = c()
  for(i in lag){
    lagged1 = x[(1+i): length(x)]
    lagged2 = x[1:(length(x)-i)]
    auto.corr[i] <- round(cor(lagged1, lagged2),3)
  }
  return(auto.corr)
}

correlation = function(x, y){
  out =list()
  a = x - mean(x)
  b = y - mean(y)
  cov = t(a)%*%b / (length(b)-1)
  rho = cov/sqrt(var(x)*var(y))
  Tstat = rho * sqrt((length(x)-2)/(1-rho^2))
  N = length(x)
  AR1x = AR1(x = x)
  AR1y = AR1(x = y)
  EffN = N * (1 - AR1x*AR1y)/(1+AR1x*AR1y)
  p.value = pt(-abs(Tstat),df=EffN)*2
  out$R = rho
  out$T = Tstat
  out$P = p.value
  out$Effn = EffN
  print(out)
}

mydata.cor = correlation(x = mydata$eruptions, y = mydata$waiting)

myplot = ggplot(data = mydata)+
  geom_point(aes(x = mydata$waiting, y = mydata$eruptions))+
  labs(x = "Wait times", y = "Eruption duration", title = "Correlation")+
  geom_text(aes(x = 90, y = 2, label = "r =0.9008112 
                p = 8.369302e-72"))

myplot

#It looks like waittimes might be a good predictor for the duration of the upcoming eruption. Your boss asks you to calculate how long the upcoming eruption will last based on the current wait time. All our hard work last week on correlation does not help us with this prediction. We need a MODEL; and will start with a simple linear model. To do this - ordinary linear regression!

#2a. Create a function that uses linear algebra to create a model of Y = X*b, that has two inputs - your design matrix X, and your predictand Y, that returns b

B = function(X = Xd, Y = Y){
  XX=t(X)%*%Xd
  XY=t(X)%*%Y
  B=solve(XX)%*%XY
  return(B)
}

#2b. Use your function to calculate the model where your design matrix (X) is only the waittimes (a 272x1 matrix). What does b mean in this case? Calculate y-hat over the range of geyser waitimes and add that line to your graph. How well does this model fit the data?

X = mydata$waiting
Y = mydata$eruptions
Xd=c(X)

b = B(Xd, Y) # b is the slope 

yhat=Xd%*%b

new.plot = myplot + geom_line(aes(X,yhat))
new.plot

residuals = Y-yhat

res.plot = ggplot()+
  geom_point(aes(X,residuals))+
  labs(x = "Waiting time", y = "Residuals")

res.plot #the residuals appear to cluster into two groups

his.plot = ggplot()+geom_histogram(aes(residuals))
his.plot #the residuals might be bimodal which would suggest to me that the model is not fitting the data very well

#2c. Calculate another model, this time adding a column of ones to your design matrix to allow the derivation of a y=mx+b type model (with an intercept). Add this to your plot too - with a new color and a good legend. Is this an improvement?

ones=matrix(1,nrow=length(X))
length(X)
Xd=cbind(ones,X)
b = B(Xd, Y)
yhat2=Xd%*%b

new.new.plot = new.plot + geom_line(aes(X,yhat2, colour = "with intercept 
in the calculation"))+
  theme(legend.title=element_blank())

new.new.plot

residuals = Y-yhat2

res.plot = ggplot()+
  geom_point(aes(X,residuals))+
  labs(x = "Waiting time", y = "Residuals")

res.plot 

his.plot = ggplot()+geom_histogram(aes(residuals))
his.plot #it is an improvement as the clusters have come together a bit more than previously.

#2d. A new eruption is starting! It's been 83 minutes since the last eruption. Your boss asks how long it will last. What's the answer?

eruption.lm = lm(Y ~ X)
nd = data.frame(X=83)
predict(eruption.lm, nd, interval="predict") 

#3. Your boss says "Great!"; but being a graduate of this class she likes to think probabilistically. So she asks "What's the uncertainty in your prediction?".

SSE = t(residuals)%*%residuals
MSE = SSE / (length(Y)-length(b)) 
s = sqrt(MSE)

covB = solve(t(Xd)%*%Xd)*as.vector(s^2)
stdb = diag(sqrt(covB))

Xd = c(1,83)
pred = Xd%*%b 
pred.unc = Xd%*%stdb
print(pred.unc)

#3a. Back to the drawing board. You need to beef up your function to include uncertainty. First, let's calculate the standard error of the regression. To do this you will use the regression residuals (the differences between the real Y-values and the predicted y-values (or y-hat)).
#First we need to calculate the root-mean-squared-error (RMSE) on the residuals. Calculate the sum of squares of the residuals, then divide by the degrees of freedom (n-2 for regression), then take the sqare root of all of that. Call this s.

ones=matrix(1,nrow=length(X))
length(X)
Xd=cbind(ones,X)

SSE = t(residuals)%*%residuals
MSE = SSE / (length(Y)-length(b)) 
s = sqrt(MSE)

#3b. Describe in human words what s is (including it's units).

#is a measure of the difference between the predicted and actual values. it has the same units as the quantity being estimated, in this case, the eruption duration, which is in minutes

#3c. Now you need to use s, to estimate the standard error of the regression coefficients. First calculate the covariance matrix of design matrix, and multiply that by s^2. The standard error of the regression coefficients is equal to sqrt of the covariance matrix.
#Note, you will get a square matrix, with the standard errors along the diagnal. Use diag() to extract just the diagnal.

covB = solve(t(Xd)%*%Xd)*as.vector(s^2)
stdb = diag(sqrt(covB))

#3d. OK, now we have standard error estimates on our regression coefficents. Calculate two more lines for your plot, that characterize twice the range of the standard error (the two sigma range). Add this range to your plot using geom_ribbon().

yhatHi = Xd%*%(b+2*stdb)
yhatLo = Xd%*%(b-2*stdb)

unc.plot = new.new.plot +
  geom_ribbon(aes(x=X,ymin = yhatLo,ymax = yhatHi, colour = "2 sigma SE"),fill = "green", alpha = 0.1)
  
unc.plot
#3e. The last eruption ended up being 4 minutes long - different than your prediction. Now your boss really wants to have uncertainy on your prediction. Another eruption is starting, we've been waiting only 61 minutes. Provide a new prediction to your boss with a best estimate, and 2-sigma uncertainty range. 

eruption.lm = lm(Y ~ X)
nd = data.frame(X=61)
predict(eruption.lm, nd, interval="predict") 

SSE = t(residuals)%*%residuals
MSE = SSE / (length(Y)-length(b)) 
s = sqrt(MSE)

covB = solve(t(Xd)%*%Xd)*as.vector(s^2)
stdb = diag(sqrt(covB))

Xd = c(1,61)
pred = Xd%*%b 
pred.unc = Xd%*%(2*stdb)
print(pred.unc)

#3f. Your boss appreciates your more probabilistic prediction; however she needs the answer to a specific question. A bigwig is visiting the park, but only has time to watch a short eruption. If your boss tells the bigwig that this eruption will be less than three minutes, and it's not, she will be fired. Assuming your predictive model follows a normal distribution, what's the probability that the upcoming eruption will be less than 3 minutes long? Make a plot that shows the distribution of possibilities that support your probability estimate.

xseq = seq(40,98,length.out = 100) #the numbers come from point 5, the spread of the uncertainty in the plot with the regression
gauss = dnorm(xseq,mean = pred,sd=pred.unc)
ggplot()+geom_area(aes(xseq,gauss),fill = "white")+geom_vline(xintercept = 80)

1-pnorm(3,mean = pred,sd = pred.unc)


#Part 2. Multiple regression.
#Bad news. That last eruption ended up lasting 181 secs, and you got fired. Such is life of an Earth and Environmental Data Analyst. Good news though, you got a new job working for an oil company. 

#Now you're going to use your same tools to create a new regression model to predict permeability based on some image-analysis data on rock samples. Your training dataset is called "rock"
#?rock

#4a. First, explore your data. Create a facted ggplot that shows permeability plotted against area, perimeter and shape.

#4b. Use your regression function to create linear models for all three predictors (area, shape, and perimeter) separately. For each model, calculate yhat (the permeability values predicted by your model), and calcuate the percent of variance that is represented by your model (using what we learned last week in lab).


#4c. Use the same function you wrote before to create a MULTIPLE linear regression model, using area, perimeter AND shape as predictors. Calculate percent variance explained (R^2) for this model as well.



#4d. Examine the standard errors in your regression model. They're a little hard to compare since they're all in different units. Convert them all to percentages of the b values. Based on this, which predictor is doing most of the heavy lifting in the model? Is this consistent with the percent variance explained you observed in 4b?



#Part 3. Polynomial regression
#Well, the price of oil continued to fall and you lost your oil company job. Good thing you have such a strong skillset to fall back on! You quickly obtained new work with a logging company.

#Federal law requires that your company not cut trees that are older than 20 years; and your new boss asks you to create a model to predict age based on tree height. They provide you some data about this relation in "loblolly" . 
#?loblolly

#5a. Create a scatter plot of height vs age. Does a linear model seem like a good choice here?

#5b. Calculate a linear model of this relationship, calculate your predicted values (y-hat), and add that line and the uncertainties around it, to your graph.


#5c. Now calculate the residuals (Y-Yhat). Do you notice a pattern?


#5d. Let's try a non-linear model. A polynomial model. For a polynomial model, our design matrix should be, for example [X^2 X 1's], although more pieces could be added. Create a new design matrix and use your function to calcualte a polynomial regression. Add your polynomial model (and it's uncertainties) to your plot. Does this work better?



#5e. Great. Your boss is impressed with your work (and wondering how you got yourself fired twice recently). The logging company figures that it can get away with cutting a few old trees, so long as 95% of the trees it cuts are less than 20 yrs old. Given this, use your new model, and the assumption that its distribution is normal, to figure out what is the tallest trees the company should cut to make sure that 95% are less than 20 years old? (Hint: there are multiple ways to do this, but using qnorm() is probably most efficient.)



