X=airquality$Wind
Y=airquality$Ozone
toRemove=which(is.na(X) | is.na(Y)) #remove any NA in the dataset
X=X[-toRemove]
Y=Y[-toRemove]
plot(X,Y)

#use regression to create a simple model to predict ozone conc from wind speed

#step 1 is to create a design matrix which has column of 1 and column of data to use
ones=matrix(1,nrow=length(X))
length(X)
Xd=cbind(ones,X) #this is our design matrix

#double check there aren't any NAs
any(is.na(Xd))

#use normal equation which is B = (X'X)^-1 * (X'Y)
XX=t(Xd)%*%Xd
XY=t(Xd)%*%Y
B=solve(XX)%*%XY
print(B)

#b0 is 96.872
#b1 is -5.55

#visualize the results
xseq=seq(0,20)
yhat=xseq*B[2]+B[1] #just calculating it as a simple y=mx+b type equation.
#now lets plot the original data, and add a line for our model.
library(ggplot2)
regPlot = ggplot()+geom_point(aes(X,Y))+geom_line(aes(xseq,yhat))
print(regPlot)

#calculate that with LA
yhat2 = Xd%*%B
ggplot()+geom_point(aes(X,Y))+geom_point(aes(X,yhat2),colour = "red")

#the difference between the points and the points we modelled is the error
residuals = Y-yhat2
ggplot()+geom_point(aes(X,residuals)) #as a line plot, will be centered around zero but randomly distributed around the mean, if there is a structure then it is bad news (e.g. a U shape means a linear model is not ideal). The range of uncertainty can also be seen i.e. 50 to -50, this tells us how well our model will work. Also want residuals to be normally distributed.
ggplot()+geom_histogram(aes(residuals)) #as a histogram, shows that it is a little skewed but probably approx normal

#estimate the uncertainty on B. To do this we start with the residuals

#ultimately we want to use the root mean squared error. If we calculated the mean as normal, we would get a value close to zero, which is why we must square the value first

#
SSE = t(residuals)%*%residuals
MSE = SSE / (length(Y)-length(B)) #the degrees of freedom here is the number of observations minus the number of parameters we calculated
#Now moving forward we want the root mean square error (RMSE) or s .
s = sqrt(MSE) #the sum of squares, averaged and rooted, a single value which approx the uncertainty in the residuals
print(s)

#Now, let's use s to calculate uncertainty on B
#Find the covariance of B. Using this equation
covB = solve(t(Xd)%*%Xd)*as.vector(s^2)
print(covB)

#turn the covariance matrix into estimates of the standard error of B
stdB = diag(sqrt(covB)) #returns only the diagonal entries
print(stdB)
#returns the standard error (closely related to SD, doesn't correspond to a normal distribution) on B0 and B1

#OK now calculate lines that correspond to the uncertainty, by adding/subtracting the standard error
yhatHi = Xd%*%(B+stdB)
yhatLo = Xd%*%(B-stdB)

#ribbon can display uncertainty
ggplot()+geom_ribbon(aes(x=X,ymin = yhatLo,ymax = yhatHi),fill = "green")+geom_point(aes(X,Y))+geom_line(aes(xseq,yhat))

#calculate the uncertainty around a specidfic x value
X5 = c(1, 5)  #Create a single value design matrix to make a prediction
pred = X5%*%B #multiply that matrix by B to get you modeled prediction
pred.unc = X5%*%stdB#multiply that matrix by std error of B to get uncertainty on prediction
print(pred.unc)

#If we assume that prediction is normal, with a mean of the prediction and a standard deviation of the probability, what's probability in our prediction that if the wind is blowing 5 mph, the ozone concentration will be above 80?
#simple probabilistic testing
xseq = seq(40,98,length.out = 100) #the numbers come from point 5, the spread of the uncertainty in the plot with the regression
gauss = dnorm(xseq,mean = pred,sd=pred.unc)
ggplot()+geom_area(aes(xseq,gauss),fill = "white")+geom_vline(xintercept = 80)

#how would we use pnorm() to calcuate the proba that ozone > 80?
1-pnorm(80,mean = pred,sd = pred.unc)
