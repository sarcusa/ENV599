library(ggplot2)
head(diamonds)
#let's make a big plot avoiding the three rows that aren't data
plot(diamonds[1:100,-c(2,3,4)])

#get the matrix ready for analysis
Xraw = diamonds[1:10000,-c(2,3,4)]

#subtract the mean from each column. We can do this easily with scale()

#and use scale() to subtract that mean.
X = scale (Xraw, scale=TRUE) #z scoring, column by column, TRUE divides by standard dev
#what does the scale=FALSE do?
X2= scale (Xraw, scale=FALSE)

head(X)
head(X2)

#let's calculate our covariance matrix:
coMat  = (t(X)%*%X)/ (nrow(X)-1)
head(coMat)
#compare that to a correlation matrix, its the same
head(cor(Xraw))

coMat2 = (t(X2)%*%X2)/ (nrow(X2)-1)
head(coMat2)

#Now that we have our covariance/correlation matrix, we can use svd to find the eigen values and vectors:
out = svd(coMat)
names(out)#d=D (eigenvalues), u=E, v=E^T

print(out$d)#svd returns only, the diagonal. Positive numbers that decrease in value
diag(out$d)

#so our total covariance is just the sum of out$d
sum(out$d)

#and the fraction of variance in each is
out$d/sum(out$d)

#use new EOFs (=eigenvectors) to project data onto this new coordinate sys
PCs = (X %*% out$v) #this is our projection of our data on the eigen vectors. We call this the PC score. 
dim(PCs)
cov(PCs)#did it work? If so each PC score should be uncorrelated with all the others. 

#look at PCs, by plotting the first and second PCs plotted against one another
df = cbind(as.data.frame(PCs),diamonds[1:nrow(X),]) #let's make a big data frame with the original data, and our PCs
dim(out$v)

df2 = cbind(as.data.frame(out$v),names(diamonds[,-c(2,3,4)])) #and a second with just the eigenvectors
names(df2)[8]="names"
arrowScale=6 #lets setup the length of our arrows as a variable
ggplot(df, aes(x = V1, y= V2))+#and make an awesome plot, we're going to compare PC1 and 2
  geom_point(aes(colour=cut),size=1)+ #first just plotting points and colouring them by the cut
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+#then we'll plot some 0 lines for our reference
  coord_fixed(xlim=c(-7,7),ylim=c(-7,7))+#and set the scale
  geom_segment(data=df2,aes(x=0,y=0,xend = V1*arrowScale, yend = V2*arrowScale))+#Now we'll add lines that correspond to our eigen vectors
  geom_text(data=df2,aes(x = V1*arrowScale, y = V2*arrowScale,label=names) )#and label those lines

#PCS maps the same way as x,y,z, carat and price, all related but not suprising
#second EOF is heavily loaded in table and depth in opposite direction (inverse relationship)
#PCs maximize the variance

#repeat this with PC2&3
ggplot(df, aes(x = V3, y= V2))+#and make an awesome plot, we're going to compare PC1 and 2
  geom_point(aes(colour=cut),size=1)+ #first just plotting points and colouring them by the cut
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+#then we'll plot some 0 lines for our reference
  coord_fixed(xlim=c(-7,7),ylim=c(-7,7))+#and set the scale
  geom_segment(data=df2,aes(x=0,y=0,xend = V3*arrowScale, yend = V2*arrowScale))+#Now we'll add lines that correspond to our eigen vectors
  geom_text(data=df2,aes(x = V3*arrowScale, y = V2*arrowScale,label=names))#and label those lines


