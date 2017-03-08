
#Earth and enviromental data analysis.
#Keep this code here. It:
rm(list=ls()) #removes all your local variables. This helps you check for bugs by clearing memory each time you run your script


#Lab 08. Principal Componants Analysis (Empirical Orthogonal Functions)

#1. Explore the iris dataset. We're looking for the major modes of variability in the iris dataset. PCA let's us readily explore this.
?iris

#1a. first plot of matrix of scatter plots. The built-in plotting option for data.frames makes a handy grid that's a nice analogy of a covariance matrix. Do this for the iris data (you'll have to exclude the species names column.)

my.df = iris[,-5]
plot(my.df)

#1b. Which variables appear to have high covaraince?
#sepal and petal length
#petal length and width

#1c. Now let's calculate the covariance matrix. First, make a matrix from iris that excludes the species column - we'll use that later when examining the data.
my.df = iris[,-5]

#1d. Now we want to calculate the covariance matrix. First need to make a new version of this matrix such that each column has Center the data so that the mean of each column is 0
#scale() is a useful function to do this, but check afterward to make sure you'be done it right

scaled = scale (my.df, scale=TRUE)
head(scaled)
mean(scaled[,1]) #the mean of the column is zero

#1e. OK - now use linear algebra to calculate your covariance matrix. Are the values consistent with the plots in 1a?

coMat  = (t(scaled)%*%scaled)/ (nrow(scaled)-1)
#yes, it is consistent

#1f. Now we want to try to find a matrix ("P") (our EOFs) that we can multiply by our original matrix to turn it into orthogonal indices (our PCs).
#To do so, use svd() on your covariance matrix. 
out = svd(coMat)

#The "v" matrix that is returned is your P matrix. Thanks svd! These are also called your loadings.
#the "d" results are the diagonal of your new covariance matrix. Save this for later.

d = out$d
P = out$v

#Multiply your original matrix by P - this gives you your PCs. Explain what's happening in this step. 

PCs = (scaled %*% P) #the data is projected onto the eigenvectors (i.e. the new coordinate system after rotation)

#2. Explore the results. 
#2a. Calculate the covariance matrix of your PCs. Did PCA accomplish the what it set out to?

cov(PCs)# yes, the off diagonal values are zeros

#2b. If all went well, all of the Covariance in the dataset, has been moved into the variance of the 4 PCs (all zeros off the diagonals, right?). Calculate the fraction of variance of total variance represented by each PC.

var = diag(cov(PCs)/sum(cov(PCs)))

#2c. Go back to the D vector you stored earlier. Calculate the fractional of total values in that vector compared to the total.

print(d/sum(d))

#2d. How do the values in 2c and 2b compare? (Hint: they should be very similar)
#yes, they are similar

#2e. Plot these fractions of variance (PC # on the x-axis, fraction of variance on y-axis)

p1 = c(1,2,3,4)
plot(p1, var, type = "l", xlab = "PC", ylab = "Fraction of variance", xaxt = "n")
axis(1, at = 1:4)

#2f. Which PCs do you think we should examine?

#PCs 1 and 2 because they explain most of the variance

#2g. Let's examine those PCs. To do so we need to make one graph:

#i. We want this graph to compare the observations between the first two PCs, and color the dots by the species (perhaps some species fall into different PC-space than others.)
#ii. We want to show how the original coordinates map onto our new coordinate system  - so labeled arrows that point towards their PC loadings.

df = cbind(as.data.frame(PCs),iris[1:nrow(scaled),]) #let's make a big data frame with the original data, and our PCs
df2 = cbind(as.data.frame(P), names(iris[,-5])) #and a second with just the eigenvectors
names(df2)[5]="names"
arrowScale=4

library(ggplot2)

ggplot(df, aes(x = V1, y= V2))+
  geom_point(aes(colour=Species),size=1)+ 
  geom_hline(yintercept=0)+geom_vline(xintercept=0)+
  coord_fixed(xlim=c(-4,4),ylim=c(-4,4))+
  geom_segment(data=df2,aes(x=0,y=0,xend = V1*arrowScale, yend = V2*arrowScale))+
  geom_text(data=df2,aes(x = V1*arrowScale, y = V2*arrowScale,label=names))

#2h. OK - let's interpret this awesome graph. Setosa is appears distinct from the species. Does it separate along PC1 or PC2? Which variable(s) primarily map along that PC. What does this tell you about what real measurement(s) separate Setosa from the other two?

#its separates along PC1
#petal length and width
#setosa has smaller petals

#2i. Two of the original variables map very similarly. What does this tell you about the relationship between these two variables?

#the relationship is in the same direction

#3 Principal components analysis of timeseries data 
#3a. Load in the river flow data from North America.

setwd("//cefnsshares/Homes/NAU-STUDENTS/sha59/Desktop/ENV599/lab08-sarcusa")
setwd("C:/Users/sha59/Google Drive/ENV599/lab08-sarcusa")
load("riversNA.Rdata")

#3b. Now that we have our methodology for PCA setup - it's trivial to make those calculations. For the rest of your life, the hard part will be manipulating your data into a proper matrix. That starts today.
#Create a matrix that has monthly river flow data with time moving down the rows, and each column representing a different site.
#We want to look at riverflow between 1948 and 1996. Many records from North America don't start that early or end that late - especially in the soon-to-be-walled-off non-US North American countries. We only want to include records that extend over that range. Here's some code that you might use to find those records.

startYear=1948
endYear=1996
tu=which((unlist(lapply(rivers,function(x){min(x$d$Year)})))<=startYear & (unlist(lapply(rivers,function(x){max(x$d$Year)})))>=endYear)#this tells you which entries in rivers cover the time span we defined.

#This means your matrix should be 49*12 rows long, and have length(tu) columns
#Create an empty matrix of this size that we'll populate with the records in a for loop. 
#Go ahead and write that for loop. Within the loop, you'll need to:

#1)pull the data.frame out of each spot in the list, 
#2)reshape the monthly matrix into a nx1 vector, 
#3)then write that into a new row for each river in your empty matrix, making sure that you have the right rows in the right place. 

#Here's an example of what how to convert that matrix into a vector:
toPC[,i]=as.vector(unlist(t(rivers[[tu[i]]]$data[index,2:13])))#for the monthly option.
#where index is the rows of the matrix that you want (that correspond to the time frame we chose), and i is incrementing through the good datasets (tu) and also each column in your matrix.

toPC = matrix(data = NA, nrow = 49*12, ncol = length(tu))
for(i in 1:length(tu)){
  index = match(startYear:endYear,rivers[[tu[i]]]$data$Year)
  toPC[,i]=as.vector(unlist(t(rivers[[tu[i]]]$data[index,2:13])))
}

#Warning. Some of the rivers will be have long enough records, but will be missing entire years of data in the interval we're studying. This might break your code. To deal with this you can either replace those missing values appropriately in your matrix with NAs (preferred solution), OR exclude those datasets from matrix (easier, but less preferred solution)

#An alternative approach would be to average all of the monthly data into annual data, and perform PCA on that. You can choose whether to do PCA on the montly or annual values. Ideally, try it on both! Here is what the middle of your loop might look like if you want to annually average:
toPC[,i]=as.vector(apply(rivers[[tu[i]]]$data[index,2:13],1,mean,na.rm=TRUE))#or for the annual option where you're averaging the results.
#3c. Congratulations - you now have a big matrix "X" that we can perform PCA on. First though, take a look at the histogram of the matrix (all the data). Is it approximately normally distributed?

#River flow data are famously not normally distributed. So let's log-transform all the data. (use log() ). One last note though - because sometimes, river flow is 0, (and log(0) is -INFINITY), let's add 0.1 to all the data before log-transforming. 

toPC2 = toPC+0.1
toPC.log = log(toPC2)

#3d. Now to do PCA. Note that there are some NAs in this dataset. Those will break our method unless we deal with them. I'll describe the simplest way to deal with them below
#First we need prepare the matrix by removing the mean. Optionally, we can also adjust the standard deviation to one if we want to use a correlation matrix, rather than a covariance matrix. Which should we use here? Explain your logic.

#we can use a covariance matrix here because the units are the same and the difference between the min and the max is not major.

scaled = scale(toPC.log, scale = FALSE)

#3e. Now that it's scaled - let's deal with those NAs
#since our new mean is 0 for all rows, let's replace those NAs as zero - we'll pretend the missing values are mean values.

scaled[is.na(scaled)] <- 0

#3f. I told you that preparing the matrix would be a pain. Go ahead and calculate your EOFs and PCs for this matrix. 
#How many EOFs and PCs (same number) did you get? Does this seem like the right number?

coMat  = (t(scaled)%*%scaled)/ (nrow(scaled)-1)
out = svd(coMat)
d = out$d
P = out$v
PCs = (scaled %*% P) #61 PCs, yes it is the right number because that's also the number of stations in the dataset

#4. Investigate the results. 
#4a. Now calculate the percent variance explained by each of the PC timeseries. Make a plot that shows this for each of the PCs.
#How many PCs do you think we should look at?

var = diag(cov(PCs)/sum(cov(PCs)))
var = (d/sum(d))
p1 = c(1:61)
plot(p1, var, type = "l", xlab = "PC", ylab = "Fraction of variance", xaxt = "n")
axis(1, at = 1:61)
#we should look at the first 4 PCs because that were the line bends on the graph

#4b. Use ggplot to make a timeseries (time vs PCs) for all the PCs you chose to investigate (at least 3). You can make these as individual figures, but using the gridExtra package and grid.arrange() would be more useful.


#month.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#months = rep(month.names, 49)
#mat = cbind(months,year,PCs)
#colnames(df) = c("Month", "Year", 1:61)

df = as.data.frame(PCs)
year = rep(1948:1996, each = 12)

library(ggplot2)

my.plot = qplot(data = df, x = df$Year, y = df$`1`)

PC1.plot = ggplot()+
  geom_line(aes(year,df$V1), colour = "blue")+
  ylab("PC1")+
  xlab("Year")
 
PC1.plot

PC2.plot = ggplot()+
  geom_line(aes(year,df$V2), colour = "darkgreen")+
  ylab("PC2")+
  xlab("Year")

PC2.plot

PC3.plot = ggplot()+
  geom_line(aes(year,df$V3), colour = "red")+
  ylab("PC3")+
  xlab("Year")

PC3.plot

PC4.plot = ggplot()+
  geom_line(aes(year,df$V4))+
  ylab("PC4")+
  xlab("Year")

PC4.plot

library(gridExtra)

grid.plot = grid.arrange(PC1.plot, PC2.plot, PC3.plot, PC4.plot)


#Plot the first PC against time. 

PC1.plot

#4c. Make maps of all the EOFs using the ggmap library. 

#install.packages("ggmap")
#then you should be set to proceed as normal.
#First, you'll need to extract all of your lats and longs for the records you selected.

coord.toPC = matrix(data = NA, nrow = 2, ncol = length(tu))
for(i in 1:length(tu)){
  coord.toPC[1,i]=as.vector(unlist(t(rivers[[tu[i]]]$lat)))
  coord.toPC[2,i]=as.vector(unlist(t(rivers[[tu[i]]]$lon)))
}

coord.df = as.data.frame(t(coord.toPC))
colnames(coord.df) = c("Lat", "Long")

map.df = cbind(coord.df, P)
names(map.df)[c(3,4,5,6)]= c("PC1", "PC2", "PC3", "PC4")

#Then save the EOFs and the coordinates in a data.frame.

#ggmap is a lot like ggplot, but first you start with a base map. We can get a base map easily by making a bounding box using all the coordinates we want to map.
library(ggmap)
?make_bbox #note the scale option if your map isn't quite right. 

make_bbox(coord.df[,2], coord.df[,1])

#Then use your bounding box to create a map
?get_map
map = get_map(location=make_bbox(coord.df[,2], coord.df[,1]),
        maptype = "terrain",
        source = "google",
        zoom = 4)

plot(map)

#then start your ggmap with this map, and add layers, exactly like ggplot
?ggmap

#Let's visualize our EOFs as colored dots (where the color corresponds to the loading (the values in E$v)). Make your dots an appropriate size, and make a nice color scale. Colors on EOF maps should always go to white near the zero values, since we want to disregard the low loadings.

my.map1 = ggmap(map, extent = "normal", legend = "right")+
  geom_point(data = map.df, aes(x = Long, y = Lat, colour = PC1), size =3)+
  scale_color_gradient2(high = "white", low ="red")+
  labs(title = "PCA loadings US rivers 1948-1996", x = "Longitude", y = "Latitude")
  
my.map1

#Go ahead and make maps for all your EOFs that you chose to investigate. They can each be there own figure. Or you could put them in all the same with grid.arrange() or you could pair EOFs with their PCs (my personal favorite option). Your choice.

mid = mean(map.df$PC2)

my.map2 = ggmap(map, extent = "normal", legend = "right")+
  geom_point(data = map.df, aes(x = Long, y = Lat, colour = PC2), size =3)+
  scale_color_gradient2(midpoint = mid, mid = "white", low ="red", high = "blue")+
  labs(x = "Longitude", y = "Latitude")

my.map2

mid = mean(map.df$PC3)

my.map3 = ggmap(map, extent = "normal", legend = "right")+
  geom_point(data = map.df, aes(x = Long, y = Lat, colour = PC3), size =3)+
  scale_color_gradient2(midpoint = mid, mid = "white", low ="red", high = "blue")+
  labs(x = "Longitude", y = "Latitude")

my.map3

mid = mean(map.df$PC4)

my.map4 = ggmap(map, extent = "normal", legend = "right")+
  geom_point(data = map.df, aes(x = Long, y = Lat, colour = PC4), size =3)+
  scale_color_gradient2(midpoint = mid, mid = "white", low ="red", high = "blue")+
  labs(x = "Longitude", y = "Latitude")

my.map4

grid.plot2 = grid.arrange(my.map1, my.map2, my.map3, my.map4)

#4d. Describe and interpret your results. What are the primary spatial and temporal patterns of riverflow variability in the US?


