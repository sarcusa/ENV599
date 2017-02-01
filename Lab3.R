#lab 03. More introduction to R. Booleans, flow control, functions

#####################
#lab 03 - PART 1
#To communicate effectively with your congressman, You need to convert a bunch of temperatures from Celsius to Fahrenheit
#you could do this manually, using R like a calculator, but since you'll be doing it a lot, why not make your life easier

#1a. Write a function that converts a temperature in Celsius to Fahrenheit. 

#multiple by 9, divide by 5, then add 32
convertCtoF = function(tempC){
  F =((tempC*9)/5)+32
  return(F)
  }

#first - after you've created your temperature conversion function, run that code, so your function is ready to use 

#1b. test it out, does it convert properly?
convertCtoF(0)
convertCtoF(20)
#yes it works!

#1c. use your function to calculate the F equivalent of every C temperature (only integers) between 0 and 100. You'll want to use seq() for this.

degreeF=c()
for(i in seq(0,100, by = 1)){
  degreeF[i+1] = convertCtoF(i)
  print(degreeF[i+1])
}

#1d. now make a scatter plot comparing C (x-axis) to F (y-axis)
#as always, make reasonable acis labels and such
degreeC=seq(0,100, by =1)
plot(degreeC, degreeF, main="C to F convertion", xlab="Degree C", ylab="Degree F")

#1e. What temperature is the same in C as in F? I know that you may already know this, and it's much easier to solve algebraically, but I want you to solve it the hard way. Specifically: write a while loop, that uses your function to keep testing integers until you find the answer. If I were you, I'd start with the smallest possible C (that's an integer), and move up, to make sure you don't miss it.
#remember - be careful with while loops, as they can make your computer keep running forever.

tempC = -100
while(F!= tempC){
  tempC=tempC+1
  F = convertCtoF(tempC)}
print(F)

#2. OK, what if you wanted to make a more flexible function. Make a new file and call it TemperatureConverter.R
# temperatureConverter is your one-stop shop for converting any temperature between F, C or K.
# You're function is now going to need more inputs, right? Since you need to know the temperature, what it's coming from, and what it's going to. Ultimately, it should work something like this:
#newTemp=temperatureConverter(57,from="C",to="K")

#to make this work, you will need several if-then-else statements
#also, please set up your function with default values of "C" for from, and "F" for to.

#demonstrate that your new function works by
#a. converting 300 K to C
#b. converting 200 F to K
#c. converting 100 C to F with only one input (taking advantage of your defaults)

TemperatureConverter(300, from = "K", to = "C")
TemperatureConverter(200, from = "F", to = "K")
TemperatureConverter()
#the last tempearture convertion does no work

################################
#part 2. More functions and loops
#Imagine this - we just took out our drones, and collected some not fancy new topography data for a volcano.

#we loaded our data into R, and stored it in the variable "volcano"


#3a. what kind of data set is this?
class(volcano)
#matrix


#3b. what are it's dimensions?
dim(volcano)
#87 x 61

#this dataset is preloaded in R so you can learn more about it like this:
?volcano

#3c. make a quick map of the volcano elevation using the "filled.contour" plotting option
filled.contour(volcano, color = terrain.colors, asp = 1)

#OK, we're interested in predicting where on the volcano is most likely for landslides to occur
#to do this, we'd like to make a map of what the slope is, everywhere on the volcano

#4a. to do this, lets right a function, that calculates slope if you give it a vector from the matrix (a vector being 1 column or row from the matrix)
#so write a function that calculates change in elevation between grid cells in a vector. The output should be all positive {use abs()} and be 1 cell shorter than the input.

volcano<- volcano

slope = function(x = volcano[1,]){
  for(j in volcano){
    v=split(volcano,row(volcano))
  }
  
  for(b in volcano){
    v2=split(volcano,col(volcano))
  }
  for(i in x){
    return(slp=abs(diff(x)))
  }
}

slope(x= volcano[,2])

#4b. now use your function to calculate the slopes from the 43rd column (a north-south stripe), and plot them (x=east-west,y=height)
#this question doesn't make sense, what does east-west have to do with a north-south stripe?

slopes43<-slope(x= volcano[,43])

heights<-volcano[,43]
newh<-heights[-87]

plot(slopes43,newh, xlab = "East-West", ylab = "Elevation", main = "Slope Changes from Volcano")

#5a. now create a new matrix that's the appropriate size, full of NAs, for your N-S slopes. use matrix() to do this.
newmatrix<-matrix(data=NA, nrow = 86, ncol=61)

#5b. now write a for loop that loops through all the columns, uses your function to calculate the slopes, and then puts the values into your N-S slopes matrix


for(i in seq_len(ncol(newmatrix))){
  newmatrix[,i]= slope(x= volcano[,i])
}

#5c. make a filled.contour plot of your N-S slopes
filled.contour(newmatrix, color = terrain.colors, asp = 1)

#5d. write a command that tells us which cell has the greates N-S slope

which(newmatrix == max(newmatrix), arr.ind = TRUE)


#6. repeat this for E-W

newmatrix2<-matrix(data=NA, nrow = 87, ncol=60)
for(i in seq_len(nrow(newmatrix2))){
  newmatrix2[i,]= slope(x= volcano[i,])
}

which(newmatrix2 == max(newmatrix2), arr.ind = TRUE)

#7a.Challenge question: now average your N-S slopes, and E-W slopes together. This is a little tricky, since your matrices are not the same size. It might help to sketch out a grid that shows where you have elevations and where you have slopes (both NS and EW)

nm<-newmatrix[,-61]
nm2<-newmatrix2[-87,]
my.list<-list(nm,nm2)
averages<-matrix(data=NA, nrow = 86, ncol=60)
for(i in averages){
  averages = apply(simplify2array(my.list),c(1,2),mean)
}

#7b. make a final filled.contour plot that shows the average slope. Where is the steepest part of the volcano?
filled.contour(averages, color = terrain.colors, asp = 1)


#part 3 ggplot

#EEDA - An introduction to ggplot2

#There are abundant resources on the web to help with ggplot2. Most of the exercises here are mirrored in Derek Sondereger's notes here: http://oak.ucc.nau.edu/dls354/Home/teaching/introduction_to_r.pdf


#For this Lab, we will be exploring the relation between ozone and several environmental parameters in NYC
#check out the airquality dataset
?airquality

#You're going to need the ggplot library - so load it in.

################################
##PART 3. Playing with ggplot2.

#1a. Use ggplot to Make a scatter plot that compares ozone to wind. How does ggplot handle missing values?


#1b. Good - now plot it with larger dots, that are not black, and not circles
#hint: use ggplot documentation and http://www.cookbook-r.com/Graphs/Shapes_and_line_types/ and google, as needed


#1c. Now - add a linear trend line


#What's a factor?
?factor
#compare
airquality$Month
factor(airquality$Month)


#1d. Replot your scatter plot, but this time color the dots based on what month they correspond to. Try coloring by both Month, and factor(Month). What's the difference?

#Now color it by a continuous variable - like Temperature

#that color scale is not ideal. Use a better one (i.e., one that is intuitive AND visible to color blind people)


#OK - the axis labels by default are the data.frame column names, which isn't bad, but don't include units. Make better labels on this final graph. {hint: labs()}


#congratulations! You've now created an effective, single plot that allows you (or your reader) to quickly understand the relation between ozone, wind and temperature.
#1e. Briefly describe what you've learned about what controls ozone concentrations based on this plot.



##PART 2. More plots 
#OK - more types of plots. 
#Objective: create a plot that shows ozone concentration (plus uncertainties) in each month. (hint: dont forget what we learned about factoring)

#2a. first, let's explore some built in options - http://docs.ggplot2.org/current/
#two reasonable options are boxplots and violin plots. Try both of these.


#Saw we want something more simple (and probably less informative)...
#2b. Use what we've learned in class thus far to calculate the mean and standard deviation of ozone in each month.
#hint(look into the na.rm option in mean and sd. What does that do?)


#2c. Now create a data frame for plotting in ggplot2

#2d. now make a bar plot with the mean of each month with error bars around it

#2e. Tinker with the coloring and other aesthetics (including labels) to make this a more visually effective graph.

##PART 3. Faceting. Sometimes lots of plots are useful
#3a. create a histogram of solar Radiation (using ggplot2)
#Make sure you add units to your axes as appropriate

#3b. Now create a new column in your dataframe that characterizes whether it's a sunny day or not.
#Based on your histogram, what seems like a good cut off for sunny vs not sunny?

#create a new vector, and apply the string "sunny" to the sunny days and "cloudy" to the cloudy days.

#now add that sunniness column into your dataframe


#3c. now create a faceted plot that's a scatterplot of  Ozone to wind speed, for both sunny and cloudy separately, and for each month separately (so it should be a 2x5 grid of plots)

#you probably got a grid that is 3x5. Why did this happen? If you haven't already - modify your data frame to fix this. Also make your facet labels as pretty as can be.

#3d. add linear trendlines to these data.

#3e. There's now a lot of information on one figure about what controls ozone. Under what conditions is the relationship the strongest? Why do you think this is the case?














