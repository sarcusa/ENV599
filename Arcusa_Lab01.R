#Earth and Environmental Data Analysis
#================

#Lab 1
#----------

#Before you start. Fill out this form!
#http://goo.gl/forms/At6GoW6CKr


###Introduction to R and DataJoy, 1

#Section 1
#=======
#Playing with subsetting
#---------

#In class we assigned numbers to variables, but you can assign letters, or series of letters as well (we call these "strings")
#e.g., 
a="p"
b="i"
c="g"

#1. try it yourself : spell out an animal that is at least six letters long.

a<- "s"
b<-"a"
c<-"l"
d<-"m"
e<-"o"
f<-"n"


#2. Now, you have a bunch of letters stored in their own variables, what if you want to assign this into a single variable that has it's own word?
#use the function "c" to concatenate this into a word e.g. c(a,b,c), and assign this into a variable called "animal"

animal <- c(a,b,c,d,e,f)

print(animal)

#3. OK, now subset animal. Grab the first three letters, then the last three, then letters 4 , 6 and 2, in that order.

subset<-animal[c(1:3,4:6,4,6,2)]
subset

#what happens if you enter animal[-1]? Or animal[-4]? Given this, what do negative indices mean?

animal[-1] #removes the first variable
animal[-4] #removes the fourth variable
#negative indices removes them

#4. lastly, write a command that subsets your animal to create a new word (may not be possible with all animals)
newword<-animal[c(1,3,2,4)]
newword

#Section 2
#=======
#Now let's do some science
#---------


#Load in your data set
 #Don't forget to assign it to a variable!
setwd("C:/Users/sha59/Google Drive/ENV599")
temp=read.csv("MonthlyGlobalTemperature.csv")

#How big is your dataset? How many rows and columns?
dim(temp)
#166x13

#What do the rows and columns represent? (Do they have names?)

rownames(temp) #rows are numbers but actually could be the year
colnames(temp) #columns are months

#It doesn't really make sense to keep one of these columns in the data table, let's assign that column to it's own variable, named appropriately
Yr<-temp[1]

#and the rest of the table to a different variable. Also named appropriately.
months<-temp[2:13]
rownames(months)<-temp$Year
rownames(months)

#OK - now write code to answer these questions:

#1 What was the temperature in September of 1941?
which(Yr == '1941')
months[92,c('Sep')]
#-0.129

#2a. What's the warmest temperature ever recorded in February?
maxFeb<- max(months$Feb)
maxFeb
#0.763

#2b. In which year was that warm February recorded? (hint:, use "which")

yrmaxFeb <- months[which(months$Feb == maxFeb),]
yrmaxFeb
#1998

#3. What was the median monthly temperature in 1976?
median(as.matrix(months)[which(Yr =='1976'),])
#-0.2155
# to check: median<- apply(months,1, median), the answer is the same

#4. Given these data, calculate mean monthly temperature (for all months), and then plot them, with appropriate axis labels (ALWAYS!)
monthly_temp<-apply(months,2,median)
x_months<- as.vector(colnames(months))

plot(monthly_temp, 
     type="l", xaxt="n",
     xlab="Month", ylab='Mean monthly temperature', main= "Global Temperature (C)")
axis(1,at=1:12,labels = x_months)

#5. Calculate and plot Annual mean temperature from 1850-2015
median<- apply(months,1, median)
medianmatrix<-as.matrix(median)

plot(t(Yr), medianmatrix,
     xlab="Year", ylab='Mean annual temperature (deg C)', main= "Global Temperature (1850-2015)", 
     type = "l")

#6. Plot a histogram of NH summer (average June, July and August) temperatures.
summer_temp<-apply(months[6:8],1, median)
hist(summer_temp, main = "NH Summer (JJA) Temperature (deg C)", xlab = "Average (1850-2015) Temperature (C)")

#7a. Plot a box plot of monthly temperature. Which month has the biggest spread in temperature?
boxplot(months)
#Either Feb or Dec has the largest spread, difficult to tell from the plot 

#7b. Calculate the standard deviations of all monthly temperatures. Does this result agree with your answer to the previous question?

SD<-apply(months,2,sd)
which(SD == max(SD))
#February has the largest standard deviation which agrees with the answer for 7a

#7c. Bonus challenge: Explain why (in terms of Earth Science), you got the answer you got in question 7
#February has the largest spread because it is probably one of the month that has warmed the most over the span of the dataset
#Winter months tend to warm more than summer months, as it is the minimum temperature that sees the most warming


