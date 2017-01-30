##Earth and Environmental Data Analysis, Week 03
#Booleans, Flow control and Functions. Oh my!

#We'll start with Booleans

#Here's a boolean
5==6
#also works with string
string1="hello!"
string2="hello!"
string1==string2

answer = 4>3
print(answer)
class(answer)

#other boolean expressions
5<=5

#Brief detour. What is NA?
x = NA
y= 7

#Boolean returning functions that start with "is."
is.na(x)
is.na(y)

is.logical(answer)
is.data.frame(answer)

#R can perform logical operations on logical type data
5==5 & 5>6 # returns a false because one is true, & means and
5==5 | 5>6 # returns a true because one is true, shift backslash line | means or

#use this build complex logic
(4>3 | 3>4) & 5==5

#we can also use "NOT" statements using "!", i.e. flips to the opposite
!is.na(x)
5!=6 #a shorter way to find not equal to
!(5==6)#means the same thing to R

#with real data
data= MonthlyGlobalTemperature

#now we can explore and mess with "data"
class(data)

#get all of the rows from the first column
year = data[,1]
year==1956
#or try 
year>1956
#we're just interested in what happened between 1942 and 1963
tf = year>=1942 & year<=1963
print(tf)

#which rows are TRUE (met the criteria we laid out)
goodYears = which(tf) #which will return the row numbers of the answer in the ()
print(goodYears)

#lets make sure this did what we wanted
year[goodYears] #this enables us to see the values of the rown numbers

#what was the average tempertaure of February between 1942 and 1963
mean(data[goodYears,3])

#you can also use logical indexing
mean(data[year>=1942 & year<=1963,3])

#Another example of how using logic makes science easier
coldFebruaries = which(data$Feb < -0.5)
print(coldFebruaries)
length(coldFebruaries)

year[coldFebruaries] #this will print out the years that were cold, the [] stand for "of"

##Part 2. Flow control

#if/else statements

if(5==6){
  print("It's True! Good job!")
}else{
  print("It's False! Try again :(")
}    

num1=(7*3^2)
num2=(6*4^2)

#try an else if
if(num1==num2){
  print("It's True! Good job!")
}else if(num1<num2){
  print("It's smaller, not the same!")
}else{
  print("it must be bigger!")
}

#now for "for loops" !

for(i in 1:10){
  print(paste("We're in loop",as.character(i)))
}

#the incrementing variable doesnt have to be i, and it doesn't have to go in order
for(nick in seq(10,-4,by=-2)){
  print(paste("Now nick is equal to",as.character(nick)))
}

#we can use this for data analysis
#calculate temperature across every column

meanMonthlyTemperature=c()
for(i in 2:ncol(data)){
  meanMonthlyTemperature[i-1]=mean(data[,i])     #can use na.rm=TRUE to over ride NAs  
  
}
print(meanMonthlyTemperature)

#One more kind of loop
#the while loop!

dontStop=TRUE

t=0
while(dontStop){
  t=t+1 
  print(t)
  dontStop = t<100 
} 

#another version with a safety net
t=0
while(t!=55){
  t=t+2
  print(t)
  
  #add a safety net
  if(t > 100){
    print("Uh oh")
    break
  }
  
}


#Part 3. Functional programming

#this creates a function
dice = function(numberOfDice=1){
  pips = sample.int(6,size = numberOfDice,replace=TRUE)
  return(pips) #also needs to return something
}

#this runs it
print(dice(5)) #the number changes the default

#lets create another function
guessMyDice = function(yourGuess){
  pips = dice()+dice()
  if(yourGuess==pips){
    print("Wow, you're psychic!")
    return(TRUE)
  }else{
    print("You lose. Try again")
    return(FALSE)
  }
  
}

guessMyDice(7)

#one last thing
#programming lets you cheat at guessing games.
ilose = TRUE
i=0
while(ilose){
  i=i+1
  print(i)
  didIWin = guessMyDice(12)
  if(didIWin){
    ilose=FALSE
  }
  
}
