#dice = function(numberOfDice=1){
#  pips = sample.int(6,size = numberOfDice,replace=TRUE)
#  return(pips) #also needs to return something
#}

#this runs it
#print(dice(5)) #the number changes the default
#
#guessMyDice = function(yourGuess){
#  pips = dice()+dice()
#  if(yourGuess==pips){
#    print("Wow, you're psychic!")
#    return(TRUE)
#  }else{
#    print("You lose. Try again")
#    return(FALSE)
#  }
#  
#}

#guessMyDice(7)

convertCtoF = function(tempCF){
  F =((tempCF*9)/5)+32
  return(F)
}

convertFtoC = function(tempFC){
  C =((tempFC-32)*5)/9
  return(C)
}

convertKtoF = function(tempKF){
  K =((9/5)*(tempKF-273.15)+32)
  return(K)
}

convertFtoK = function(tempFK){
  F = ((5/9)*(tempFK-32)+273.15)
  return(F)
}

convertCtoK = function(tempCK){
  C = tempCK + 273.15
  return(C)
}

convertKtoC = function(tempKC){
  K = tempKC - 273.15
  return(K)
}

TemperatureConverter = function(x, from= "y", to= "z" ){
  pips = 
}


