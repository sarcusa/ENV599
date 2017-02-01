
convertCtoF = function(tempCF){
  CF =((tempCF*9)/5)+32
  return(CF)
}

convertFtoC = function(tempFC){
  FC =((tempFC-32)*5)/9
  return(FC)
}

convertKtoF = function(tempKF){
  KF =((9/5)*(tempKF-273.15)+32)
  return(KF)
}

convertFtoK = function(tempFK){
  FK = ((5/9)*(tempFK-32)+273.15)
  return(FK)
}

convertCtoK = function(tempCK){
  CK = tempCK + 273.15
  return(CK)
}

convertKtoC = function(tempKC){
  KC = tempKC - 273.15
  return(KC)
}

TemperatureConverter = function(x = 100, from= "C", to= "F" ){
#pips = 
    if(from=="K"& to == "C"){
      return(print(convertKtoC(x)))
    }else{
      return(print(convertCtoK(x)))
    }
  if(from == "K"& to == "F"){
    return(print(convertKtoF(x)))
  }else{
    return(print(convertFtoK(x)))
  }
  if(from == "C" & to =="F"){
    return(print(convertCtoF(x)))
  }else{
   return(print(convertFtoC(x)))
  }
}


