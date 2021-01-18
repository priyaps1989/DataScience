add<-function(input1,input2){
  return(input1 + input2)
}
sub<-function(input1,input2){
  return(input1 - input2)
}
multiply<-function(input1,input2){
  return(input1 * input2)
}
divide<-function(input1,input2){
  return(input1 / input2)
}

factors<-function(input1) {
  print(paste("The factors of",input1,"are:"))
  ##looping from 1 to user input number to get the mod 
  for(i in 1:input1) {
    if((input1 %% i) == 0) {
      print(i)
    }
  }
}

prime<-function(input1){
  check = 0
  # prime numbers are greater than 1
  if(input1 > 1) {
    # check for factors
    check = 1
    for(i in 2:(input1-1)) {
      if ((input1 %% i) == 0) {
        check = 0
        break
      }
    }
  } 
  if(input1 == 2)  {  check = 1}
  if(check == 1) {
    print(paste(input1,"is a prime number"))
  } else {
    print(paste(input1,"is not a prime number"))
  }
}

