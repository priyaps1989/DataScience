source("functions.R")

while (1) {
  
  cat(" \n  \n " )
print("******Simple R Calculator - Select operation******")
print("1.Add")
print("2.Subtract")
print("3.Multiply")
print("4.Divide")
print("5.Factors")
print("6.Prime")
print("7.Exit")
print("**************************************************")
cat(" \n   " )
choice = as.integer(readline(prompt="Enter choice[1/2/3/4/5/6/7]: "))
if(choice < 5){
  input1 = as.integer(readline(prompt="Enter first number: "))
  input2 = as.integer(readline(prompt="Enter second number: "))
  
  if(is.na(input1) | is.na(input2)){cat(" \n Enter valid numbers \n\n " )}
  
}else if((choice == 5) | (choice == 6)){
  input1 = as.integer(readline(prompt="Enter the number: "))
  if(is.na(input1) ){cat(" \n Enter a valid number \n " )}
} 
  
  else if (choice == 7){print("Thank You")
    break}else { cat(" \n Enter a valid choice \n\n " )

  }

if(choice < 5 && (!is.na(input1)) && (!is.na(input2))){
operator = switch(choice,"+","-","*","/")
output = switch(choice, add(input1, input2), sub(input1, input2), multiply(input1, input2), divide(input1, input2))
print(paste(input1, operator, input2, "=", output))

}else if (choice == 5 && (!is.na(input1))){factors(input1)  
  }  else if (choice == 6 && (!is.na(input1))){prime(input1)}
      else if (choice == 7){break}
}   
    
  
  

