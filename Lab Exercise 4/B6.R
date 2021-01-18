## Question 6

# H0;
IQ = 100 #average IQ
# H1;IQ != 100
# A sample of 30 participants who have taken the medication has a mean of 110 
sample = 30
mean = 110 
sd = 10 # average IQ 100 with a standard deviation of 10

## Z is calculated as +/- 2.58
z = (mean-IQ)/(sd/sqrt(sample))
print(z)
alpha = 0.01
# 1-0.5a = 0.995
# In table, only find nearest value 0.9951

##Result: Reject the Null Hypothesis.
##Conclusion: Medication significantly affect the intelligence Z = 5.477, p < 0.01.
## Z is calculated as +/- 2.58

z0 <- 2.5+0.08 

if (z <= z0*(-1) | z > z0 ) {
  print("Reject")
} else {
  print("Accept")
}
#checking Hypothesis
L = 110 - z0 * sd/sqrt(sample)
U = 110 + z0 * sd/sqrt(sample)
##110 +/- 4.7104 is the confidence interval
print(L)
print(U)

## Question 6 ends

