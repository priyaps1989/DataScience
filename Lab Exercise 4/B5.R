## Question 5

# There are the two hypothesis for the problem.
# Null Hypothesis: H0: mean1 = mean2
# Alternative Hypothesis: H1: mean1!=mean2

m1 = c(16.03,16.04,16.05,16.05,16.02,16.01,15.96,15.98,16.02,15.99)
m2 = c(16.02,15.97,15.96,16.01,15.99,16.03,16.04,16.02,16.01,16.00)

##Calculating Mean

mean1 <- mean(m1)
mean2 <- mean(m2)
n1=10
n2=10
sd1<-sd(m1)
sd2<-sd(m2)
##Calculating  Variance

variance1 <- var(m1)
variance2 <- var(m2)

## Z Factor

z1 <- (m1 - mean1)/sd(m1)
z2 <- (m2 - mean2)/sd(m2)

sp= sqrt(((n1 - 1)*sd1^2 + (n2-1)*sd2^2)/(n1 + n2 - 2))

t0 <- (mean1 -mean2)/(sp*sqrt(1/n1 + 1/n2))
print(t0)

t1 = t.test(m1,m2) #hypothesis t-test > print(t1)
print(t1)

alpha = 0.05

t0.025 <- 2.101 ## Value from the z distribution table [t 0.025,18 = 2.101]

if (t0 <= t0.025) {
  print("Accept")
} else {
  print("Reject")
}


## since t0<=t0.025 and t0 is 0.79835 which  does not fall under critical region,we accept the null hypothesis


## t0 is 0.79835 which  does not fall under critical region,
##we accept the null hypothesis


## Question 5 ends

