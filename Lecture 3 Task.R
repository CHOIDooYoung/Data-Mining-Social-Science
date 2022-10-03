# Test the lw of large number for N random normally distributed numbers with
# mean = 0, Standard Divination (stdev) = 1

# Create an R script that will count how many of these numbers
# fall between -1 and 1
# and divide by the total equation of N

# You know that E(X) = 68.2%
# Check that Mean (XN) -> E(X) as your return script while increasing N

rnorm(100) 


N <- 1000000                   #Number of degree of freedom (Sample Size)
counter <- 0                   #Reset the counter
for (i in rnorm (N)){          #iterate over vector numbers
  if (i > -1 & i < 1) {        #Check where iterated variable falls
    counter <- counter + 1     #increase counter if condition is met
  }
}
  
answer <- counter / N           #Calculate hit-ratio
print(answer)                   #print answer in console
 
