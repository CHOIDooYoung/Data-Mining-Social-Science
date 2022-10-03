

library(dplyr)
library(readxl)

# means comment

# integer
x <- 2L 

# Use L if you are not going to be adding then
# or you're never going to be creating, arithmetic
# operations, Just Put L after the number.

# typeof describes the types of  x have and
# in order to check the type all you
# have to do is type in "typeof ()" 

typeof(x)

#double
y <- 2.5

typeof(y)


# complex

z <-3 + 2i

typeof(z)

#character
a <- "h"
typeof(a)


#logical
q1 <- TRUE # or T
typeof(q1)
q2 <- FALSE # or F
typeof(q2)


# Using Variables
# How to save : Just Click Control + S (Windows), for MAC Command + S 

A <- 10
B <- 5
C <- A + B

C

print(C)

# Multiple selection, use your mouse to highlight. 
# variable 01
var1 <- 2.5
# variable 02
var2 <- 4


results <- var1 / var2
results


# sqrt is a function and the brackets
# indicate that the values inside are
# being passed on to this function. 

answer <- sqrt(var2)
answer 


# paste(): Takes multiple elements from the multiple vectors and concatenates them into a single element.
# paste0(): The paste0() function has space as its default separator and limits your opportunities in the output as well.

greeting <- "Hello"
name <- "Bob"
message <- paste (greeting, name)
message


# > paste(1,'two',3,'four',5,'six')
# [1] "1 two 3 four 5 six"
# > paste0('df',1:5,collapse = '_')
# [1] "df1_df2_df3_df4_df5"


# Logical Operators
# TRUE T
# FALSE F

4 < 5
10 > 100
4 == 5

# Logical Expression

# == equal to
# != not equal to 
# <
# >
# <=
# >=
# ! not
# | or 
# &
# isTRUE(x)

results <- 4 < 5
results
typeof(results)



results2 <- !(5 > 1)
results2 

results | results2
results & results2

isTRUE (results)


# CREATING LOOP


while(FALSE){
  
  print("Hello")
  
}


while(TRUE){
  
  print("Hello")
  
}

# to stop press ESC

#  You to perform certain actions such as loop
# for iterating certain operations

counter <- 1
while(counter < 7 ) {
  print(counter)
  counter <- counter + 1
  
}

# For Loop
for(i in 1:5){
  print("Hello World")
  
}

1:5

for(i in 5:10){
  print("Hello World")
  
}

5:10


# IF Statement

# ---- -2 ---- -1 ---- 0 ---- 1 ----- 2 -----

# rnorm: Vector of normally distributed random numbers.

rnorm(1)

# 2 nested statement 

rm(answer)
x <- rnorm(1)
if(x > 1){
  answer  <- "It is Greater than ONE"
} else {
  answer <- "It is Less than 1"
}

# 3 nested statement

rm(answer)
x <- rnorm(1)
if(x > 1){
  answer  <- "It is Greater than ONE"
} else {
  if (x >= -1){
    answer <- "Between -1 and 1"
  } else {
    answer <- "Less than -1"
  }
  }



#Chain Statement

rm(answer)
x <- rnorm(1)
if(x > 1){
  answer  <- "It is Greater than ONE"
} else if (x >= -1){
    answer <- "Between -1 and 1"
  } else {
    answer <- "Less than -1"
  }


# Manipulating Data Frame

hmnrghts<-read.table(file.choose(),
                     header=TRUE, na="NA")
head(hmnrghts)

afganistan <- hmnrghts[hmnrghts$country=="afganistan",]
afganistan <- hmnrghts[hmnrghts$country=="afganistan", c("country","democ")]

aletter <- hmnrghts[hmnrghts$country==c("afganistan", "albania"),]

