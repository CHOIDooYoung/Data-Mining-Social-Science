
library(dplyr)


x <- 1:5
x
x + 2
x + 6:10

print("Hello World")

7+3
x <- c(1,2,3,4,5)
x
x*y

file.choose()


library(readxl)
Sample01 <- read_excel(file.choose())
View(Sample01)

c(1,2,3,4,5)
1:5

Test.scores <- c(80, 85, 100, 90, 75)
sum(Test.scores)
mean(Test.scores)
median(Test.scores)
mode(Test.scores)

Test.scores


# Manipulating data frame (Columns and Rows in R)
A <- data.frame(Blood_type = c("A","A","B"), Body_weight = c(65,59,45))
BT <- A[, c("Blood_type")]
BT1 <- select (.data = A, Blood_type)
