# install.packages("dplyr)                       
# Install & load dplyr
# The dplyr package in R Programming Language is a structure of data manipulation
# that provides a uniform set of verbs, helping to resolve the most frequent 
# data manipulation hurdles.

library("dplyr")
library("tidyr")

# Descriptive Statistics library
library("psych")


# Data Frame Excel Export library  

library("writexl")


# Ploting Library
# library("ggpubr")
# library("ggplot2")



# Problem 1

# The data below shows the reading and math scores of 12 students.  

Read.score <- c(1, 1, 2, 3, 3, 4, 5, 6, 6, 7, 8, 8)
Math.score <- c(4, 7, 3, 8, 5, 7, 9, 4, 8, 10, 10, 9)
Problem.one <- data.frame(Read.score, Math.score)

cov(Problem.one)
cor(Problem.one)
describe(Problem.one)

# Problem 2
# The data below shows the hourly earnings (tips included) of 10 employees at a 
# bar and their attractiveness scores (0 = not at all attractive … 
# 10= extremely attractive).  

Attractiveness <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
Hourly.earning <- c(20, 24, 25, 26, 20, 30, 32, 38, 34, 40)
Problem.two <- data.frame(Attractiveness, Hourly.earning)

cov(Problem.two)
cor(Problem.two)
describe(Problem.two)


# Problem 3
# The data below shows the score on a promotion test given to police officers 
# and the number of hours studied. 

Hour.studied <- c(0, 1, 2, 3, 4, 6, 8, 16)
Promotion.score <- c(0, 0, 1, 4, 5, 6, 8, 8)
Problem.three <- data.frame(Hour.studied, Promotion.score)

cov(Problem.three)
cor(Problem.three)
describe(Problem.three)


# Problem 4
# The data below shows the height of students and the overall high school average. 

Height <- c(73, 79, 62, 69, 74, 77, 81, 63, 68, 74)
School.average <- c(100, 95, 90, 80, 70, 65, 60, 40, 30, 20)
Problem.four <- data.frame(Height, School.average)

cov(Problem.four)
cor(Problem.four)
describe(Problem.four)


# Problem 5
# The data below shows the number of pounds overweight and the hourly wage of 
# 10 employees working as secretaries in a law firm.   


OverWeight <- c(50, 30, 20, 20, 18, 13, 10, 4, 0, 0)
Hourly.wage <- c(12, 14, 15, 13, 15, 14, 20, 19, 22, 25)
Problem.five <- data.frame(OverWeight, Hourly.wage)

cov(Problem.five)
cor(Problem.five)
describe(Problem.five)
