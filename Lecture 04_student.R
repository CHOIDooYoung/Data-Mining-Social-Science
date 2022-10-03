# install.packages("dplyr)                       
# Install & load dplyr
# The dplyr package in R Programming Language is a structure of data manipulation
# that provides a uniform set of verbs, helping to resolve the most frequent 
# data manipulation hurdles.

library("dplyr")
library("tidyr")


# library to manipulate date types and format 
library("lubridate")

# library to manipulate time-series data 
library("xts")
library("zoo")
library("forcats")
library("imputeTS")


# Manipulating excel files

library("readxl")
library("writexl")


#Plotting Library
library("ggpubr")
library("ggplot2")
library("corrplot")


#library for correlation matrix with P values
library("Hmisc")

#library for descriptive statistics
library("psych")

#=========================================
# Actual Cording Area
# Data Frame Manipulation with Time Series data. 
#=========================================

# Select a target data and load the data.
# owid-cvid-data.xlsx

Source.data <- read_excel(file.choose())

# Check the data

head(Source.data)

#Select Specific Row : Select Kenya 

Covid19.Daily.Kenya <- Source.data[Source.data$location %in% c("Kenya"), ]
Covid19.Daily.Kenya <- Source.data[Source.data$location %in% c("Kenya", "Angola"), ]
Covid19.Daily.Kenya2 <- Source.data[Source.data$location == "Angola", ]
Covid19.Daily.Kenya2 <- Source.data[Source.data$location == "Angola" | Source.data$location == "Kenya", ]

# TASK 01, Select a country from the SADC and GCC states


Covid19.Daily.SADC <- Source.data[Source.data$location == c("Angola"),]
Covid19.Daily.GCC <- Source.data[Source.data$location == c("United Arab Emirates"),]

# GCC states: "Bahrain", "Kuwait", "Oman", "Qatar", "Saudi Arabia", "United Arab Emirates"

# SADC states: "Angola", "Botswana", "Comoros", "Democratic Republic of Congo", "Eswatini", 
# "Lesotho", "Madagascar", "Malawi", "Mauritius", "Mozambique", "Namibia", "Seychelles", 
# "South Africa", "Tanzania", "Zambia", "Zimbabwe"



# Select Specific Columns : Select "location" and "new_cases"

Covid19.Daily.Kenya.newcase01 <- Source.data[Source.data$location =="Kenya", c("location","new_cases" )]
Covid19.Daily.Kenya.newcase02 <- Covid19.Daily.Kenya[Covid19.Daily.Kenya$location =="Kenya", c("location","new_cases" )]

# Task, Select a new_case column from above selected SADC and GCC states
# SADC 
Covid19.Daily.SADC.newcase01 <- Source.data[Source.data$location == c("Angola"),c("location","new_cases" )]
Covid19.Daily.SADC.newcase02 <- Covid19.Daily.SADC[Covid19.Daily.SADC$location == c("Angola"),c("location","new_cases" )]

#GCC
Covid19.Daily.GCC.newcase01 <- Source.data[Source.data$location == c("United Arab Emirates"),c("location","new_cases" )]
Covid19.Daily.GCC.newcase02 <- Covid19.Daily.GCC[Covid19.Daily.GCC$location == c("United Arab Emirates"),c("location","new_cases" )]


#=========================================
# Convert Daily Data to Monthly Data
#========================================



# Duplicate Data
Covid19.Monthly.Kenya  <- Covid19.Daily.Kenya 


# Set as date data
# Using "lubridate" library, function ymd
# it is able to switch the date system with dmy (day: month: year), mdy (month: day: year)
# default is ymd

Covid19.Monthly.Kenya$date <- ymd(Covid19.Monthly.Kenya$date)



# Convert to date data or confirm to date data
# Monthly.Data.Source$date <- ymd(Monthly.Data.Source$date)

# Check the Data
# floor_date(x,
#         unit = c("second", "minute", "hour", "day", "week", "month", "year"))


# Convert Daily data to Monthly data.

Covid19.Monthly.Kenya <- Covid19.Monthly.Kenya %>%
  group_by(Date = floor_date(date, 'month')) %>%
  summarise(new_cases = sum(new_cases, na.rm = T))


# TASK 03, Convert you SADC and GCC data to monthly from the daily


# you can save data frame data to excel data (requires library("writexl"))
write_xlsx(Covid19.Monthly.Kenya, "Covid19.Monthly.Kenya.xlsx")


# TASK 04, Save to excel data.


# =======================================
# Correlation Test With R
#========================================
# Covariance and Correlation

Stock.Market.Growth <- c(3, -2, 4, 6, 9)
Economic.Growth <- c(1, -1, 2, 3, 5)

df <- data.frame(Stock.Market.Growth, Economic.Growth)

COV.P <- cov (Stock.Market.Growth, Economic.Growth, method = "pearson")
COR.P <- cor (Stock.Market.Growth, Economic.Growth, method = "pearson")





# Missing Data Imputation

# There are several methods to imputation with 
# Library "zoo", "forecast" and 

#Functions:
# na.locf() : only for Time Series
# na.fill()
# na.trim()
# na.interp(): only for Time Series 
# tsclean(): only for Time Series
# na.omit() # listwise deletion


Source.data02 <- read_excel(file.choose())
hmnrghts <- Source.data02 

head(hmnrghts)

#Check the NA's with Summary, if there are NA's with the non-time series data, remove the line
summary(hmnrghts)

#removing the line

hmnrghts.no.NA <- na.omit(hmnrghts)

summary(hmnrghts.no.NA)

head(hmnrghts.no.NA)

# Covariance test With R
cov(hmnrghts.no.NA$democ, hmnrghts.no.NA$military,  method = "pearson")


# Correlation Test With R

cor(hmnrghts.no.NA$democ, hmnrghts.no.NA$military,  method = "pearson")


# Ploting Library

library("ggpubr")

head(hmnrghts.no.NA$democ)
ggscatter(hmnrghts.no.NA, x = "military", y = "democ", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Level of Military", ylab = "Level of Democracy")

# Visual inspection of the data normality using Q-Q plots 
# (quantile-quantile plots). Q-Q plot draws the correlation 
# between a given sample and the normal distribution.

# Democracy
ggqqplot(hmnrghts.no.NA$democ, ylab = "Democracy")
# Military
ggqqplot(hmnrghts.no.NA$military, ylab = "Military")


# Covariance and Correlation

Stock.Market.Growth <- c(3, -2, 4, 6, 9)
Economic.Growth <- c(1, -1, 2, 3, 5)

df <- data.frame(Stock.Market.Growth, Economic.Growth)

# TASK 04, create df's COV and COR

cor(df, method = "pearson")


# Compute correlation matrix


Corruption <- c(3, 2, 4, 6, 8)

df1 <- data.frame(Stock.Market.Growth, Economic.Growth, Corruption)

cor(df1, method = "pearson")


# Load Sample data... mtcars

data("mtcars") #default sample 
head(mtcars)

# Compute correlation matrix

mtcars.cm <- cor(mtcars)
print(mtcars.cm)

#round rounds the values in its first argument to the specified number of decimal places (default 0). 
round(mtcars.cm, 2)


# Hmisc's rcorr library displays Correlation matrix with significance levels (p-value)
mtcars.cm2 <- rcorr(as.matrix(mtcars))
mtcars.cm2



#Draw a correlogram
corrplot(mtcars.cm, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)



#  very simple library for descriptive statistics 
# use describe() for the descriptive statistics

library(psych)

describe(mtcars)

