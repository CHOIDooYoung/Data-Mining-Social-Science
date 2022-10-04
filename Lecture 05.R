# Lecture 05: "Cross Sectional, Panel, and Time Series Analysis"

# library for loading an excel file
library("readxl")
# Date data manipulation library
library("lubridate")
#R packages for data science
library("tidyverse")
# library for the correlation matrix with P-values
library("Hmisc")
# a descriptive statistics library
library("psych")


# library for time series and Panel (please install)
library("vars")
library("urca")
library("forecast")
library("zoo")
library("xts")
library("tsbox")
library("tseries")
library("stats")
library("foreign")
library("car") 


# Import Data
# Import Lecture 05_Sample
Source.file <- read_excel(file.choose())

# Check the head

head(Source.file)

#==============================================================
# Mission 1: Road to Regression, The cross-sectional data.
#==============================================================

# Select a year for the cross-sectional data

CS.data <- Source.file

# Change column name
# rename (dataframe, Newname = Oldname)
# CS.data <- rename(CS.data, date = sc)

# Format the dates with date format
CS.data$Date <- ymd(CS.data$Date)

# Extract cross-sectional data: In this tutorial, 
# we will create a 2015 cross-sectional data set.

head(CS.data)
CS.data <- CS.data[CS.data$Date == "2015-12-31",]

# Your task is to select a date from the year 2002.
# Use your unique assignment name. e.g., CS.data.2002

# Plotting Corruption and Government Stability: Classic Method

plot(CS.data$`Government Stability`, CS.data$Corruption) # Classic Method
# test regression for the abline (add a best fit line [regression line])
# lm (Y~X, data = source)

abline (lm (`Government Stability` ~ `Corruption`, data = CS.data))


# Plotting Corruption and Government Stability: Cosmetic Method

# Default
ggplot(CS.data, aes(x=`Government Stability`, y=Corruption)) + geom_point()

# Change the point size, and shape
ggplot(CS.data, aes(x=`Government Stability`, y=Corruption)) +
  geom_point(size=1, shape=18, color = "blue") +
  geom_smooth(method=lm, linetype="dashed", color="red")

#======================================================
# Plot with your data.
#======================================================


# ================================
# Create a correlation matrix
#================================

#* TIP *# 

# Remove columns from the data frame
# "subset" is when you want to extract or drop the column.
# '-' sign indicates dropping variables. 


CS.data02 <- subset(CS.data, select = -c(Date, Country))


# Create a correlation matrix

cor(CS.data02)

# Create a correlation matrix with p-values.
Cor.mat <- rcorr(as.matrix(CS.data02))
Cor.mat


# ================================
# Descriptive Statistics
#================================

describe(CS.data02)


# ================================
# Regression (OLS)
#================================

# Single regression (OLS)
# Estimate the model and same the results in object "ols"
# OLS <- lm (Y ~ X, data = data.source)


OLS01 <- lm (`Ethnic Tensions`~ `Religious Tensions`, data = CS.data02)
OLS01
summary(OLS01)

OLS02 <- lm (Corruption ~ `Bureaucracy Quality`, data = CS.data02)
OLS02
summary(OLS02)


# Your task, select your two variable and create a single OLS



# Multiple regression (OLS)
# OLS <- lm (Y ~ X1 + X2 + X3 + Xn, data = data.source)

OLS03 <- lm (`Ethnic Tensions`~ `Religious Tensions` + `Law and Order` + `Military in Politics`, data = CS.data02)
OLS03
summary(OLS03)


# Your task, select your multiple variable and create a multiple OLS

#==============================================================
# Mission 2: Road to Time Series
#==============================================================


# Select a country for the Time Series

TS.data <- Source.file

# Define name as UAE

TS.UAE <- TS.data

# Change column name
# rename (dataframe, Newname = Oldname)

# Apply date format to the dates

head(TS.UAE)

# Extract Time-Series data
# in this tutorial we cleate a Time series data with "UAE"
head(TS.UAE)
TS.UAE <- TS.UAE[TS.UAE$County %in% "UAE",]

head(TS.UAE)

# Remove Country Column from the data frame.
TS.UAE <- subset(TS.UAE, select = -c(Date, County))

# Data Cleansing

# TS.data$GDP <- na.fill(TS.data$GDP, 0)


# Data02 <- na.locf(Data01)
# Data02 <- na.fill(Data01, 0)
# Data02 <- na.omit(Data01)
# Data02 <- na.approx(Data01)
# Data02 <- na.spline(Data01)



# Visualization
plot(TS.UAE.GDP)
autoplot(TS.UAE.GDP) + ggtitle("GDP")

# Descriptive Statistics

TS.discriptive <- describe(TS.data)
TS.discriptive

#########################
# cross correlation
#########################


TS.UAE.CM <- rcorr(as.matrix(TS.UAE))
TS.UAE.CM
auto.arima()


##############################
# Cheking spurious regression
##############################

SP.OLS <- lm(TS.UAE$Corruption ~ TS.UAE$GDP + TS.UAE$`Government Stability`+ 
            TS.UAE$`Ethnic Tensions` + TS.UAE$`Military in Politics`, 
            data = TS.UAE)
summary(SP.OLS)



###################################################
# Always set the stationary to Null hypothesis 
# H0 : stationary
# H1 : Non stationary
###################################################

#TESTING FOR Stationary TEST

adf.test(TS.UAE01$GDP)
TS.GDP.diff <- diff(TS.UAE$GDP)
adf.test(TS.GDP.diff)
TS.GDP.diff <- diff(TS.GDP.diff)
adf.test(TS.GDP.diff)

adf.test(TS.UAE$`Government Stability`)
TS.Gov.Stab <- diff(TS.UAE$`Government Stability`)
adf.test(TS.Gov.Stab)

adf.test(TS.UAE$`Socioeconomic Conditions`)
adf.test(TS.UAE$`Investment Profile`)
adf.test(TS.UAE$`Internal Conflict`)
adf.test(TS.UAE$`External Conflict`)
adf.test(TS.UAE$Corruption)
adf.test(TS.UAE$`Military in Politics`)
adf.test(TS.UAE$`Religious Tensions`)
adf.test(TS.UAE$`Law and Order`)
adf.test(TS.UAE$`Ethnic Tensions`)
adf.test(TS.UAE$`Democratic Accountability`)
adf.test(TS.UAE$`Bureaucracy Quality`)


pp.test(TS.UAE$GDP)
TS.GDP.diff <- diff(TS.UAE$GDP)
pp.test(TS.GDP.diff)


pp.test(TS.UAE$`Government Stability`)
TS.Gov.Stab <- diff(TS.UAE$`Government Stability`)
pp.test(TS.Gov.Stab)

pp.test(TS.UAE$`Socioeconomic Conditions`)
pp.test(TS.UAE$`Investment Profile`)
pp.test(TS.UAE$`Internal Conflict`)
pp.test(TS.UAE$`External Conflict`)
pp.test(TS.UAE$Corruption)
pp.test(TS.UAE$`Military in Politics`)
pp.test(TS.UAE$`Religious Tensions`)
pp.test(TS.UAE$`Law and Order`)
pp.test(TS.UAE$`Ethnic Tensions`)
pp.test(TS.UAE$`Democratic Accountability`)
pp.test(TS.UAE$`Bureaucracy Quality`)



##############################
# Time Series and ARMA models
##############################

# arima(p,d,q) forecasting equation
# arima(1,0,0) = first-order autoregressive model
# arima(0,1,0) = random walk
# arima(1,1,0) = differenced first-order autoregressive model
# arima(0,1,1) without constant = simple exponential smoothing
# arima(0,1,1) with constant = simple exponential smoothing with growth
# arima(0,2,1) or (0,2,2) without constant = linear exponential smoothing
# arima(1,1,2) with constant = damped-trend linear exponential smoothing


#============================================================
# Convert to time-series form the data frame
# frequency = 1 is YEAR, 7 is WEEK, 12 is Month, 4 is Quarter
#=============================================================
head(TS.UAE)

# Define GDP as the forecasting variables.

# Convert as forecasting time series

TS.UAE.GDP <- ts(TS.UAE$GDP, frequency = 1, start = c(1984))
TS.UAE.GDP

# Check the data status

is.ts(TS.UAE.GDP)

# Plot the data

autoplot(diff(diff(TS.UAE.GDP)))

# Additional Tests

checkresiduals(diff(diff(TS.UAE.GDP)))

Box.test(diff(diff(TS.UAE.GDP, lag=1, type = c("Ljung-Box"))))

ggtsdisplay(diff(diff(TS.UAE.GDP)), main ="")

# Forecast

autoplot(forecast(diff(diff(TS.UAE.GDP))))


##############################
# Panel Analysis
##############################

# Bars at top indicates corresponding graph (i.e. countries)
# from left to right starting on the bottom row

head(Source.file)

# Assign as Panel data name
Panel.data <- Source.file

# Plot

coplot(GDP ~ Date|Country, type = "l", data = Panel.data) #Just with the line
coplot(GDP ~ Date|Country, type = "b", data = Panel.data) # With the dots

# Exploring panel data

scatterplot(GDP ~ Date|Country, boxplots=FALSE, smooth=TRUE, data=Panel.data)

# Panel OLS
head(Panel.data)
OLS.panel01 <- lm(Panel.data$Corruption ~ Panel.data$`Bureaucracy Quality`, data = Panel.data)
summary(OLS.panel01)

plot(Panel.data$`Bureaucracy Quality`, Panel.data$Corruption, pch=19, xlab="Bureaucracy Quality", ylab="Corruption")
abline(lm(Panel.data$Corruption ~ Panel.data$`Bureaucracy Quality`))

# Display with the factors
OLS.panel01.factors <- lm(Panel.data$Corruption ~ Panel.data$`Bureaucracy Quality` + factor(Country) - 1, data = Panel.data)
summary(OLS.panel01.factors)

# Plot
scatterplot(Corruption ~ `Bureaucracy Quality`|Country, boxplots=FALSE, smooth=TRUE, xlab = "`Bureaucracy Quality`", ylab = "Corruption", data=Panel.data)
abline(lm(Panel.data$Corruption ~ Panel.data$`Bureaucracy Quality`),lwd=3, col="red")
