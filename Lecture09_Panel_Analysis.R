# Install required packages

#Check if the required packages are installed, if not, install them

if (!require("plm")) install.packages("plm")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("foreign")) install.packages("foreign")
if (!require("readxl")) install.packages("readxl")
if (!require("car")) install.packages("car")
if (!require("tseries")) install.packages("tseries")
if (!require("corrplot")) install.packages("corrplot")

# Load packages

library(plm)
library(ggplot2)
library(foreign)
library(readxl)
library(car)
library(tseries)
library(corrplot)
library(psych)

# Load data
# Import Excel data file by opening a file dialog

data <- read_excel(file.choose())

# Create the line plot
#Plot GDP per capita by country and year

ggplot(data, aes(x = Year, y = GDP_per_capita, group = Country, color = Country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "GDP per Capita by Country (2010-2017)", x = "Year", y = "GDP per Capita") +
  theme_minimal() +
  theme(legend.title = element_blank())


#=============================================================================
# 1. Descriptive Statistics
#=============================================================================

# Calculate the mean and standard deviation of GDP per capita for each country
# Aggregate data by country, calculating mean and standard deviation for each

agg_data <- aggregate(GDP_per_capita ~ Country, data, function(x) c(mean = mean(x), sd = sd(x)))

# Print the aggregated data

print(agg_data)

# Create the box plot

# Plot GDP per capita distribution for each country using box plots

ggplot(data, aes(x = Country, y = GDP_per_capita, fill = Country)) +
  geom_boxplot() +
  labs(title = "GDP per Capita by Country (2010-2017)", x = "Country", y = "GDP per Capita") +
  theme_minimal() +
  theme(legend.title = element_blank())


#=============================================================================
# 2. Statistical Tests
#=============================================================================

#Calculate pairwise correlation coefficients and p-values
#Compute correlations and p-values for each pair of variables in the dataset

corr_results <- apply(data[, c("Unemployment_rate", "Political_stability_index", "GDP_per_capita", "Inflation_rate", "Govt_debt_to_GDP")], 2, function(x) {
  apply(data[, c("Unemployment_rate", "Political_stability_index", "GDP_per_capita", "Inflation_rate", "Govt_debt_to_GDP")], 2, function(y) {
    corr <- cor(x, y, method = "pearson")
    pval <- cor.test(x, y, method = "pearson")$p.value
    return(sprintf("%.2f (%.2e)", corr, pval))
  })
})

# Print correlation matrix with correlation coefficients and p-values

print(matrix(corr_results, ncol = length(corr_results), byrow = TRUE))



# Calculate pairwise correlation coefficients and p-values
# Compute correlations and p-values for each pair of variables in the dataset
cor_matrix <- cor(data[, c("Unemployment_rate", "Political_stability_index", "GDP_per_capita", "Inflation_rate", "Govt_debt_to_GDP")])

# Get p-values matrix using cor.mtest function
pval_matrix <- corrplot::cor.mtest(data[, c("Unemployment_rate", "Political_stability_index", "GDP_per_capita", "Inflation_rate", "Govt_debt_to_GDP")])$p

# Create a color palette based on p-values
col_pval <- colorRampPalette(c("darkred", "red", "white", "blue", "darkblue"))

# Create graph-based correlation matrix using circle method and p-value based color
corrplot(cor_matrix, method = "circle", type = "upper", col = col_pval(200), 
         p.mat = pval_matrix, sig.level = 0.05, insig = "blank",
         tl.col = "black", tl.srt = 45)



# Apply OLS for the test

# Perform OLS regression

model <- lm(Unemployment_rate ~ Political_stability_index + GDP_per_capita + Inflation_rate + Govt_debt_to_GDP, data)

# Print the summary of the regression results

summary(model)


# Generate predicted values from the model
data$predicted_Unemployment_rate <- predict(model, data)

# Scatter plot of actual Unemployment_rate vs predicted Unemployment_rate
ggplot(data, aes(x = predicted_Unemployment_rate, y = Unemployment_rate)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Scatter Plot: Actual vs. Predicted Unemployment Rate",
       x = "Predicted Unemployment Rate",
       y = "Actual Unemployment Rate") +
  theme_minimal()




# Fixed effects using Least squares dummy variable model

model.dum <- lm(Unemployment_rate ~ Political_stability_index + GDP_per_capita + Inflation_rate + Govt_debt_to_GDP + factor(Country), data)
summary (model.dum)

# Create the scatter plot with a regression line for each country
# Plot unemployment rate against inflation rate, with a separate regression line for each country

S.plot <- ggplot(data, aes(x = Inflation_rate, y = Unemployment_rate, color = Country)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  theme_minimal()
  labs(title = "Scatter Plot and Regression Line of Unemployment Rate and Inflation Rate by Country",
     x = "Inflation Rate",
     y = "Unemployment Rate",
     color = "Country")

# Print the plot

print(S.plot)

# Data exploration

names(data)
head(data)
summary(data)

# # Create an index for the panel data
# Create panel data frame with Country and Year as indexes

pdata <- pdata.frame(data, index = c("Country", "Year"))


# Create fixed effects model

fixed_model <- plm(Unemployment_rate ~ Political_stability_index + GDP_per_capita +
                     Inflation_rate + Govt_debt_to_GDP,
                   data = pdata, model = "within")

# Print the summary of the fixed effects model

summary(fixed_model)

# Create random effects model

Random_model <- plm(Unemployment_rate ~ Political_stability_index + GDP_per_capita +
                      Inflation_rate + Govt_debt_to_GDP,
                    data = pdata, model = "random")

# Print the summary of the random effects model

summary(Random_model)

# Pooled OLS Model
# Create pooled OLS model

Pooled_model <- plm(Unemployment_rate ~ Political_stability_index + GDP_per_capita +
                      Inflation_rate + Govt_debt_to_GDP,
                    data = pdata, model = "pooling")

# Print the summary of the pooled OLS model

summary(Pooled_model)

# Choose the best model using Hausman test
# Perform Hausman test to compare fixed and random effects models

hausman_test <- phtest(fixed_model, Random_model)
print(hausman_test)

# Diagnostic plots for the chosen model
# Plot the distribution of residuals

residuals_plot <- ggplot(data.frame(residuals = residuals(fixed_model)), aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", alpha = 0.8) +
  geom_density(color = "blue") +
  ggtitle("Residuals Distribution") +
  theme_minimal()

# Print the residuals distribution plot

print(residuals_plot)

# Plot residuals vs fitted values

residuals_vs_fitted_plot <- ggplot(data.frame(fitted = fitted(fixed_model), residuals = residuals(fixed_model)), aes(x = fitted, y = residuals)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(se = FALSE, method = "loess", color = "darkred") +
  ggtitle("Residuals vs Fitted") +
  theme_minimal()

# Print the residuals vs fitted values plot

print(residuals_vs_fitted_plot)

# Model predictions

# Predict unemployment rate using the fixed effects model

predicted_inv <- predict(fixed_model, newdata = pdata)
predicted_inv

# (Existing code above)

# Cook's Distance Plot
# Plot Cook's Distance to identify influential observations

cooksD <- cooks.distance(model)
plot(cooksD, pch = 16, main = "Cook's Distance Plot", xlab = "Observations", ylab = "Cook's Distance")
abline(h = 4 / (nrow(data) - length(coef(model)) - 1), col = "red", lty = 2)

# Added: Heteroskedasticity Test
# Perform Breusch-Pagan Test for Heteroskedasticity

library(lmtest)
bp_test <- bptest(fixed_model)
print(bp_test)

# Cook's Distance Plot
# Plot Cook's Distance to identify influential observations

cooksD <- cooks.distance(model)
plot(cooksD, pch = 16, main = "Cook's Distance Plot", xlab = "Observations", ylab = "Cook's Distance")
abline(h = 4 / (nrow(data) - length(coef(model)) - 1), col = "red", lty = 2)

# Added: Heteroskedasticity Test
# Perform Breusch-Pagan Test for Heteroskedasticity

library(lmtest)
bp_test <- bptest(fixed_model)
print(bp_test)


adf.test (diff(data$GDP_per_capita))
adf.test (diff(data$GDP_per_capita))

adf.test(data$Unemployment_rate)
adf.test(data$Inflation_rate)
adf.test(data$Political_stability_index)

