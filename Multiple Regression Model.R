library(tidyverse)
library(stats)
library(ggplot2)
library(lmtest)
library(car)

# Load data into project

load(url("https://www.dropbox.com/s/cnwtcr096szm8im/omsba_5112_birthweight.rdata?raw=1"))

my_data <- birthweight
head(my_data)

# Q1. Summary statistics of data
summary(my_data)

# Q2a. Percentage of white babies = 78
white_freq <- sum(my_data$white == 1)
percent_white <- mean(my_data$white == 1) * 100

# Q2b. Coefficient on family income, using family income, sex of child, and race as the explanatory variables
model <- lm(bwght ~ faminc + male + white, data=my_data)
summary(model)
coefficients <- coef(model)
family_income_coefficient <- coefficients["faminc"]

# Q2c. Adding mother's education changes the degrees of freedom by 2

model <- lm(bwght ~ faminc + male + white + motheduc, data=my_data)
summary(model)
coefficients <- coef(model)
family_income_coefficient <- coefficients["faminc"]

# Q3. Regression: y=bwght and x=cigs, faminc, male, white, parity. F-statistic=15.3
model <- lm(bwght ~ cigs + faminc + male + white + parity, data=my_data)
f_statistic <- summary(model)$fstatistic

#Q4. The model is valid because the p-value is more than 0.10

#Q5. Histogram to show distribution 
ggplot(data=my_data, aes(x=bwght))+
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(x = "Brth Wight (ounces)", y = "Frequency", title = "Histogram of Birth Weight")

#Q6. My histogram is pretty evenly distributed, so at least one of the assumptions are fulfilled.

#Q7. Is there enough evidence to conclude that the effect of cigarette consumption during pregnancy is linearly related to birth weight at the 5% significance level?
model <- lm(bwght ~ cigs, data = my_data)
summary(model)
#extremely small p-value shows smoking does affect birth weight

#Q8. Let's check the same for family income and effect on birth weight
model <- lm(bwght ~ faminc, data = my_data)
summary(model)
# A very small p-value also shows family income direcly relates to birth weight

#Q9. What effect does being a boy have on birth weight?
model <- lm(bwght ~ male, data = my_data)
summary(model)
#p-value of 0.007091 shows a correlation in being male and birth weight

#Q10. Does being white have any correlation to a baby's birth weight?
model <- lm(bwght ~ white, data = my_data)
summary(model)
# A p-value of 2.081e-06 is also shows that being white correlates to birth weight

model <- (my_data)
model_summary <- summary(my_data)
r_squared <- (model_summary)

# Residual vs Fitted Plot
model <- lm(bwght ~ cigs + faminc + male + white + parity, data=my_data)
residuals <- residuals(model)
fitted <- fitted(model)
plot(fitted, residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")
