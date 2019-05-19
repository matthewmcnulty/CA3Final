# Importing dataset and replacing missing entries with "NA".
# PopRes mean 'Population and Residential Properties'.
PopRes <- read.csv('20112016.csv', header = TRUE, 
                      stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Showing the total number of rows,
# the first 10 rows of the data frame, 
# and the structure of the data frame.
nrow(PopRes)

head(PopRes, 10)
str(PopRes)

# Adding a suitable title for each attribute of the data.
colnames(PopRes) <- c("County", "Date of Sale", "Total Sales", 
                         "Mean Price (€)", "Median Price (€)", "Mode Price (€)",
                         "Population of County", "Population Change", "Population Change (%)",
                         "Area (km^2)", "Population Density (/km^2)")

head(PopRes, 10)
str(PopRes)

# Showing the total number of mean missing values 
# for each column in the data frame.
sum(is.na(PopRes))
colSums(is.na(PopRes))
sum(complete.cases(PopRes))

head(PopRes, 10)
str(PopRes)

# Cannot remove or replace missing entries in the 'Mode Price' 
# column as it is dependent on its respective county. 
# The value for 'Mode Price (€)' cannot be calculated as the sample size is not large enough.

# Modify the 'County' and 'Date of Sale' attributes to be categorising factors.
PopRes$County <- factor(PopRes$County)
class(PopRes$County)

PopRes$`Date of Sale` <- factor(PopRes$`Date of Sale`)
class(PopRes$`Date of Sale`)

head(PopRes, 10)
str(PopRes)

# Creating a new subset called my_variables 
# Store within it only variables of interest.
# It is thought that "Population of County" and 
# "Population Density (/km^2)" could have an 
# affect on "Total Sales" and "Mean Price (€)".
my_variables <- subset(PopRes, 
                       select = c("Total Sales", 
                                  "Mean Price (€)", 
                                  "Population of County", 
                                  "Population Density (/km^2)"))

head(my_variables, 10)
str(my_variables)

# Summary statistics showing the minimum, maximum, quartiles, and 
# mean for variables of interest.
summary(my_variables)

# Creating a function that uses input data (x)
# and outputs its mean, no of values, standard deviation,
# skew, and kurtosis value.
my_stats <- function(x, na.omit = FALSE) {
  if (na.omit)
    x <- x[!is.na(x)] # omit missing values in x
  n <- length(x)
  m <- mean(x)
  s <- sd(x) # standard deviation of all values in x
  skew <- sum((x - m) ^ 3 / s ^ 3) / n
  kurt <- sum((x - m) ^ 4 / s ^ 4) / n - 3
  return(c(n = n, mean = m, stdev = s, skew = skew, kurtosis = kurt))
}

# Descriptive statistics of my_variables via sapply()
head(my_variables)
sapply(my_variables, my_stats)

# •	The mean of Total Sales is 102.038462, with a SD of 178.907355. 
# The distribution is skewed to the right(+4.472292) 
# and is sharper than a normal distribution (+22.547758).

# •	The mean of Mean Price (€) is 1.553869e+05, with a SD of 6.588035e+04. 
# The distribution is skewed to the right(+1.867109e+00) 
# and is sharper than a normal distribution (+4.833429e+00).

# •	The mean of Population of County is 1.797352e+05, with a SD of 2.480682e+05. 
# The distribution is skewed to the right(+3.706695e+00) 
# and is sharper than a normal distribution (+1.380929e+01).

# •	The mean of Population Density (/km^2) is 106.16635, with a SD of 267.12248. 
# The distribution is skewed to the right(+4.57647) 
# and is sharper than a normal distribution (+19.63182).

# As the skewness values are greater than 1, the distribution is highly skewed.

# Graphically show the distribution shape for continuous variables.
install.packages("e1071")
library(e1071)
par(mfrow = c(2, 2)) # divides graph area into 2 rows, 2 cols

# Distribution shape for 'Total Sales'
plot(density(PopRes$`Total Sales`), 
     main = "Density plot : Total Sales",
     ylab = "Frequency",
     sub = paste("Skewness : ", round(e1071::skewness(PopRes$`Total Sales`), 2)))

# Filling the area under the graph with colour
polygon(density(PopRes$`Total Sales`), col = "blue")

# Distribution shape for 'Mean Price (€)'
plot(density(PopRes$`Mean Price (€)`), 
     main = "Density plot : Mean Price (€)",
     ylab = "Frequency",
     sub = paste("Skewness : ", round(e1071::skewness(PopRes$`Mean Price (€)`), 2)))

# Filling the area under the graph with colour
polygon(density(PopRes$`Mean Price (€)`), col = "green")

# Distribution shape for 'Population of County'
plot(density(PopRes$`Population of County`), 
     main = "Density plot : Population of County",
     ylab = "Frequency",
     sub = paste("Skewness : ", round(e1071::skewness(PopRes$`Population of County`), 2)))

# Filling the area under the graph with colour
polygon(density(PopRes$`Population of County`), col = "red")

# Distribution shape for 'Population Density (/km^2)'
plot(density(PopRes$`Population Density (/km^2)`), 
     main = "Density plot : Population Density (/km^2)",
     ylab = "Frequency",
     sub = paste("Skewness : ", round(e1071::skewness(PopRes$`Population Density (/km^2)`), 2)))

# Filling the area under the graph with colour
polygon(density(PopRes$`Population Density (/km^2)`), col = "yellow")

# Graphs replicate the skewness and kurtosis found for each continuous variable.

# Using a QQ plot to check for normality. 
# Following code plots sample data against 
# that of a normal distribution.
par(mfrow = c(2, 2)) # divides graph area into 2 rows, 2 cols

# QQ plot for 'Total Sales'
with(PopRes, {
  qqnorm(PopRes$`Total Sales`,
         main = "Total Sales")
  qqline(PopRes$`Total Sales`)
})

# QQ plot for 'Mean Price (€)'
with(PopRes, {
  qqnorm(PopRes$`Mean Price (€)`,
         main = "Mean Price (€)")
  qqline(PopRes$`Mean Price (€)`)
})

# QQ plot for 'Population of County'
with(PopRes, {
  qqnorm(PopRes$`Population of County`,
         main = "Population of County")
  qqline(PopRes$`Population of County`)
})

# QQ plot for 'Population Density (/km^2)'
with(PopRes, {
  qqnorm(PopRes$`Population Density (/km^2)`,
         main = "Population Density (/km^2)")
  qqline(PopRes$`Population Density (/km^2)`)
})

# Each QQ plots shows each variable is right-skewed. 
# While the QQ plot for 'Population Density (/km^2)' looks
# like that of a normal distribution. A zoomed view will show
# that is slightly right-skewed. The other performed 
# tests for normality verify that it is not normally distributed.

# Formal test for normality
# using the Shapiro-wilks test.
normality_test1 <- shapiro.test(PopRes$`Total Sales`)
normality_test1$p.value
normality_test2 <- shapiro.test(PopRes$`Mean Price (€)`)
normality_test2$p.value
normality_test3 <- shapiro.test(PopRes$`Population of County`)
normality_test3$p.value
normality_test4 <- shapiro.test(PopRes$`Population Density (/km^2)`)
normality_test4$p.value
# All p-values are clearly lower than 0.05,
# so they are not normally distributed.

# The variables of interest are all continuous variables. 
# PDFs, QQ Plots, and Shapiro-wilks tests have confirmed that 
# none of them are normally distributed. This indicates that
# a non-parametric test is required. Therefore, the relationship 
# between an independent population variable and a dependent 
# residential property variable would correspond the Spearman's
# Correlation Coefficient test.

install.packages("pwr")
library(pwr)

# Null hypothesis, H0: The population level or population density of 
# a County does not affect the total number of sales or the 
# mean sold price of residential properties.

# Alternative hypothesis, H1: The population level or population density of 
# a County does affect the total number of sales or the 
# mean sold price of residential properties.

# H0: rho = 0
# H1: rho > 0

# It is suspected that there is a “large” positive linear relationship 
# between the population factors (independent) and price and 
# number of sales (dependent) variables.
cohen.ES(test = "r", size = "large")

# The effect size r is equal to 0.5.
# The usual significance level of 5% is chosen. A p-value
# less than this means that the null hypothesis H0 can
# be rejected and that the alternative hypothesis H1 can be
# accepted.
# Checking for an appropriate power value to make the most
# of the available data.
power_information <- pwr.r.test(n = 52,
                                r = 0.5, 
                                sig.level = 0.05)
power_information
# The power was set to 0.95 to reduce the probability of a
# type-2 error as much as possible.
power_information <- pwr.r.test(r = 0.5, 
                                sig.level = 0.05,
                                power = 0.95)
power_information
plot(power_information)
# The minimum sample size to effectively carry out 
# hypothesis testing is 46 observations.

# Population of County vs. Total Sales
cor.test(PopRes$`Population of County`, PopRes$`Total Sales`, method = "spearman")
# Population of County vs. Mean Price
cor.test(PopRes$`Population of County`, PopRes$`Mean Price (€)`, method = "spearman")
# Population Density vs. Total Sales
cor.test(PopRes$`Population Density (/km^2)`, PopRes$`Total Sales`, method = "spearman")
# Population Density vs. Mean Price
cor.test(PopRes$`Population Density (/km^2)`, PopRes$`Mean Price (€)`, method = "spearman")

# Like all correlation coefficients, Spearman’s rho 
# measures the strength of association between two variables. 

# Resulting p-value from Spearman Correlation Coefficient 
# between 'Population of County' and 'Mean Price (€)' is 5.498e-09, with 
# Spearman's rank correlation rho equal to 0.7209938.

# While all of the tests produce a p-value that is statistically significant,
# the 'Population of County' seems to have the strongest effect on 'Mean Price (€)'.
# Therefore, the null hypothesis H0 can be rejected and the alternative 
# hypothesis H1 can be accepted.

# Visualising the relationship between 'Population of County' and 'Mean Price (€)'. 
par(mfrow = c(1, 1)) # resets the graph area
simple_linear_model <- lm(PopRes$`Population of County` ~ PopRes$`Mean Price (€)`, data = PopRes)
simple_linear_model

plot(x = PopRes$`Population of County`, 
     y = PopRes$`Mean Price (€)`,
     main = "Population of County ~ Mean Price (€)",
     xlab = "Population of County",
     ylab = "Mean Price (€)")

abline(simple_linear_model)

# As seen from the graph, there are few obvious outliers. The data point
# that lies furthers from the linear model is Dublin. This is due to the 
# small relative size of the county and the amount of people living in it.

# Using a boxplot to determine the mathematical outliers 
# within both of the continuous variables.
par(mfrow = c(1, 2)) # divides graph area into 1 row, 2 cols
boxplot(PopRes$`Population of County`, 
        main = "Population of County", 
        sub = paste("Outlier rows ", 
                    boxplot.stats(PopRes$`Population of County`)$out))

boxplot(PopRes$`Mean Price (€)`, 
        main = "Mean Price (€)", 
        sub = paste("Outlier rows ", 
                    boxplot.stats(PopRes$`Mean Price (€)`)$out))

# 'Population of County' has 4 outliers,
# while 'Mean Price (€)' has 3.

