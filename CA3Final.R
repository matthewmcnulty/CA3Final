# Importing dataset and replacing missing entries with "NA".
PopvPrice <- read.csv('20112016.csv', header = TRUE, 
                      stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Show the total number of rows,
# the first 10 rows of the data frame, 
# and the structure of the data frame.
nrow(PopvPrice)

head(PopvPrice, 10)
str(PopvPrice)

# Add a suitable title for each attribute of the data.
colnames(PopvPrice) <- c("County", "Date of Sale", "Total Sales", 
                         "Mean Price (€)", "Median Price (€)", "Mode Price (€)",
                         "Population", "Population Change", "Population Change (%)",
                         "Area (km^2)", "Population Density (/km^2)")

head(PopvPrice, 10)
str(PopvPrice)

# Show the total number and mean missing values 
# for each column in the data frame.
sum(is.na(PopvPrice))
colSums(is.na(PopvPrice))
sum(complete.cases(PopvPrice))

head(PopvPrice, 10)
str(PopvPrice)

# Cannot remove or replace missing entries in the 'Mode Price' 
# column as it is dependent on its respective county. 
# The value for 'Mode Price' cannot be calculated as the sample size is not large enough.

# Modify the 'County' and 'Date of Sale' attributes to be categorising factors.
PopvPrice$County <- factor(PopvPrice$County)
class(PopvPrice$County)

PopvPrice$`Date of Sale` <- factor(PopvPrice$`Date of Sale`)
class(PopvPrice$`Date of Sale`)

head(PopvPrice, 10)
str(PopvPrice)

# Descriptive statistics ----------------------------------------------
# we’ll look at measures of central tendency, variability
# and distribution shape for continuous variables
my_variables <- c("Total Sales", "Mean Price (€)", 
                  "Population", "Population Density (/km^2)")
head(PopvPrice[my_variables], 10)
str(PopvPrice[my_variables])


# Summary statistics
# provides the minimum, maximum, quartiles, and 
# mean for numerical variables and frequencies 
# for factors and logical vectors
summary(PopvPrice[my_variables])

# Descriptive statistics via sapply()
my_variables <- c("Total Sales", "Mean Price (€)", 
                  "Population", "Population Density (/km^2)")

# Our function that accepts in data (x)
# and outputs it as mean, no of values, SD
# skew and Kurtosis value

my_stats <- function(x, na.omit = FALSE) {
  if (na.omit)
    x <- x[!is.na(x)] # omit missing values
  m <- mean(x)
  n <- length(x) #no of values in x
  s <- sd(x) #SD of all values in each column
  skew <- sum((x - m) ^ 3 / s ^ 3) / n
  kurt <- sum((x - m) ^ 4 / s ^ 4) / n - 3
  return(c(n = n, mean = m, stdev = s, skew = skew, kurtosis = kurt))
}

# sapply(x, FUN, options) where x is the 
# data frame (or matrix) and FUN is an arbitrary function
head(PopvPrice[my_variables])
sapply(PopvPrice[my_variables], my_stats)

# Results show mean Total Sales is 102.038462, with a SD of 178.907355. 
# The distribution is skewed to the right(+4.472292) 
# and is sharper than a normal distribution (+22.547758).

# Results show mean Mean Price (€) is 1.553869e+05, with a SD of 6.588035e+04. 
# The distribution is skewed to the right(+1.867109e+00) 
# and is sharper than a normal distribution (+4.833429e+00).

# Results show mean Population is 1.797352e+05, with a SD of 2.480682e+05. 
# The distribution is skewed to the right(+3.706695e+00) 
# and is sharper than a normal distribution (+1.380929e+01).

# Results show mean Population Density (/km^2) is 106.16635, with a SD of 267.12248. 
# The distribution is skewed to the right(+4.57647) 
# and is sharper than a normal distribution (+19.63182).

# As the skewness values are greater than 1, the distribution is highly skewed.

# Show  graphically
install.packages("e1071")
library(e1071)
par(mfrow = c(2, 2)) # divides graph area into 2 cols

plot(density(PopvPrice$`Total Sales`), 
     main = "Density plot : Total Sales",
     ylab = "Frequency",
     sub = paste("Skewness : ", round(e1071::skewness(PopvPrice$`Total Sales`), 2)))

# Lets fill in the area under the density plot
polygon(density(PopvPrice$`Total Sales`), col = "blue")

plot(density(PopvPrice$`Mean Price (€)`), 
     main = "Density plot : Mean Price (€)",
     ylab = "Frequency",
     sub = paste("Skewness : ", round(e1071::skewness(PopvPrice$`Mean Price (€)`), 2)))

# Lets fill in the area under the density plot
polygon(density(PopvPrice$`Mean Price (€)`), col = "green")

plot(density(PopvPrice$Population), 
     main = "Density plot : Population",
     ylab = "Frequency",
     sub = paste("Skewness : ", round(e1071::skewness(PopvPrice$Population), 2)))

# Lets fill in the area under the density plot
polygon(density(PopvPrice$`Population`), col = "red")

plot(density(PopvPrice$`Population Density (/km^2)`), 
     main = "Density plot : Population Density (/km^2)",
     ylab = "Frequency",
     sub = paste("Skewness : ", round(e1071::skewness(PopvPrice$`Population Density (/km^2)`), 2)))

# Lets fill in the area under the density plot
polygon(density(PopvPrice$`Population Density (/km^2)`), col = "yellow")

# Using a QQ plot to check for normality
# qqnorm function plots your sample
# against a normal distribution
with(PopvPrice, {
  qqnorm(PopvPrice$`Total Sales`,
         main = "Total Sales")
  qqline(PopvPrice$`Total Sales`)
})

with(PopvPrice, {
  qqnorm(PopvPrice$`Mean Price (€)`,
         main = "Mean Price (€)")
  qqline(PopvPrice$`Mean Price (€)`)
})

with(PopvPrice, {
  qqnorm(PopvPrice$Population,
         main = "Population")
  qqline(PopvPrice$Population)
})

with(PopvPrice, {
  qqnorm(PopvPrice$`Population Density (/km^2)`,
         main = "Population Density (/km^2)")
  qqline(PopvPrice$`Population Density (/km^2)`)
})

# Formal test for normality
# using the Shapiro-wilks test
normality_test1 <- shapiro.test(PopvPrice$`Total Sales`)
normality_test1$p.value
normality_test2 <- shapiro.test(PopvPrice$`Mean Price (€)`)
normality_test2$p.value
normality_test3 <- shapiro.test(PopvPrice$Population)
normality_test3$p.value
normality_test4 <- shapiro.test(PopvPrice$`Population Density (/km^2)`)
normality_test4$p.value
# P value indicates whether the sample
# comes from a normal distribution
# p-value is clearly lower than 0.05
# so it is not normally distributed

# The variables of interest are all continuous variables. PDFs, QQPlots, and Shapiro
# tests have confirmed that none of them are normally distributed. This indicates that
# a non-parametric test is required. Therefore, the relationship between an independent population variable
# and a dependent residential property variable would correspond the the Spearman's
# Correlation Coefficient test.

install.packages("pwr")
library(pwr)

# It is suspected that there is a “large” positive linear relationship 
# between the independent and dependent variables. (Our minimum correlation is 0.6).
cohen.ES(test = "r", size = "large")

power_information <- pwr.r.test(r = 0.5, 
                                sig.level = 0.05,
                                power = 0.95)
power_information
plot(power_information)

# Population vs. Total Sales
cor.test(PopvPrice$Population, PopvPrice$`Total Sales`, method = "spearman")
# Population vs. Mean Price
cor.test(PopvPrice$Population, PopvPrice$`Mean Price (€)`, method = "spearman")
# Population Density vs. Total Sales
cor.test(PopvPrice$`Population Density (/km^2)`, PopvPrice$`Total Sales`, method = "spearman")
# Population Density vs. Mean Price
cor.test(PopvPrice$`Population Density (/km^2)`, PopvPrice$`Mean Price (€)`, method = "spearman")

# Like all correlation coefficients, Spearman’s rho 
# measures the strength of association between two variables. 

# Visualise if there are any linear relationships between the 
# independent and dependent variables
par(mfrow = c(1, 1)) # divides graph area into 2 cols
simple_linear_model <- lm(PopvPrice$Population ~ PopvPrice$`Mean Price (€)`, data = PopvPrice)
simple_linear_model

plot(x = PopvPrice$Population, 
     y = PopvPrice$`Mean Price (€)`,
     main = "Population ~ Mean Price (€)",
     xlab = "Population",
     ylab = "Mean Price (€)")

abline(simple_linear_model)

par(mfrow = c(1, 2)) # divides graph area into 2 cols
boxplot(PopvPrice$Population, 
        main = "Population", 
        sub = paste("Outlier rows ", 
                    boxplot.stats(PopvPrice$Population)$out))

boxplot(PopvPrice$`Mean Price (€)`, 
        main = "Mean Price (€)", 
        sub = paste("Outlier rows ", 
                    boxplot.stats(PopvPrice$`Mean Price (€)`)$out))

