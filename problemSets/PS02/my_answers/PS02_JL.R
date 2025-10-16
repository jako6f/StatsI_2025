#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
#### Problem 2

###Question 1

##Question1a: Calculate Chi-square manually

#storing the counts into a vector 

counts <- c(14, 6, 7, 7, 7, 1)   

#naming rows and columns
row_labels <- c("Upper class", "Lower class")
col_labels <- c("Not Stopped", "Bribe requested", "Stopped/given warning")

#creating the contingency table
contingency_table <- matrix(data = counts, nrow = 2, ncol = 3, byrow = TRUE, dimnames = list(Class = row_labels, Outcome = col_labels))
table_with_margins <- addmargins(contingency_table)

#extracting the totals
row_totals <- table_with_margins[, "Sum"][1:2]
col_totals <- table_with_margins["Sum", ][1:3]
grand_total <- table_with_margins["Sum", "Sum"]

#looping through calculating expected value for each cell
expected_values <- matrix(NA, nrow = 2, ncol = 3, dimnames = dimnames(contingency_table))

for (i in 1:nrow(expected_values)) {
  for (j in 1:ncol(expected_values)) {
    expected_values[i, j] <- (row_totals[i] * col_totals[j]) / grand_total
  }
}
print(expected_values)

#calculating Chi-square
chi_square <- sum(((contingency_table - expected_values)^2) / expected_values)
chi_square


##Question 1b: computing p-value

#calculating p-value
df <- (nrow(contingency_table) -1) * (ncol(contingency_table) - 1)
alpha <- 0.1
p_value <- 1 - pchisq(chi_square, df = df)
p_value
is_significant <- alpha > p_value
is_significant

##Question 1c: computing standardised residuals
chi_test <- chisq.test(contingency_table)
residuals <- chi_test$residuals
residuals
##Question 1d: interpreting standardised residuals
#If the Chi-square test is significant (which it wasn't), we know that the variables are related, but we don't know which specific categories are driving that relationship.
#The standardised residuals tell us how much the observed counts deviate from the expected counts, in units of standard deviation.
#Values close to 0 mean the observed count is very close to what would be expected under the null hypothesis (statistical independence).
#Values greater than abs(2) are considered large enough to indicate a significant deviation.
#In our case, none of the standardised residuals exceed abs(2), which confirms earlier finding (1c) that the Chi-Squared test was not statistically significant at alpha=0.10

###Question 2

##Question 2a:Hypothesis

#H0 assumes no difference between villages with reserved seats for women and those without. (H0: mu_reserved = mu_not_reserved)
#Ha assumes the policy does have an effect, meaning there is a difference, but does not specify the direction. (Ha: mu_reserved ≠ mu_not_reserved)


##Question 2b: Building a model

#load data
data_url <- "https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"
west_bengal_data <- read.csv(data_url)

#scatterplot
plot(west_bengal_data$reserved~west_bengal_data$water,main='Women in power and policy differences',xlab='reservation policy',ylab='number of new or repaired drinking water facilities',pch=20)

#build model
model <- lm(water ~ reserved, data = west_bengal_data)
summary(model)

# producing LaTeX code
install.packages("stargazer")
library(stargazer)

stargazer(model, type = "latex",
          title = "Bivariate Regression of Outcome on Reservation Policy",
          dep.var.labels = "Outcome",
          covariate.labels = c("Reserved (1 = Yes)"),
          digits = 3,
          no.space = TRUE)

##Question 2c: interpret model

#Coefficients:
#Beta0 (Intercept) 14.74 is the estimated average number of new water facilities in villages where the seat was NOT reserved (reserved = 0).
#Beta1 9.252 is the estimated causal effect of the reservation policy. It means that villages with a reserved seat had, on average, 9.252 more new or repaired water facilities than non-reserved villages.

#Significance:
#With ap-value of 0.0197 for Beta1, we reject H0 that the reservation policy had no effect. The policy is associated with a statistically significant increase in the number of water facilities. 

#Effect size
#An R-squared value of 0.01688 means that the reserved policy explains only about 1.69% of the total variance in the number of water facilities. While the effect is statistically significant, it's a very small factor in the overall variation of the outcome.

