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


lapply(c("stats","stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc_sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")

##Q1
#Q1.1: Run regression
model1 <- lm(voteshare ~ difflog, data = inc_sub)
summary(model1)

#display output in LaTeX
stargazer(model1, type = "latex",
          title = "Regression of Vote Share on Logged Spending Difference",
          label = "tab:model1",
          style = "default",
          digits = 3,
          header = FALSE)

#Q1.2: Scatterplot with regression line
pdf("scatterplot_model1.pdf")
plot(inc_sub$difflog, inc_sub$voteshare,
     main = "Vote Share vs. Logged Spending Difference",
     xlab = "Logged Spending Difference (difflog)",
     ylab = "Incumbent Vote Share",
     pch = 19, col = "darkgray")

abline(model1, col = "blue", lwd = 3)
dev.off()
#Q1.3: Save residuals
residuals_model1 <- residuals(model1)
residuals_model1
#Q1.4: Prediction equation
# voteshare_hat = 0.579 + 0.0417 * difflog

##Q2

#Q2.1: Run regression
model2 <- lm(presvote ~ difflog, data = inc_sub)
summary(model2)
#display output in LaTeX
stargazer(model2, type = "latex",
          title = "Regression of Presidential Vote Share on Logged Spending Difference",
          label = "tab:model2",
          style = "default",
          digits = 3,
          header = FALSE)

#Q2.2: Scatterplot with regression line
pdf("scatterplot_model2.pdf")
plot(inc_sub$difflog, inc_sub$presvote,
     main = "Presidential Vote Share vs. Logged Spending Difference",
     xlab = "Logged Spending Difference (difflog)",
     ylab = "Presidential Vote Share",
     pch = 19, col = "darkgray")

abline(model2, col = "red", lwd = 3)
dev.off()

#Q1.3: Save residuals
residuals_model2 <- residuals(model2)
residuals_model2

##Q3

#Q3.1: Run regression
model3 <- lm(voteshare ~ presvote, data = inc_sub)
summary(model3)
#display output in LaTeX
stargazer(model3, type = "latex",
          title = "Regression of Incumbent Vote Share on Presidential Vote Share",
          label = "tab:model3",
          style = "default",
          digits = 3,
          header = FALSE)

#Q3.2: Scatterplot with regression line
pdf("scatterplot_model3.pdf")
plot(inc_sub$presvote, inc_sub$voteshare,
     main = "Incumbent Vote Share vs. Presidential Vote Share",
     xlab = "Presidential Vote Share (presvote)",
     ylab = "Incumbent Vote Share (voteshare)",
     pch = 19, col = "darkgray")
abline(model3, col = "green", lwd = 3)
dev.off()

##Q4

#Q4.1: Run regression of residuals
model4 <- lm(residuals_model1 ~ residuals_model2)
summary(model4)

#display output in LaTeX
stargazer(model4, type = "latex",
          title = "Regression of Residuals: Incumbent Vote Share on Presidential Vote Share",
          label = "tab:model4",
          style = "default",
          digits = 3,
          header = FALSE)

#Q4.2: Scatterplot with regression line
pdf("scatterplot_model4.pdf")
plot(residuals_model2, residuals_model1,
     main = "Residuals of Vote Share vs. Residuals of Presidential Vote Share",
     xlab = "Residuals from presvote ~ difflog",
     ylab = "Residuals from voteshare ~ difflog",
     pch = 19, col = "darkgray")
abline(model4, col = "orange", lwd = 3)
dev.off()

##Q5

#Q5.1: Run multiple linear regression
model5 <- lm(voteshare ~ difflog + presvote, data = inc_sub)
summary(model5)

#display output in LaTeX
stargazer(model5, type = "latex",
          title = "Regression of Incumbent Vote Share on Spending Difference and Presidential Vote Share",
          label = "tab:model5",
          style = "default",
          digits = 3,
          header = FALSE)


