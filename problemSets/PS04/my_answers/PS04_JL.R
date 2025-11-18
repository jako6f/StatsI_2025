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
lapply(c("car", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##Question 1

#preliminaries
data(Prestige)
help(Prestige)

total_cells <- nrow(Prestige) * ncol(Prestige)
total_nas <- sum(is.na(Prestige))
proportion_na <- total_nas / total_cells
percentage_na <- proportion_na * 100
percentage_na 
# proportion of NAs < 2% -> we can safely omit missing cases relying on R's default listwise deletion when running a regression

# Q1a) create dummy variable
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

# Q1b) run the linear model with interaction
model1 <- lm(prestige ~ income * professional, data = Prestige)
summary(model1)
  
  #latex documentation
  stargazer(model1, type = "latex",
            title = "Regression of Prestige on Income, Professional, and Interaction",
            style = "default",
            digits = 3,
            header = FALSE)



