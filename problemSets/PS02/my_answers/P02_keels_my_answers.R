#setting working directory
getwd()
setwd("/Users/sire/Desktop/Stats Prob 2/my_answers")
getwd()


#removing objects
rm(list=ls())

#detaching libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

#loading libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#installing packages
lapply(c(),  pkgTest)
library(tidyverse)

#setting my seed
set.seed(42)

#start of work

####### Task 1a)
#creating the matrix and naming it for better understanding.
x <- matrix(c(14, 7, 6, 7, 7, 1), ncol = 3)
  colnames(x) <- c("not stopped", "bribe requested", "stopped/given warning")
  rownames(x) <- c("upper class", "lower class")
print(x)

#creating the empty matrix to fill with the new values
xexp <- matrix(data = NA, ncol = 3, nrow =2)
  colnames(xexp) <- c("not stopped", "bribe requested", "stopped/given warning")
  rownames(xexp) <- c("upper class", "lower class")
print(xexp) #visually checking whether it is really empty

for (i in 1 : nrow(x)) {
  for (j in 1 : ncol(x)) {
    xexp[i, j] <- (sum(x[i, ]) * sum(x[, j])) / sum(x) #formula 
  }
}

print(xexp) #checking whether the matrix was filled and the test statistic done

#creating the empty matrix to fill for the chi-test
chi <- matrix(data = NA, ncol = 3, nrow = 2)
  colnames(chi) <- c("not stopped", "bribe requested", "stopped/given warning")
  rownames(chi) <- c("upper class", "lower class")
print(chi)

for (i in 1:nrow(chi)) {
  for (j in 1:ncol(chi)) {
    chi[i, j] <- ((x[i, j] - xexp[i, j])**2) / xexp[i, j] #formula for the values
  }
}

print(chi)

####### Task 1b)

chi_sqrd <- sum(chi) #getting the chi squared by summing it
print(chi_sqrd)

#getting the P-Value from the Chi-Test
p_chi_sqrd <- pchisq(chi_sqrd, df = (ncol(x) - 1) * (nrow(x) - 1), lower.tail=FALSE)
print(p_chi_sqrd)

chi_test <- chisq.test(x)
print(chi_test)

####### Task 1c)

#subsetting the standardised residuals form the chi_test
chi_test$stdres  


############### Problem 2
##### Problem 2: Data wrangling
rm(list=ls()) #clean up

#importing the data frame
df <- read.table("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header=T, sep = ",")

ls(df)  #getting an overview  
View(df) #getting an overview

boxplot(water ~ female, data = df, main = "res. vs. no-res.", ylim = c())
#the outliers make the boxplot rather unreadable therefore the ylim will be set

#check for effect plots in r

pdf("ps02plot1.pdf")
boxplot(water ~ female, data = df, main = "res. vs. no-res.", ylim = c(0, 60))
dev.off()

#The authors hypothesize that female politicians
#are more likely to support policies female voters want. 
#Researchers found that more women
#complain about the quality of drinking water than men. You need to estimate the effect
#of the reservation policy on the number of new or repaired drinking water facilities in the
#villages.

#HO: The reservation policy has no relation to more repaired drinking water facilities.
#HA: The reservation policy has a relation to more repaired drinking water facilities.

#2b) bivariate regression

correlation <- cor(df$water, df$reserved, method = "pearson") #correlation function
correlation

corrtest <- cor.test(df$water, df$reserved) #correlation test for more info
corrtest

summary(lm(df$water~df$reserved)) #getting the crucial data
#remark for myself: lm() = y and then x



      