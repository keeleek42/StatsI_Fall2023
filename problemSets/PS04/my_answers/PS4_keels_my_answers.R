#####################
#load libraries
#set wd
getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
#clear global .envir
#####################

#remove objects
rm(list=ls())

#detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

#load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

if(!require(wbstats)){
  install.packages("wbstats")
  library(wbstats)}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)}

if(!require(stargazer)){
  install.packages("stargazer")
  library(stargazer)}

if(!require(readr)){
  install.packages("readr")
  library(readr)}

install.packages("car")
library(car)

# here is where you load any necessary packages
# ex: stringr
lapply(c("stringr"),  pkgTest) #stringr is working

df <- data(Prestige)
help(Prestige) #checking the codebook
Prestige

##########
##########---
##########

#1a
Prestige #visual inspection
print(any(is.na(Prestige$type))) #checking whether there are nans
#there are some nans

Prestige <- Prestige[!is.na(Prestige$type), ] #getting rid of nans
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
head(Prestige)


#1b
str(Prestige)

model1 <- lm(prestige ~ income + professional + income*professional, 
             data = Prestige)

summary(model1)
stargazer(model1) #stargazing for latex

#1c

#1d

#1e
37.7812800 + 21.1422589 #predicted y

#1f
prestige1000 <- (21.1422589 + (0.0031709 * 1000) + (37.7812800*1) 
                 - (0.0023257 * 1 * 1000))
print(prestige1000)  

#1g
prestige6000_0 <- (21.1422589 + (0.0031709 * 6000) + (37.7812800*0) 
                   - (0.0023257 * 0))
prestige6000_0

prestige6000_p <- (21.1422589 + (0.0031709 * 6000) + (37.7812800*1) 
                      - (0.0023257 * 1 * 6000))
prestige6000_p

difference <- prestige6000_p - prestige6000_0
print(difference)

##########
##########---
##########

#2a
#Ho = Yard signs in precincts do not affec toutcome // B2= 0
#Ha = Yard signs in precincts affect outcome // B2 not 0

t1 <- (0.042 - 0) / 0.016 #coefficient / std error = t statistic
print(t1)
#hint: https://homework.study.com/explanation/the-t-statistic-is-computed-by-a-dividing-the-regression-coefficient-by-the-standard-error-of-the-estimate-b-dividing-the-regression-coefficient-by-the-standard-error-of-the-coefficient-c-dividing-the-standard-error-of-the-coefficient-by-the-regress.html#:~:text=Answer%20and%20Explanation%3A&text=The%20t%2Dstatistic%20is%20the,the%20coefficient%20is%20considered%20significant.
#t statistic = 2.625

## df = N − k = 131 − 3 = 128
#https://www.ucl.ac.uk/~uctp41a/b203/lecture8.pdf
#N observations = 131
#k parameters = 3 (2 variables + 1 intercept)
#https://www.statology.org/p-value-of-t-score-r/
p1 <- 2 * pt(t1, 128, lower.tail=FALSE)
p1


#2b
#Ho = Yard signs in adjacent precincts do not affect outcome // B2 = 0
#Ha = Yard signs in adjacent precincts affect outcome // B2 not 0

t2 = (0.042 - 0) / 0.013
print(t2)


p2 <- 2 * pt(t2, 128, lower.tail=FALSE)
print(p2)


#2c


#2d


