#####################
# load libraries
# set wd
getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
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

# here is where you load any necessary packages
# ex: stringr
lapply(c("stringr"),  pkgTest) #stringr is working

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")
View(inc.sub) #checking out the data

#Task 1.1

model_1 <- lm(inc.sub$voteshare~inc.sub$difflog) #running a regression with the desired outcome
summary_1 <- summary(model_1) #saving it 

print(summary_1) #printing it
stargazer(model_1) #stargazing for easy transfer to latex

#Task 1.2

scatter_1 <- ggplot(data = inc.sub, 
         mapping = aes(x = difflog,
                       y = voteshare)) + 
  labs(x = "Difflog",
       y = "Voteshare",
       title = "Model 1: Voteshare and Difflog") +
  theme_minimal() + 
  geom_point(color = "grey27", size = .8) +
  geom_smooth(method = 'lm',col="maroon1")

scatter_1
ggsave("scatter_1.png", plot = scatter_1, dpi = 300,
       height = 3,
       width = 6)


#Task 1.3
residuals_1 <- model_1$residual #saving the subset residual 
#(subset residual: https://www.statology.org/extract-residuals-from-lm-in-r/)

#Task 1.4
summary_1 #taking the Y intercept 0.579 + 0.0417X1 + E

###################

#Task 2.1
model_2 <- lm(inc.sub$presvote~inc.sub$difflog) #running a regression with the desired outcome
summary_2 <- summary(model_2) #saving it 

print(summary_2) #printing it
stargazer(model_2) #stargazing for easy transfer to latex

#Task 2.2
scatter_2 <- ggplot(data = inc.sub, 
                    mapping = aes(x = difflog,
                                  y = presvote)) + 
  labs(x = "Difflog",
       y = "Presvote",
       title = "Model 2: Presvote and Difflog") +
  theme_minimal() + 
  geom_point(color = "grey27", size = .8) +
  geom_smooth(method = 'lm',col="maroon1")

scatter_2
ggsave("scatter_2.png", plot = scatter_2, dpi = 300,
       height = 3,
       width = 6)

#Task 2.3
residuals_2 <- model_2$residual #saving the subset residual (subset resiudal: https://www.statology.org/extract-residuals-from-lm-in-r/)

#Task 2.4
summary_2 #taking the Y intercept 0.508 + 0.024 + E

###################

#Task 3.1
model_3 <- lm(inc.sub$voteshare~inc.sub$presvote) #running a regression with the desired outcome
summary_3 <- summary(model_3) #saving it 

print(summary_3) #printing it
stargazer(model_3) #stargazing for easy transfer to latex

#Task 3.2
scatter_3 <- ggplot(data = inc.sub, 
                    mapping = aes(x = presvote,
                                  y = voteshare)) + 
  labs(x = "Presvote",
       y = "Voteshare",
       title = "Model 3: Voteshare and Presvote") +
  theme_minimal() + 
  geom_point(color = "grey27", size = .8) +
  geom_smooth(method = 'lm',col="maroon1")

scatter_3
ggsave("scatter_3.png", plot = scatter_3, dpi = 300,
       height = 3,
       width = 6)

#Task 3.3
summary_3 #taking the Y intercept 0.441 + 0.388X1 + E

###################

#Task 4.1
model_4 <- lm(residuals_1~residuals_2) #running a regression with the desired outcome
summary_4 <- summary(model_4) #saving it 

print(summary_4) #printing it
stargazer(model_4) #stargazing for easy transfer to latex

#Task 4.2
scatter_4 <- ggplot(data = inc.sub, 
                    mapping = aes(x = residuals_2,
                                  y = residuals_1)) + 
  labs(x = "Res.2 of Presvote & Difflog",
       y = "Res.1 of Voteshare & Difflog",
       title = "Model 4: Residuals 1 and Residuals 2") +
  theme_minimal() + 
  geom_point(color = "grey27", size = .8) +
  geom_smooth(method = 'lm',col="maroon1")

scatter_4
ggsave("scatter_4.png", plot = scatter_4, dpi = 300,
       height = 3,
       width = 6)

#Task 4.3
summary_4 #taking the Y intercept -1.942e-18 + 2.569e-01X1 + E
format(-1.942e-18, scientific = FALSE) #so I understand...
format(2.569e-01, scientific = FALSE)

###################

#Task 5.1
model_5 <- lm(inc.sub$voteshare~inc.sub$difflog + inc.sub$presvote) #running a multi-regression with the desired outcome
summary_5 <- summary(model_5) #saving it 

print(summary_5) #printing it
stargazer(model_5) #stargazing for easy transfer to latex

#Task 5.2
summary_5 #taking the Y intercept 0.449 + 0.036X1 + 0.257X2 + E



#just some visualisation to help me get confused
scatter_5 <- ggplot(data = inc.sub, 
                    mapping = aes(x = inc.sub$difflog + inc.sub$presvote,
                                  y = inc.sub$voteshare)) + 
  geom_point() +
  geom_smooth(method='lm', col="maroon1")

scatter_5 

scatter_6 <- ggplot(data = inc.sub, 
                    mapping = aes(x = residuals_2,
                                  y = residuals_1)) + 
  geom_point() +
  geom_smooth(method='lm', col="maroon1")

scatter_6

