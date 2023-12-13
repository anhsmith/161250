# Example based on WalkTheDogs data from the R package Stat2Data.
# Download data
url1 <- "http://www.massey.ac.nz/~anhsmith/data/WalkTheDogs.RData"
download.file(url = url1, destfile = "WalkTheDogs.RData")
load("WalkTheDogs.RData")

# Plotting Kcal against Steps
library(tidyverse)
ggplot(WalkTheDogs, aes(x=Steps, y=Kcal))+geom_point()


p1 <- WalkTheDogs %>% filter(Steps>5) %>% ggplot(aes(x=Steps, y=Kcal))+geom_point()
p2 <- WalkTheDogs %>% filter(Steps<=5) %>% ggplot(aes(x=Steps, y=Kcal))+geom_point()
library(patchwork)
p1+p2 + plot_annotation(title="Steps below 5 and above 5 groups")

# Fitting Tukey line
attach(WalkTheDogs)
line(Steps, Kcal)
ggplot(WalkTheDogs, aes(x=Steps, y=Kcal)) + geom_point()+geom_smooth(method = MASS::rlm)


# Showing the Tukey line
rline <- line(Steps, Kcal)
int.1 <- rline$coefficients[1]
slope.1 <- rline$coefficients[2]
ggplot(WalkTheDogs, aes(x=Steps, y=Kcal)) + geom_point() + 
  geom_abline(aes(slope =slope.1, intercept = int.1))

# Plot of residuals and fits
Residuals <- residuals(rline)
Fits <- fitted.values(rline)
dfm <- cbind.data.frame(Residuals, Fits)
ggplot(dfm, aes(x=Fits, y=Residuals))+geom_point()


# Basic median line
# This line is adjusted to get the Tukey line

source("http://www.massey.ac.nz/~kgovinda/eda/rline.R")
rline(Steps, Kcal)

# MASS package robust linear model fit
rlm.fit <- MASS::rlm(Kcal~Steps)
int.2 <- rlm.fit $coefficients[1]
slope.2 <- rlm.fit $coefficients[2]

ggplot(WalkTheDogs, aes(x=Steps, y=Kcal)) + geom_point() + 
  geom_abline(aes(slope =slope.2, intercept = int.2), col=2)

# or use geom_smooth
ggplot(WalkTheDogs, aes(x=Steps, y=Kcal)) + geom_point() + 
  geom_smooth(method = MASS::rlm)

# Show both lines
ggplot(WalkTheDogs, aes(x=Steps, y=Kcal)) + geom_point() + 
  geom_abline(aes(slope =slope.1, intercept = int.1))+
  geom_abline(aes(slope =slope.2, intercept = int.2), col=2)

# Another robust fit
library(robustbase)
lmrobfit <- lmrob(Kcal~Steps)
int.3 <- rlm.fit $coefficients[1]
slope.3 <- rlm.fit $coefficients[2]
ggplot(WalkTheDogs, aes(x=Steps, y=Kcal)) + geom_point() + 
  geom_smooth(method = robustbase::lmrob)

# Show All three lines
ggplot(WalkTheDogs, aes(x=Steps, y=Kcal)) + geom_point() + 
  geom_abline(aes(slope =slope.1, intercept = int.1))+
  geom_abline(aes(slope =slope.2, intercept = int.2), col=2)+
  geom_abline(aes(slope =slope.3, intercept = int.3), col=3)


# Simple Regression fit
my.reg <- lm(Kcal~Steps)

# Show the fit
ggplot(WalkTheDogs, aes(x=Steps, y=Kcal)) + geom_point() + 
  geom_smooth(method = lm)

# Old Style summary
summary(lm(Kcal~Steps))

# Tidy features
library(tidyverse)
library(broom)
library(data.table)
data.table(glance(my.reg)%>% 
             mutate_if(is.numeric, round, 2))
data.table(tidy(my.reg)%>% 
             mutate_if(is.numeric, round, 2))

#Residual plots
library(ggfortify)
autoplot(my.reg, which=1:4)
autoplot(my.reg, which=1:2)

library(ggplot2)
dfm <- cbind.data.frame(Residuals=residuals(my.reg), 
                        Std.residuals=rstandard(my.reg), 
                        Student.Residuals=rstudent(my.reg), 
                        Fitted.values=fitted.values(my.reg))

p1 <- ggplot(dfm, aes(x=Fitted.values, y=Residuals))+geom_point()+geom_smooth()
p2 <- ggplot(dfm, aes(x=Fitted.values, y=Std.residuals))+geom_point()+geom_smooth()
p3 <- ggplot(dfm, aes(x=Fitted.values, y=Student.Residuals))+geom_point()+geom_smooth()

library(patchwork)
p1/(p2+p3) + plot_annotation(title="Three kinds of residual vs fits plots")


