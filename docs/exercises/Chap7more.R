download.file("http://www.massey.ac.nz/~anhsmith/data/volumeperception.RData", destfile = "volumeperception.RData")
load("volumeperception.RData")

# EDA

library(GGally)
ggpairs(volumeperception)

library(psych)
corr.test(volumeperception)

library(corrplot)
corrplot(cor(volumeperception),  type = "upper", method="number")

library(tidyverse)
library(corrr)
volumeperception %>% correlate() %>% network_plot(min_cor=0.2)

# Full regression
# Note the . notation

my.reg=lm(volume~., data=volumeperception)
summary(my.reg)

# Diagnostic plots
library(ggfortify)
autoplot(my.reg, which=c(1:4))
autoplot(my.reg, which=c(1:6))

# Detailed output
library(lessR)
reg(volume ~ max.width + min.width + elongation + height, data=volumeperception)

# ANOVA
anova(my.reg)

library(car)
Anova(my.reg, type=2)
Anova(my.reg, type=3)

# Subset selection
# Base R
drop1(my.reg,  test="F")
step(my.reg, direction="backward")

inimodel = lm(volume ~ 1, data=volumeperception)
add1(inimodel,  ~max.width, test="F")

step(inimodel, direction = "forward", scope = list(lower =~1, upper = ~max.width + min.width + elongation + height), data=volumeperception)

# Combination
step(my.reg)


#Other packages
library("MASS")
stepAIC(my.reg, direction="forward")
stepAIC(my.reg, direction="backward")

library(leaps)
model = regsubsets(volume ~ max.width + min.width + elongation + height, data=volumeperception)
library(HH)
summaryHH(model)
plot(model)


## Polynomial

my.poly <- lm(height ~ poly(max.width, 2,  raw=T), data= volumeperception)
summary(my.poly)

my.poly <- lm(height ~ poly(max.width,3,  raw=T) + poly(elongation, 2, raw=T), data= volumeperception)
summary(my.poly)




