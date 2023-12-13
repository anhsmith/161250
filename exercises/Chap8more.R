# Data Source
# Lucas, HL. 1956. Switchback trials for more than two treatments. Journal of Dairy Science, 39, 146-154.
# Sanders, WL and Gaynor, PJ. 1987. Analysis of Switchback Data Using Statistical Analysis System. Journal of Dairy Science, 70, 2186-2191.
# Dataset lucas.switchback {agridat}

# Variables
# cow -cow factor, 12 levels
# trt - treatment factor, 3 levels
# period - period factor, 3 levels
# yield - yield (FCM = fat corrected milk), pounds/day
# block - block factor
# Six cows were started together in block 1, then three cows in block 2 and three cows in block 3.


# Obtain data

load(url("http://www.massey.ac.nz/~anhsmith/data/lucas.RData"))
attach(lucas)

# Become familiar with the data
View(lucas)

# An EDA plot

library(ggplot2)
ggplot(lucas, aes(y=yield, x= block, color=cow, shape=trt)) + 
  geom_point() + facet_wrap(~period)

# Main effects plot

ggplot(data = lucas, aes(x = trt, y = yield)) +
  stat_summary(fun = mean, geom = "point",aes(group = 1)) +
  stat_summary(fun = mean, geom = "line", aes(group = 1)) + 
  geom_abline(intercept = mean(yield), slope=0)+ 
  theme_bw()+ggtitle("Main effect of trt") 

#Try main effects plots of other factors

# Interaction plot

ggplot(data = lucas, aes(x = trt, y = yield, group=period, colour=period)) +
  stat_summary(fun=mean, geom="point")+
  stat_summary(fun=mean, geom="line")+
  geom_abline(intercept = mean(yield), slope=0)+ 
  theme_bw()+ggtitle("trt*period interaction")

## SS for a full model

summary(aov(yield~ trt*block*period*cow))

# One-way ANOVA

summary(aov(yield~ trt))
library(ggfortify)
autoplot(aov(yield~ trt))

# Two-way ANOVA
summary(aov(yield~ trt*cow))

# Three-way ANOVA
summary(aov(yield ~ block + cow:block + period:cow:block + period:block + trt))

# A Nested ANOVA
summary(aov(yield ~ trt + cow + block + period+Error(block/period)))

# Analysis depends on the treatment and unit structures which can lead to confounding
# Focus on practical issues
# Approach empirically, particularly for interactions 

