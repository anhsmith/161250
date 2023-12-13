library(carData)
# data on migraine treatments collected by Tammy Kostecki-Dillon
# help("KosteckiDillon")
data(KosteckiDillon)


#Checking normality for airquality data (KosteckiDillon$airq)

library(tidyverse)
p1 <- ggplot(KosteckiDillon, aes(airq)) 
p1 <- p1 + geom_histogram(aes(y=..density..), bins=10)
p1 + stat_function(fun = dnorm, 
                   args=list(mean=mean(KosteckiDillon$airq), 
                             sd=sd(KosteckiDillon$airq)), 
                   geom = "line")
p1

ggplot(KosteckiDillon, aes(sample=airq)) + 
  stat_qq() + stat_qq_line()

library(car)
qqPlot(KosteckiDillon$airq, 
       distribution="norm", 
       groups=KosteckiDillon$sex)


shapiro.test(KosteckiDillon$airq)

ks.test(KosteckiDillon$airq, "pnorm", 
        mean(KosteckiDillon$airq), 
        sd(KosteckiDillon$airq))



library(fitdistrplus)
m0 <- fitdist(KosteckiDillon$airq, "norm")
plot(m0)
m1 <- fitdist(KosteckiDillon$airq, "lnorm")
plot(m1)
m2 <- fitdist(KosteckiDillon$airq, "gamma")
plot(m2)
m3 <- fitdist(KosteckiDillon$airq, "weibull")
plot(m3)

# Cullen and Frey plot

descdist(KosteckiDillon$airq)

# Density Comparison

denscomp(list(m0,m1,m2,m3), 
         legendtext = c("normal",  
                        "lognormal", 
                        "gamma", 
                        "Weibull"))

# Goodness of fit criteria

gofstat(list(m0,m1,m2,m3), 
        fitnames= c("normal",  
                       "lognormal", 
                       "gamma", 
                       "Weibull"))
