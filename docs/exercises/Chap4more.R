url1 <- "http://www.massey.ac.nz/~anhsmith/data/rangitikei.RData"
download.file(url = url1, destfile = "rangitikei.RData")
load("rangitikei.RData")
attach(rangitikei)

# Old style
hist(people, probability=T)
curve(dnorm(x, mean(people), sd(people)), add= T,lty=2)

# ggplot style
library(ggplot2)
p1 <- ggplot(rangitikei, aes(people))+geom_histogram(aes(y=..density..), bins=10)
p1 + stat_function(fun = dnorm, args=list(mean=mean(rangitikei$people), sd=sd(rangitikei$people)), geom = "line")

# Old style
qqnorm(people)
qqline(people)

# ggplot style
ggplot(rangitikei, aes(sample=people))+stat_qq()+stat_qq_line()


shapiro.test(people)
ks.test(people, "pnorm")  # ties

t.test(people, mu=100)
t.test(people, mu=100, alternative="greater")

t.test(people~factor(time))


D1  <-  function(x) {(mean(x) - median(x)) / sd(x)}
D2  <-  function(x) {(mean(x) - median(x)) / (fivenum(x)[4] - fivenum(x)[2])}
D3  <-  function(x) {((fivenum(x)[4] + fivenum(x)[2]) / 2-median(x)) / (fivenum(x)[4] - fivenum(x)[2])}

x <- people
VMat <- cbind(
  Vreci = -1 / x,
  V = x,
  VSq = sqrt(x),
  VLog = log(x)
)
apply(VMat, 2, D1)



# require(MASS)
# b <- boxcox(x ~ 1)
# title("Log-likelihood curve of boxcox parameter")
# k <- b$x[which.max(b$y)]
# mtext(paste("optimum power=", formatC(k)))

library(lindia)
gg_boxcox(lm(people~1))

t.test(log(people), mu=log(100))

t.test(log(people)~factor(time))


# Variance stabilisation
url1 <- "http://www.massey.ac.nz/~anhsmith/data/WalkTheDogs.RData"
download.file(url = url1, destfile = "WalkTheDogs.RData")
load("WalkTheDogs.RData")
attach(WalkTheDogs)

library(lindia)
gg_boxcox(lm(Kcal+0.01~1))

WalkTheDogs$trans.Kcal  <-  Kcal^(1/3)

library(tidyverse)
p1 <- WalkTheDogs %>% group_by(Day) %>% 
  summarise(Means=mean(Kcal), SDs=sd(Kcal)) %>% 
  ggplot(aes(y=SDs, x=Means))+geom_point()
p2 <- WalkTheDogs %>% group_by(Day) %>% 
  summarise(Means=mean(trans.Kcal), SDs=sd(trans.Kcal)) %>% 
  ggplot(aes(y=SDs, x=Means))+geom_point()

library(patchwork)
p1+p2


