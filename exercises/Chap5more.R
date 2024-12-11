# Chapter 5 examples

library(tidyverse)
names(diamonds)

## Some EDA plots

ggplot(diamonds, aes(color))+geom_bar() + facet_wrap(~cut)
ggplot(diamonds, aes(color))+geom_bar(aes(fill=cut))
ggplot(diamonds, aes(color))+geom_bar(aes(fill=cut))+ facet_wrap(~clarity)



library(vcdExtra)
vcd::mosaic(tab.data, shade=TRUE, legend=TRUE)

assoc(tab.data, shade=TRUE) 
strucplot(tab.data, core = struc_assoc)

sieve(tab.data)

library(gplots)
gplots::balloonplot(tab.data, main ="Balloon Plot", xlab ="", ylab="",
                    label = FALSE, show.margins = FALSE)

# A test of association
# 
chisq.test(tab.data)

chisq.test(tab.data)$expected
chisq.test(tab.data)$stdres

chisq.test(tab.data, simulate.p.value = TRUE)

# Square contingency tables (read SG)

counts <- c(123,2,0,0,1,48, 10,420,9,1,4,217, 2,21,102,1,5,54, 0,8,2,15,0,6,0,4,0,0,7,5,1,3,0,1,1,62)
lbl <- c("R1","R2", "R3", "R4", "R5", "R6")
tabledata = matrix(counts, ncol=6, nrow=6, byrow=T, dimnames = list(lbl,lbl))
dtm <- data.frame(tabledata)
dtm$Totals <- rowSums(dtm)
dtm["Totals",] <- colSums(dtm)
library(kableExtra)
kable(dtm, caption = " Crosstabulation of origin religion by current religion") %>% kable_styling(bootstrap_options = "striped", full_width = F)

chisq.test(tabledata)
chisq.test(tabledata, simulate.p.value = TRUE)

library(vcd)
agreementplot(tabledata)

# Cohen's kappa is the diagonal sum of the relative frequencies, corrected for expected values and standardized by its maximum value. In case of complete agreement, the kappa coefficient will be unity. 

Kappa(tabledata)


## Simpson's paradox

group1 <- matrix(c(80,120,30,80), nrow=2, ncol=2, byrow=T)
group1
chisq.test(group1)


group2 <- matrix(c(20,75,25,20), nrow=2, ncol=2, byrow=T)
group2
chisq.test(group2)


all <- matrix(c(80,120,30,80)+c(20,75,25,20), nrow=2, ncol=2, byrow=T)
all
chisq.test(all)


# for small tables
# fisher.test(tab.data)  

## Symmetric plot (old style graph)

library(MASS)
plot(corresp(tab.data, nf=2))
abline(v=0)
abline(h=0)


# Also using FactoMineR package
library("FactoMineR")
CA(tab.data)


## Three-way table
new.tab <- xtabs(~ cut + color+clarity, data=diamonds)
plot(new.tab)
cotabplot(new.tab , shade=TRUE, legend=FALSE)


library(ggmosaic)
ggplot(diamonds) +
  geom_mosaic(aes(x = product(cut, color))) 

ggplot(diamonds) +
  geom_mosaic(aes(x = product(cut, color))) +
    facet_wrap(~clarity)

ggplot(diamonds) +
  ggmosaic::geom_mosaic(aes(x = ggmosaic::product(cut, color), fill=clarity))+ 
    theme(axis.text.x = element_text(angle = 90))


## modelling - not covered in the course
#library(MASS)
MASS::loglm(~ cut + color, tab.data)
MASS::loglm(~ cut + color+clarity, new.tab)
