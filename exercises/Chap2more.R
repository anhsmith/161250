library(carData)
# data on migraine treatments collected by Tammy Kostecki-Dillon
help("KosteckiDillon")

## ----simple barchart----
library(ggplot2)
data(KosteckiDillon)
ggplot(KosteckiDillon, aes(x=hatype))+geom_bar()

## ------------------------------------------------------------------------
table(KosteckiDillon$medication)

## ----barcharts----

p1 <- ggplot(KosteckiDillon, aes(x=hatype))+geom_bar(aes(fill =medication))
p1 <- p1 + ggtitle("Bar plot with colour grouping")
p2 <- ggplot(KosteckiDillon, aes(x=hatype))+geom_bar()
p2 <- p2 + facet_grid(~medication)+ ggtitle("medication-wise Bar plots")
p3 <- ggplot(KosteckiDillon, aes(x=hatype))+geom_bar(aes(fill =medication), position = "dodge")
p3 <- p3 + ggtitle("Clustered Barplot")
p4 <- ggplot(KosteckiDillon, aes(x=hatype))+geom_bar(aes(fill =medication), position = "dodge")
p4 <- p4 + ggtitle("Clustered Bar plot - flipped")
p4 <- p4 + scale_fill_grey()+coord_flip()

library(patchwork)
p1+p2+p3+p4

# or
# library(gridExtra)
# grid.arrange(p1,p2, p3, p4, ncol=2)

## simple table
table(KosteckiDillon$sex, KosteckiDillon$headache)

## ---- mosaic plot-------------------------
tab <- table(KosteckiDillon$sex, KosteckiDillon$headache)
mosaicplot(tab, main="sex*headache counts", ylab="headache")

## ----Association  plot-----------------------
library(vcd)
assoc(tab, shade=TRUE, legend=TRUE) 

## ----Spine  plot-----------------------------
spine(tab) 

## ----Doubledecker  plot---------------------
doubledecker(age~sex+medication,  data=KosteckiDillon)

## ----dot plot for two groups-------
p1 <- ggplot(KosteckiDillon, aes(x=sex, y=time))
p1 <- p1 + geom_dotplot(binaxis='y', dotsize=.5) + coord_flip()
p1

## ----A jittered dot plot---------------------
p1 <- ggplot(KosteckiDillon, aes(x=sex, y=time))
p1 <- p1 +  geom_jitter(width = 0.1) + coord_flip()
p1

## ------A stem plot-----------------
stem(KosteckiDillon$airq)

## -------A detailed stem plot--------------------
## library(aplpack)
## stem.leaf(KosteckiDillon$airq)

## ----histogram--------------------
p1 <-  ggplot(KosteckiDillon, aes(airq))
p1 <- p1 + geom_histogram(bins=12, color="black", fill="white")
p1

## ----relative frequency histogram----
p1 <-  ggplot(KosteckiDillon, aes(airq))
p1 <- p1 + geom_histogram(bins=12, aes(y=..density..), color="black", fill="white")
p1

## ----frequency polygon------------
p1 <-  ggplot(KosteckiDillon, aes(airq))
p1 <- p1 + geom_freqpoly(bins=12, aes(y=..density..), color="black")
p1

## ----smoothed density curve------
p1 <-  ggplot(KosteckiDillon, aes(airq))
p1 <- p1 + geom_density()
p1

## ----Identification of Subgrouping----------
p1 <-  ggplot(KosteckiDillon, aes(time))
p1 <- p1 + geom_density(aes(linetype=sex, color=sex))
p1


# Five number summary
fivenum(KosteckiDillon$airq)


## ----Boxplots-----------------------------
p1 <-  ggplot(KosteckiDillon, aes(y=airq, x=sex))
p1 <- p1 + geom_boxplot(aes(linetype=sex, color=sex))
p1 + coord_flip()

## ----LV plots-----------------------------
p1 <- ggplot(KosteckiDillon, aes(y=airq, x=sex))
p1 <- p1 + geom_lv(aes(fill=..LV.. )) + scale_fill_lv()
p1+coord_flip()

## ----Cumulative Relative Frequency Curve----
n <- length(KosteckiDillon$airq)
row.ids <- 1:n
cum.frqs <- row.ids/n
dfm <- data.frame(cbind(x=sort(KosteckiDillon$airq), Fx=cum.frqs))
ggplot(dfm, aes(x=x, y=Fx))+geom_path()

## ----ECDF Curve for airq data-----------
ggplot(KosteckiDillon, aes(airq)) + stat_ecdf()


## -----Quantiles---------------
quantile(KosteckiDillon$airq, seq(0,1,0.1))


## ----Quantile-Quantile plots----
nq <- length(KosteckiDillon$airq)
p <- seq(1 , nq, length.out = 20) / nq - 0.5 / nq
X <- KosteckiDillon$airq[KosteckiDillon$sex=="male"]
Y <- KosteckiDillon$airq[KosteckiDillon$sex=="female"]
p1 <- ggplot() + geom_point(aes(x = quantile(X, p), y = quantile(Y, p)))
p1 <- p1 + geom_abline(slope=1, intercept=0)
p1 <- p1 + xlab("airq quantiles for males")
p1 <- p1 + ylab("airq quantiles for females")
p1

## ----Comparing many batches----------------
p1 <- ggplot(KosteckiDillon, aes(y=time, x=hatype))
p1 <- p1 + geom_boxplot()
p1+coord_flip()

## ----Comparing jittered batches-------------
p1 <- ggplot(KosteckiDillon, aes(y=time, x=""))
p1 + geom_jitter()+ facet_grid(hatype ~ .)

## ----Scatter plot----
p1 <- ggplot(KosteckiDillon, aes(y=age, x=airq))
p1 <- p1+geom_point()
p1

## ----marginal plot-------------
library(ggExtra)
p1 <- ggplot(KosteckiDillon, aes(y=age, x=airq))
p1 <- p1+geom_point()
ggMarginal(p1, type="boxplot")

## ----Scatterplot with a grouping variable----
p1 <- ggplot(KosteckiDillon, aes(y=age, x=airq))
p1+geom_point(aes(color = hatype))


## ---------
library(GGally)
library(tidyverse)
KosteckiDillon %>% dplyr::select(time, dos, age, airq, medication) %>% ggpairs(mapping=ggplot2::aes(color=medication))


## ---- Exploring bivariate distributions----
## Old style graphs
y = KosteckiDillon$age
x = KosteckiDillon$airq
def.par = par(mfrow = c(2, 2))

plot(x, y, xlim = c(0,100), ylim = c(0,100),
   main = "Scatter plot of age vs airq")

library(MASS)
f1 = kde2d(x, y, n = 50, lims = c(0, 100, 0, 100))
image(f1,  col=gray((0:32)/32), main = "Image Plot")
contour(f1,  main = "Density contourPlot")

op = par(mar=c(0,0,2,0)+.1)
persp(f1, phi = 45, theta = 30, zlab = "density",
col = "gray", shade = .5, border = NA,
main = "Perspective plot (density estimation)")
par(op)
par(def.par)

## ----A bubble plot------
ggplot(KosteckiDillon, aes(x=airq, y=time, colour=age, size=dos))+geom_point()

## ----A 3-D plot--------------------------------
library(lattice)
cloud(age ~ airq+time, group=medication, data=KosteckiDillon)

## ----A co-plot------------------------------
coplot(age ~ airq | time*medication, data = KosteckiDillon)

## ----faceplot, echo=FALSE, messages=FALSE, fig.cap='A Cartoon Faces plot', out.width='100%'----
my.data <- KosteckiDillon %>% filter(sex=="female", hatype=="Aura", age>60)
my.data <- my.data %>% dplyr::select(time,dos,age,airq)
library(aplpack)
faces(my.data)

## ----bi plot-------------------------
fit <- princomp(my.data)
biplot(fit)



