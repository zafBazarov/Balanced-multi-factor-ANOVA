
#########################
## FERTILIZER
########################

# The effect of four fertilizers on the wheat yield should be investigated. 
# To draw conclusions on the wheat yield in general, five varieties were randomly 
# selected from all varieties grown in the area under investigation. For each
# variety three replications were carried out and the yield in [dt/ha] was assessed.


# data

yld <- c ( 57,46,28, 26,38,20, 39,39,43, 23,36,18, 48,35,48,
           67,72,66, 44,68,64, 57,61,61, 74,47,69, 61,60,75,
           95,89,90, 92,99,89, 91,98,62, 98,85,85, 78,95,89,
           92,88,99, 96,95,99, 96,93,98, 99,90,98, 99,98,99)

variety <- gl ( n=5, k=3, length=60, labels=c("v1","v2","v3","v4", "v5" ) )
fertilizer <- gl ( n=4, k=15, length=60, labels=c("f1","f2","f3", "f4") )
rep <- gl ( n=3, k=1, length=60, labels=c("r1","r2","r3"))

d1 <- data.frame(variety,fertilizer,rep,yld)

str(d1)
d1
# Two-factor ANOVA: Climate experiment

library("GAD" ) # Provides: gad, as.fixed, as.random, estimates

# or we can just download a data from our library

d2 <- read.table ( "v14-u31-05.csv", header=T, sep=";", dec=",",
                   stringsAsFactors = T )
str(d2)

# stripchart
# Only stripcharts can be used for the factor level combinations because there 
# are only three replications for each combination.

stripchart ( yld ~ fertilizer:variety,
             vertical=T, col="blue",
             data=d2 )

# Also we have to enter about fixed factors.
d2$F <- as.fixed (d2$fertilizer)
d2$V <- as.random (d2$variety)

# The factor "fertilizer" is fixed because we are interested in the effect of the specific
# fertilizers in the experiment. The factor "variety" is random because in order to "To draw
# conclusions on the wheat yield in general, five varieties were randomly selected from all
# varieties grown in the area under investigation".

# Model
m.1 <- lm ( yield ~ F + V + F:V, data = d2)
gad ( m.1 )

# Estimate the mean yields of the fertilizers
model.tables(aov(m.1),"means",cterms="F")

# pairwise comparison error
library("emmeans")
library("multcomp") # Provides: cld
library("dplyr") # Provides: summarise

# make a multiple comparison
emm.f <- emmeans (m.1, ~F)
test (contrast (emm.f, "pairwise", adjust= "none"))

test (contrast (emm.f, "pairwise", adjust= "tukey"))

# multiple pairwise comparison by groups
cld( emm.f, adjust= "tukey")

# Estimate standard error and df
snk.test(m.1,term ='F:V', among ='F', within ='V')
df <- 40
sem= 4.4796

# model 2
m.2 <- lm (yield ~ F, data = d2)
gad (m.2)

str(d2)
# LSD 0.1%
va<- gad (m.2)
dfe <- va$Df [2]
mse <- va$`Mean Sq`[2]
alpha <- 0.1
n <- 4
a <- 5
sem <- sqrt (mse/n)
sed <- sqrt(2)*sem
lsd <- qt( 1-alpha/2, dfe ) * sed
lsd
hsd <- qtukey( 1-alpha, n ,dfe ) * sem
hsd

# The varieties are a random factor. Means are only compared for fixed factors.
