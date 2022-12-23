## Balanced multi-factor ANOVA
###############################

## Example 

# The following example shows how to manually enter data if you have two factors.

yld <- c ( 69,91,84,76, 66,56,58,60, 8,4,18,10, 44,24,30,22,
           118,96,80,106, 132,88,101,119)
var <- gl ( n=2, k=12, length=24, labels=c("a","b") )
cli <- gl ( n=3, k=4, length=24, labels=c("I","II","III") )
rep <- gl ( n=4, k=1, length=24, labels=c("r1","r2","r3","r4"))

d2 <- data.frame(var,cli,rep,yld)

# Two-factor ANOVA: Climate experiment

library("GAD" ) # Provides: gad, as.fixed, as.random, estimates

#The climate experiment has two "treatment" factors, variety with 2 levels (i D 1 or i D 2) and
#climate with 3 levels (j D 1; :::; 3). There are 4 replications (k D 1; :::; 4). It was conducted in
#a factorial design: Each variety was investigated in each climate. All possible combinations of
#the factor levels are present.

# or we can just download a data from our library

cl <- read.table ( "v14-u31-01.csv", header=T, sep=";", dec=",",
                   stringsAsFactors = T )
str(cl)

# Analysis of variance. 
# A stripchart is used to display a variation. 

attach (cl)
stripchart ( yield ~ climate:variety,
             vertical=T, col="blue",
             data=cl )
# We can conclude from the stripchart that climate has 
# a different effect on a and b regions.

# The GAD package allows to specify whether a factor is fixed or random:

# Also we have to enter about fixed factors.
cl$C <- as.fixed (cl$climate)
cl$V <- as.fixed (cl$variety)

# In our example, climate and variety are essential measures, and for this reason
# they are fixed factors 

# Note the difference 
class(cl$climate)
class(cl$C)

# The fixed factors must be specified for R Studio. Data have been acquired 
# for a fixed factor at all levels of interest. In other words, 
# if an experimenter is interested in three distinct degrees of a factor, 
# such as three distinct medical treatments, then each of those three 
# eatments has been evaluated as a condition.

# Two factors ANOVA

# Linear model

m.1 <- lm ( yield ~ C + V + C:V, data=cl) # C:V interaction between climate and variety
gad ( m.1 )

# We can conclude that all factors (C, V, and C:V) have a significant effect on yield. 
# because their p-values are smaller than the 5% level of significance.
# lm () better to use both random and fixed factors
# we can use also another command for linear model. 

example<- aov  ( yield ~ C + V + C:V, data=cl) 
summary(example)
summary(m.1)

# Plot interaction##
# We can check for interactions if we have two factors in a model:

with ( cl, interaction.plot ( climate, variety, yield, type="b", col="blue" ))
with ( cl, interaction.plot ( variety, climate, yield, type="b", col="blue" ))

###########
### Treatment means and effects
#############

# Tables of means and variances

model.tables ( aov(m.1), "means" )
model.tables ( aov(m.1), "effects" )

######
## Least significant differences
#############
#We need to use a t-test or Tukey test to identify differences between variables

# MSE and DF from the ANOVA table

mse <- gad(m.1)$'Mean Sq'[4] # Residual's Mean Square
mse
dfe <- gad(m.1)$'Df'[4]
dfe

# How do we know where these values come from?

alpha <- 0.05
n.v <- 12 ; k.v <- 2
n.c <- 8 ; k.c <- 3
n.cv <- 4 ; k.cv <- 6
sed.c <- sqrt ( 2*mse / n.c )
sed.v <- sqrt ( 2*mse / n.v )
sed.cv <- sqrt ( 2*mse / n.cv )
lsd.c <- qt( 1-alpha/2,dfe ) * sed.c
lsd.v <- qt( 1-alpha/2,dfe ) * sed.v
lsd.cv <- qt( 1-alpha/2,dfe ) * sed.cv
hsd.c <- qtukey( 1-alpha,k.c ,dfe ) * sed.c / sqrt(2)
hsd.v <- qtukey( 1-alpha,k.v ,dfe ) * sed.v / sqrt(2)
hsd.cv <- qtukey( 1-alpha,k.cv,dfe ) * sed.cv/ sqrt(2)

# k is the number of factor levels, n is the number of replications per factor level. 
# n * k is the total number of observations, 24 in this case. 
#The MSE comes from the ANOVA table.

# Different values for climate, variety, and the combinations:
lsd.c
lsd.v
lsd.cv
# To find differences between these values, simply compare the means of each 
# climate with the lsd.c number; if one climate mean is greater or less than the 
# other with the lsd.c number, there is a difference. For example, climate1 mean 
# (55) and climate2 mean (80), but lsd.c (12.7) is smaller than 15. For this
# reason, they are significantly different.


# Since we have two treatment factors (variety and climate), we need an LSD and HSD for each
# factor and a third for each factor level combination. You use lsd.c and hsd.c for comparisons
# between two climates, lsd.v and hsd.v for comparisons between two varieties and lsd.cv and
# hsd.cv for comparisons between two specific climate-variety combinations.
