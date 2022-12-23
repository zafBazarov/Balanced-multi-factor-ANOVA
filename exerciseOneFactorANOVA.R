#########
##Analysis as single factor ANOVA
############################################

# data 
soils <- read.table ( "v14-u31-02.csv", header=T, sep=";", dec=",",
                      stringsAsFactors = T )
str(soils)

# model
soils$V <- as.fixed(soils$village)
m.4 <- lm (yield ~ V, data = soils)
gad(m.4)

# The treatment factor in the single factor ANOVA is fixed.

### Treatment means and honestly significant differences
va <- gad(m.4)
dfe <- va$Df[2]
mse <- va$"Mean Sq"[2]
alpha <- 0.05
n <- 4
a <- 6
sem <- sqrt(mse/(n))
sed <- sqrt(2) * sem
hsd <- qtukey( 1-alpha,a,dfe ) * sem


model.tables(aov(m.4),"means")
hsd
# Two villages are considered significantly different if their yield difference 
# is larger than 3.82.


####
## Compact letter display

# Display pairwise differences
attach(soils)
m <- tapply(yield,village,mean)
detach(soils)
source("v14-u31-00.R")
comp.lsmeans(m,hsd)

# tapply applies the function mean to the yield values and groups by the villages. Then we
#load an R script written by Prof. Frisch with source() and use the function comp.lsmeans()
#that it contains in order to create a compact letter display (CLD). If you are interested, open
#the file v14-u31-00.R and have a look at its contents (do not write!). It shows an example of
#how to write your own, customized R functions


