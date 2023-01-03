##############
## LIMPETS exercise
#####

# The oxygen uptake (sauer) of two limpet species was assessed in water with different salt
#concentration (Sokal and Rohlf 1995, p. 333). The following values in g per mg weight and
#minute were measured. The species (art) and the salt concentrations (konz) are considered as
#fixed factors.

# Example 

# The following example shows how to manually enter data if you have two factors.

data  <- c ( 7.16, 6.78, 13.60, 8.93, 8.26, 14.00, 16.10, 9.66,
             6.14, 3.86, 10.40, 5.49, 6.14, 10.00, 11.60, 5.80,
             
             5.20, 5.20, 7.18, 6.37, 13.20, 8.39, 10.40, 7.18,
             4.47, 9.90, 5.75, 11.80, 4.95, 6.49, 5.44, 9.90,
             
             11.11, 9.74, 18.80, 9.74, 10.50, 14.60, 11.10, 11.80,
             9.63, 6.38, 13.40, 14.50, 14.50, 10.20, 17.70, 12.30  )
species <- gl ( n=2, k=8, length=48, labels=c("scabra","digitalis") )
concentration <- gl ( n=3, k=16, length=48, labels=c("I","II","III") )
rep <- gl ( n=8, k=1, length=48, labels=c("r1","r2","r3","r4", "r5","r6","r7","r8"))

d2 <- data.frame(species,concentration,rep,data)

library("GAD" ) # Provides: gad, as.fixed, as.random, estimates

d1 <- read.table ( "v14-u31-03.csv", header=T, sep=";", dec=",",
                   stringsAsFactors = T )
str(d1)

attach (d1)

# A boxplot. 
boxplot ( sauer ~ konz:art )

# stripchart
stripchart ( sauer ~ konz:art,
             vertical=T, col="blue"      )

# Also we have to enter about fixed factors.
d1$K <- as.fixed (d1$konz)
d1$A <- as.fixed (d1$art)

# Model

m.1 <- lm ( sauer ~ K + A + K:A, data = d1)
gad ( m.1 )

example<- aov  ( sauer ~ K + A + K:A, data = d1) 
summary(example)
summary(m.1)

# Plot interaction##
# We can check for interactions if we have two factors in a model:

with ( d1, interaction.plot ( konz, art, sauer, type="b", col="blue" ))
with ( d1, interaction.plot ( art, konz, sauer, type="b", col="blue"  ))

# Tables of means and variances

model.tables ( aov(m.1), "means" )
model.tables ( aov(m.1), "effects" )

# in our model concentration K is significant  
model.tables(aov(m.1),"means",cterms="K")

# standard error and df


# lsd 0.05%
sed <- sqrt(2) * 0.773; dfe <- 42; alpha <- 0.05
( lsd <- sed * qt( 1-alpha/2,dfe ) )

# hsd 0.05%
sem <- 0.773
( hsd <- qtukey (1-alpha, 3, 42)* sem)

# lsd 0.1%
sed <- sqrt(2) * 0.773; dfe <- 42; alpha <- 0.1
( lsd <- sed * qt( 1-alpha/2,dfe ) )

# hsd 0.1%
sem <- 0.773
( hsd <- qtukey (1-alpha, 3, 42)* sem)


