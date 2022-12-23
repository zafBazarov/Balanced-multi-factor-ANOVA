#######################
##
## Exercise on Rape seed experiment

# Data

raps <- read.table ( "v14-u31-06.csv", header=T, sep=";", dec=",",
                     stringsAsFactors = T )
str(raps)

# Load required package

library("GAD" ) # Provides: gad, as.fixed, as.random, estimates

# We are interested in the effects of the genotypes and the environments in our 
# experiment and therefore regard those as fixed.

raps$G <- as.fixed(raps$geno)
raps$E <- as.fixed(raps$env)
raps$R <- as.random(raps$rep)

# The effects of the replications are treated as random since the effect of each 
# individual replication is not of interest in the experiment. The six areas can be
# regarded as a random sample of all possible effects of the greenhouse.

# We specify two main effects for genotype and environment and their interactions
# as well as an effect for the replication nested in the environment. G*E is short for G + E + G:E.

m <- lm ( hgt ~ G*E + E:R , data=raps )
gad (m)
estimates(m)

# The model requires modifications in the F tests for the ANOVA. The effects of the 
# genotypes and the environments are significant. There are no significant interactions 
# between genotype and environment. The effect of the replication is not significant.

model.tables(aov(m),"means",cterms="G")

# Here we have estimates of the means of the genotypes.

# The estimation of the standard error that is required for the LSD and HSD is not 
# as straightforward as before. We use snk.test() to determine the standard error 
# and the degrees of freedom. Simply ignore the rest of the output.

snk.test(m,term ="G")


# LSD
# (values from output of snk.test())
sed <- sqrt(2) * 2.345; dfe <- 76; alpha <- 0.05
( lsd <- sed * qt( 1-alpha/2,dfe ) )


###
# LSD for combination G:E
# Use this LSD for the comparison between the genotypes
# Treatment means for G*E interaction:

model.tables(aov(m),"means",cterms="G:E")

# Estimate standard error and df
snk.test(m,term ='G:E', among ='E', within ='G')

# LSD
# (values from output of snk.test())

sed <- sqrt(2)* 3.3163; dfe <- 76; alpha <- 0.05
( lsd <- sed * qt( 1-alpha/2,dfe ) )

# Use this LSD for the comparison between specific combinations of genotypes and environments.