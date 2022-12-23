
##########################
### Hierarchical ANOVA: Soils experiment
##################################################################

# This experiment is different: We investigate the yield of six villages in three different regions.
# However, since a village can only be in one region, village 1 in region 1 is different from village
# 1 in region 2. We call such a design "nested" or "hierarchical" and we need to 
# use a different kind of analysis.

library("GAD" ) # Provides: gad, as.fixed, as.random, estimates

# The following example shows how to manually enter data if you have two factors.

yield <- c ( 5,6,7,10, 9,10,12,13, 10,11,11,12, 
           15,17,18,18,  3,6,7,8,  7,7,8,10)
region <- gl ( n=3, k=8, length=24, labels=c("I","II","III" ) )
village <- gl ( n=6, k=4, length=24, labels=c("a","b", "c","d", "e","f") )
field <- gl ( n=4, k=1, length=24, labels=c("f1","f2","f3","f4"))

d2 <- data.frame(region,village,field, yield)


# we can just download a data from our library
soils <- read.table ( "v14-u31-02a.csv", header=T, sep=";", dec=",",
                      stringsAsFactors = T )
str(soils)

attach(soils)

# display data
stripchart ( yield ~ village:region,
             vertical=T, col="blue",
             data=soils )

# Fixed and random factors:
soils$R <- as.fixed (soils$region)
soils$V <- as.random(soils$village)

# We are interested in the effects of the particular regions and therefore regard 
# them as fixed. We are generally interested in the effects of the villages but 
# not in the effects of the particular ones we have in our experiment so we 
# regard them as random.

# The villages are nested in the regions:

m <- lm ( yield ~ R + R:V, data=soils )
gad (m)
estimates (m)

# The last part of the output shows how the F tests in the ANOVA are done.
# The hierarchical design means that we cannot simply do them all with the residual variance

########
# Treatment means and LSD for the first level
###############################################

# Means of regions:

model.tables(aov(m),"means")
model.tables(aov(m),"effects")

# This command can be used to select the mean of only one factor level.

model.tables( aov(m), "means", cterms = "R")
model.tables( aov(m), "means", cterms = "R:V")

# ANOVA

# The LSD can only be estimated for fixed effects.

va <- gad(m); dfe <- va$Df[2]; mse <- va$"Mean Sq"[2]
b <- 2 ; n <- 4
sed <- sqrt(2*mse/(b*n))
alpha <- 0.05;
lsd <- sed * qt( 1-alpha/2,dfe )

# There are b D 2 villages in each region (i.e. two levels of factor II) and n D 4 fields in
#each village. The equation for the SED can be found on lecture slide v14-03-227. Note that
#the equation for the LSD is always the same but the equation for the SED depends on the
#experimental design and on the comparisons we want to make.
lsd

#Two regions are considered to be significantly different if the difference in the mean yield is
#larger than the LSD of 9.72 - which is not the case here. This is consistent with the result of
#the global test of the ANOVA in which no significance for the regions was detected.