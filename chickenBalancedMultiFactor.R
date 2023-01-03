#################
###########
#### CHICKEN
########################

# The effect of three diets on the weight gain of chicken was investigated. Twelve chicken houses
#(Hühnerställe) were randomly chosen. For each diet, four chicken houses were selected and the
#chicken were fed with the diet. The weight gain of five chicken per chicken house was assessed.
#Carry out an analysis of the data set.

#(a) Visualize the data with an appropriate plot. What do you see?
 # (b) What are the treatment factors? Which factor levels do they have?
  #(c) Is the experimental design crossed or hierarchical?
  #(d) Are the factors fixed or random?
  #(e) Is the weight gain different for the different diets? Does the chicken house have an influence on the weight gain?
  
#The data are available in the file v14-u31-04a.csv.

# Data

data <- read.table ( "v14-u31-04a.csv", header=T, sep=";", dec=",",
                     stringsAsFactors = T )
data

str(data)
# visualize the data

attach (data)

stripchart ( gain ~ diet:house,
             vertical=T, col="blue",
             data=data )

boxplot ( gain ~ diet:house)

## The GAD package allows to specify whether a factor is fixed or random:
library("GAD" ) # Provides: gad, as.fixed, as.random, estimates

# Also we have to enter about fixed fadfdfd
data$D <- as.fixed(data$diet)
data$H <- as.random (data$house)
data$C <- as.random(data$chicken)

# Plot interaction##
# We can check for interactions if we have two factors in a model:
with ( data, interaction.plot ( diet, house, gain, type="b", col="blue" ))
with ( data, interaction.plot ( house, gain, diet, type="b", col="blue" ))

# Linear model

m.1 <- lm ( gain ~ D + D:H, data=data) # C:V interaction between climate and variety
gad ( m.1 )


detach(data)

