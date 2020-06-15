#install.packages("geozoo")
library("geozoo")

help(package = "geozoo")

## Generates a solid cube with equidistant points
x <- 6 * (cube.solid.grid(p = 3, n = 8)$points - .5)

library("mvtnorm")
help(package = "mvtnorm")
y <- dmvnorm(x, mean = c(0,0,0))
str(y)

## display 2D projections of X, and use y as the z value.
# should look like a hill, not matter the projection

# Next: 
# - can change the values of y. try a different var/covar matrix.
# - as a mixtures of y's