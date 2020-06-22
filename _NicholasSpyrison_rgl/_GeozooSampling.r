### Generate and save data for consumption in the app.
library("geozoo")
library("mvtnorm")

## Generates a solid cube with equidistant points between [-3, 3] in 3D
x <- 6 * (geozoo::cube.solid.grid(p = 3, n = 8)$points - .5)

## Create a function value of the sample cube, should be a "hill" in all projections
y <- mvtnorm::dmvnorm(x, mean = c(0,0,0))
#hist(y)

df <- data.frame(x, y)
colnames(df) <- c("x1", "x2", "x3", "y1")

save(df, file = "./data/df_func_surface1.rda")

### Going further: 
# - change the values of y. try a different var/covar matrix.
# - as a mixtures of y's