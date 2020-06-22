### Generate and save data for consumption in the app.
library("geozoo")
library("mvtnorm")
require("scales")
#remotes::install_github("allisonhorst/palmerpenguins")

## Generates a solid cube with equidistant points between [-3, 3] in 3D
x <- 6 * (geozoo::cube.solid.grid(p = 3, n = 8)$points - .5)
colnames(x) <- c("x1", "x2", "x3")

## Create a function value of the sample cube, should be a "hill" in all projections
y1 <- mvtnorm::dmvnorm(x, mean = c(0, 0, 0))
y1 <- 6 * scales::rescale(y1) ## Normalize range to 0:6

df <- data.frame(x, y1)

if (F){
  save(df, file = "./_NicholasSpyrison_rgl/data/df_func_surface1.rda")
}


### Going further: 
# - change the values of y. try a different var/covar matrix.
# - mixtures of y's

### new covar matrix:
std_dat <- as.data.frame(tourr::rescale(na.omit(palmerpenguins::penguins[, 3:6])))
.pca_rot <- prcomp(std_dat)$rotation
.top_3_cols <- row.names(.pca_rot[order(-.pca_rot[, 1]), ])[1:3]

library("lqmm")
set.seed(123)
n <- nrow(x)
p <- ncol(x)
var_x <- var(x)[1, 1]
vc_mat <-  diag(p) #* var_x
lt_idx <- lower.tri(vc_mat)
.norm <- rnorm(n = p, mean = 0, sd = var_x^(1 / 4)) ## rnorm sample off diag cov matrix values
vc_mat[lt_idx] <- vc_mat[t(lt_idx)] <- .norm ## assign symetric vales to lower and uper triandgles
vc_mat <- lqmm::make.positive.definite(vc_mat)

y2 <- mvtnorm::dmvnorm(x, mean = c(0, 0, 0), sigma = vc_mat)
y2 <- 6 * scales::rescale(y2) ## Normalize range to 0:6
### Trivial mixture of y's; half y1, half y2
y1.5 <- .5 * y1 + .5 *y2
y1.5 <- 6 * scales::rescale(y1.5)

y3 <- .5 * (max(y1) - y1) + .5 * y2
y3 <- 6 * scales::rescale(y3)

df <- data.frame(x, y1, y2, y1.5, y3)

if (F){
  save(df2, file = "./_NicholasSpyrison_rgl/data/df_func_surface2.rda")
}
