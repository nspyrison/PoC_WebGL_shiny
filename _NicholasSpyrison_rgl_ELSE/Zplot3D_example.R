### following example for surf3D in: 
## browseURL("https://cran.r-project.org/web/packages/plot3D/vignettes/plot3D.pdf")


# install.packages("plot3D")
library("plot3D")

par(mfrow = c(2, 2), mar = c(0, 0, 0, 0))
# Shape 1
M <- mesh(seq(0, 6*pi, length.out = 80),
          seq(pi/3, pi, length.out = 80))
u <- M$x ; v <- M$y
x <- u/2 * sin(v) * cos(u)
y <- u/2 * sin(v) * sin(u)
z <- u/2 * cos(v)
surf3D(x, y, z, colvar = z, colkey = FALSE, box = FALSE)
# Shape 2: add border
M <- mesh(seq(0, 2*pi, length.out = 80),
          seq(0, 2*pi, length.out = 80))
u <- M$x ; v <- M$y
x <- sin(u)
y <- sin(v)
z <- sin(u + v)
surf3D(x, y, z, colvar = z, border = "black", colkey = FALSE)
# shape 3: uses same mesh, white facets
x <- (3 + cos(v/2)*sin(u) - sin(v/2)*sin(2*u))*cos(v)
y <- (3 + cos(v/2)*sin(u) - sin(v/2)*sin(2*u))*sin(v)
# Karline Soetaert 7
# Figure 4: Surface plots
z <- sin(v/2)*sin(u) + cos(v/2)*sin(2*u)
surf3D(x, y, z, colvar = z, colkey = FALSE, facets = FALSE)
# shape 4: more complex colvar
M <- mesh(seq(-13.2, 13.2, length.out = 50),
          seq(-37.4, 37.4, length.out = 50))
u <- M$x ; v <- M$y
b <- 0.4; r <- 1 - b^2; w <- sqrt(r)
D <- b*((w*cosh(b*u))^2 + (b*sin(w*v))^2)
x <- -u + (2*r*cosh(b*u)*sinh(b*u)) / D
y <- (2*w*cosh(b*u)*(-(w*cos(v)*cos(w*v)) - sin(v)*sin(w*v))) / D
z <- (2*w*cosh(b*u)*(-(w*sin(v)*cos(w*v)) + cos(v)*sin(w*v))) / D
surf3D(x, y, z, colvar = sqrt(x + 8.3), colkey = FALSE,
       border = "black", box = FALSE)
