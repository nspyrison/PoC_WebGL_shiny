# Must be executed BEFORE rgl is loaded on headless devices.
options(rgl.useNULL=TRUE)

library(shiny)
library(rgl)
library(rayshader)
library(magrittr)

#Here, I load a map with the raster package.
loadzip = tempfile()
download.file("https://tylermw.com/data/dem_01.tif.zip", loadzip)
localtif = raster::raster(unzip(loadzip, "dem_01.tif"))
unlink(loadzip)

#And convert it to a matrix:
elmat = matrix(raster::extract(localtif,raster::extent(localtif),buffer=1000),
               nrow=ncol(localtif),ncol=nrow(localtif))
