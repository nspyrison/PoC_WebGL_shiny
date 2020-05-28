install.packages("rgl")
install.packages("rayshader") ## Asks for `rgdal` when upon runing example app;
install.packages("rgdal")
# Warning: In make_shadow(heightmap, shadowdepth, shadowwidth, background,  :
#   `magick` package required for smooth shadow--using basic shadow instead.; 
# consider running:
install.packages("magick")

### Notes:
# - takes about 20s to render in rStudio window
# - takes about 40s to render in chrome
# - slider: in code makes coords in 3 normal variates, but doesn't use.
# === Remarks after testing:
# - Cannot use ggplot2 themes!?, why?
# - WAY too slow for even simple "3d" ggplots.
# - max_err can cut storage by 99%,, but speed is [.5-1]x still >10 sec.
# - hard to get it to embed the widget correctly, evening copying Eric's example.

