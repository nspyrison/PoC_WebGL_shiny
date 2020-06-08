library(spinifex)

dat <- tourr::rescale(flea[, 1:6])
col <- spinifex::col_of(flea$species)
pch <- spinifex::pch_of(flea$species) - 6

rb <- tourr::basis_random(n = ncol(dat))
cmass_tpath <- save_history(dat, guided_tour(cmass(), max.tries = 100), 
                            step_size = .6, rescale = FALSE)

play_tour_path(tour_path = cmass_tpath, data = dat, angle = .08, fps = 5,
               render_type = render_gganimate, col = col, pch = pch, 
               axes = "center")

