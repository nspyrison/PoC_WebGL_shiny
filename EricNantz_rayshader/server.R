xPts <- runif(1000, 0, 1)
yPts <- runif(1000, 0, 1)
zPts <- runif(1000, 0, 1)

shinyServer(function(input, output, session) {
    
    # Expression that generates a rgl scene with a number of points corresponding
    # to the value currently set in the slider.
    output$sctPlot <- renderRglwidget({
        try(rgl.close())
        
        ambmat = ambient_shade(elmat)

        elmat_3d <- elmat %>%
          sphere_shade(texture = "desert") %>%
          add_water(detect_water(elmat), color="desert") %>%
          add_shadow(ray_shade(elmat,zscale=3,maxsearch = 300),0.5) %>%
          add_shadow(ambmat,0.5) %>%
          plot_3d(elmat,zscale=10,fov=0,theta=135,zoom=0.75,phi=45, windowsize = c(1000,800))
        render_snapshot()
        rglwidget()
    })
})
