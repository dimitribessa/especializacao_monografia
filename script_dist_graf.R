 #script para os gráficos de distribuição

func_devd <- function(x){
            params <- strip(x)
            devd(seq(72, 222,,100),
            params[1], params[3],params[4], type = 'GEV' )

}

func_gpd <- function(x){
            params <- strip(x)
           devd(seq(10, 60,,100),
           scale = params[1],shape = params[3] , threshold = 130,type = 'GP')
}

dados_devd <- purrr::map_df(list(mle_hist, mle_45, mle_85), function(x){
                densidade <- func_devd(x)
                data.frame(prec = seq(72, 222,,100), densidade)
}) 

dados_devd$cenario <- rep(c('Baseline', 'RCP 4.5', 'RCP 8.5'), times = 1, each = 100)


dados_gpd <- purrr::map_df(list(mle_histp, mle_45p, mle_85p), function(x){
              densidade <- func_gpd(x)
              data.frame(prec = seq(10,60,,100), densidade)      

})

dados_gpd$cenario <- rep(c('Baseline', 'RCP 4.5', 'RCP 8.5'), times = 1, each = 100)

#solução retirada de https://www.geeksforgeeks.org/add-common-legend-to-combined-ggplot2-plots-in-r/

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

graf_gdev <- 
ggplot(dados_devd, aes(x = prec, y = densidade)) +
geom_line(aes(colour = cenario)) + 
theme(legend.position =  'bottom') + 
labs(colour = 'Cenários', y = 'Densidade', x = 'mm/dia' , title = 'GEV')

legenda <- get_legend(graf_gdev)

graf_gpd <- 
ggplot(dados_gpd, aes(x = prec, y = densidade)) +
geom_line(aes(colour = cenario)) + 
theme(legend.position =  'hidden') + 
labs(colour = 'Cenários', y = 'Densidade', x = 'mm/dia', title = 'GP' )

graf_combined <- gridExtra::grid.arrange(graf_gdev + theme(legend.position = 'hidden'),
                        graf_gpd, ncol = 2)

gridExtra::grid.arrange(graf_combined, legenda, nrow = 2, heights = c(10,1))
