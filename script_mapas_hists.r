 #script para os mapas e histogramas (27u-jul-2023, 16:18h)
 #rodar antes os scritps de extração e operação de dados
 

 #funbções 

 #construção do gráfico
 graf_raster <- function(x, y, z = 'hidden'){
    
 ggplot()+
  geom_raster(aes(x=x,y=y,fill=layer),data=x)+
  geom_sf(fill='transparent',data=recorte)+
  paletteer::scale_fill_paletteer_c("ggthemes::Red-Blue-White Diverging", name  ='mm/dia')+
  #scale_fill_viridis_c('mm/dia',direction = -1)+
  coord_sf(expand=c(0,0))+
  labs(x='Longitude',y='Latitude',
       title=y
       )+
  #cowplot::theme_cowplot()+
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = 'dashed',
                                        size = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill=NA,color = 'black'),
        #panel.ontop = TRUE,
        legend.position = z) +
  #guides(fill = guide_colourbar(barwidth = 20)) +
  guides(fill = guide_colourbar(barwidth = unit(.8, 'npc'))) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.3) +
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
        #pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
        pad_x = unit(0.1, 'npc'), pad_y = unit(0.05,'npc'),
        style = north_arrow_fancy_orienteering)
 }

 #função para selecionar os pontos máximos do mapa
 
 func_max <- function(x, y){
               #if(!is.numeric(x)){x <- values(x)}
               #y <- values(y)
               ifelse(x >= y, x, y)
               }

 #cenário 45
 raster45 <- lapply(1:11160, function(x){dia <- sequencia45[1:8 + (x-1)*8]
                             lapply(1:8, function(x){values(dia[[x]])}) %>% #3 == prec
            Reduce('+',.)*1000}) 


 retiradas <- as.Date(datas, format = '%Y%m%d')
 raster45ii <- Reduce( func_max, raster45[!is.na(retiradas)[-11161]])

 raster45ii <-  raster(matrix(raster45ii,45,31, byrow = T), xmn=-50.65701, xmx=-48.5872, ymn=-27.9581, ymx=-26.20932, 
                      crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) 
 rasterdf45 <- as.data.frame(raster45ii, xy = T) %>% .[!is.na(.$layer),]

 graf_rcp45 <- graf_raster(rasterdf45, 'RCP 4.5')

 #cenário 85
 raster85 <- lapply(1:11160, function(x){dia <- sequencia85[1:8 + (x-1)*8]
                             lapply(1:8, function(x){values(dia[[x]])}) %>% #3 == prec
            Reduce('+',.)*1000}) 


 retiradas <- as.Date(datas, format = '%Y%m%d')
 raster85ii <- Reduce( func_max, raster85[!is.na(retiradas)[-11161]])

 raster85ii <-  raster(matrix(raster85ii,45,31, byrow = T), xmn=-50.65701, xmx=-48.5872, ymn=-27.9581, ymx=-26.20932, 
                      crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) 
 rasterdf85 <- as.data.frame(raster85ii, xy = T) %>% .[!is.na(.$layer),]

 graf_rcp85 <- graf_raster(rasterdf85, 'RCP 8.5', z = 'bottom')


 #período histórico
 raster_hist <- lapply(1:12960, function(x){dia <- sequenciahist[1:8 + (x-1)*8]
                             lapply(1:8, function(x){values(dia[[x]])}) %>% #3 == prec
            Reduce('+',.)*1000}) 

 retiradas <- as.Date(datas1975, format = '%Y%m%d')
 retiradas <- retiradas >= as.Date('1980-01-01')
 retiradas[is.na(retiradas)] <- F
 raster_histii <- Reduce( func_max, raster_hist[retiradas[-12961]])

 raster_histii <-  raster(matrix(raster_histii,45,31, byrow = T), xmn=-50.65701, xmx=-48.5872, ymn=-27.9581, ymx=-26.20932, 
                      crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))


 raster_histdf <- as.data.frame(raster_histii, xy = T) %>% .[!is.na(.$layer),]
 graf_hist <- graf_raster(raster_histdf, 'Baseline')

 #todos os mapas
 
 all_dfraster <- dplyr::bind_rows(list(raster_histdf, rasterdf45, rasterdf85))
 all_dfraster$cenario <- c(rep('Baseline',722), rep('RCP 4.5',722), rep('RCP 8.5',722))
 
 all_maps <- ggplot(data = all_dfraster)+
  geom_raster(aes(x=x,y=y,fill=layer))+
  geom_sf(fill='transparent',data=recorte)+
  paletteer::scale_fill_paletteer_c("ggthemes::Red-Blue-White Diverging", name  ='mm/dia')+
  #scale_fill_viridis_c('mm/dia',direction = -1)+
  coord_sf(expand=c(0,0))+
  labs(x='Longitude',y='Latitude'
       )+
  #cowplot::theme_cowplot()+
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = 'dashed',
                                        size = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill=NA,color = 'black'),
        #panel.ontop = TRUE,
        legend.position = 'bottom',
        strip.text.x = element_text(size = 18)) +
  guides(fill = guide_colourbar(barwidth = unit(.8, 'npc'))) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.3) +
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
        pad_x = unit(0.1, 'npc'), pad_y = unit(0.05,'npc'),
        style = north_arrow_fancy_orienteering) +
 facet_grid(.~cenario)
 #--------------------------------------------------------------------------------
 #histogramas

 #RCP 4.5
 #ggplot(data = rasterdf45, aes(x = layer)) +
 #geom_histogram( binwidth=10, fill="#999e9d", color="#e9ecef", alpha=0.9, 
 #breaks = round(seq(min(rasterdf45$layer), max(rasterdf45$layer),10))) 
 
 #abindo e salvando...
 png('hist_plot.png', height = 200)
 par(mfrow = c(1,3))
 hist_baseline <- hist(raster_histdf[,3], main = 'Baseline', xlab = 'mm/dia', ylab = 'Frequência') 
 hist_45 <- hist(rasterdf45[,3], main = 'RCP 4.5', xlab = 'mm/dia', ylab = 'Frequência') 
 hist_85 <- hist(rasterdf85[,3], main = 'RCP 8.5', xlab = 'mm/dia', ylab = 'Frequência') 

 all_hist <- recordPlot()
 par(mfrow = c(1,1))

 #gridExtra::grid.arrange(gridExtra::arrangeGrob(all_maps, all_hist), nrow = 2)
 png('mapas_raster.png', height = 350)
 all_maps
 dev.off()

