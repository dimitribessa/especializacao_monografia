 #operando os dados extraídos do modelo (rodar o extraindo_dados.R antes) - 06-jul-2023, 15:33h

 #.libPaths('D:\\Dimitri\\Docs 17-out-15\\R\\win-library\\3.1') #caminho dos pacotes
  # Carregando os pacotes
 #library('xlsx')         #para ler/criar arquivos xlsx
 library('ggplot2')      #Gráficos (mais usado)
 library('reshape2')     #para remodular data.frames
 library('dplyr')        #manipulação de dados - tydiverse
 library('stringr')      #funções de string  - tydiverse
 library('magrittr')     #para mudar nome de colunas
 library('xtable')       #exportar df para tex
 
 library('extRemes') #cálculo de valores extremos
  #funçoes espaciais
 library('rgeos') #leitura de mapas
 library('rgdal') #leitra de mapas
 library('RColorBrewer')
 library('raster')
 library('leaflet')
 library('sp')
 library('sf')
 library('ggspatial') #pacote para itens de mapas no ggplot

 #Block Maxima
 #obtendo os valores máximos mensais
 
 #rcp45
 prec45$ano_mes <- substr(prec45[,1],1,7)
 prec45$ano <- substr(prec45[,1],1,4) %>% as.numeric(.)
 prec45$mes <- substr(prec45[,1],6,7) %>% as.numeric(.)
 
 prec45$t <- 1:nrow(prec45)

 prec45mes <- aggregate(prec45 ~ano_mes, data = prec45, FUN = max)
 prec45mes$t <- 1:nrow(prec45mes)

 
 blockmax45 <- aggregate(prec45 ~ano, data = prec45, FUN = max)
 blockmax45$t <- 1:nrow(blockmax45) 

 #rcp85
 prec85$ano_mes <- substr(prec85[,1],1,7)
 prec85$ano <- substr(prec85[,1],1,4) %>% as.numeric(.)
 prec85$mes <- substr(prec85[,1],6,7) %>% as.numeric(.)
 
 prec85$t <- 1:nrow(prec85)
 
 prec85mes <- aggregate(prec85 ~ano_mes, data = prec85, FUN = max)
 prec85mes$t <- 1:nrow(prec85mes)

 blockmax85 <- aggregate(prec85 ~ano, data = prec85, FUN = max)
 blockmax85$t <- 1:nrow(blockmax85) 

 #baseline
 prechist$ano_mes <- substr(prechist[,1],1,7)
 prechist$ano <- substr(prechist[,1],1,4) %>% as.numeric(.)
 prechist$mes <- substr(prechist[,1],6,7) %>% as.numeric(.)
 
 prechist$t <- 1:nrow(prechist)

 prechistmes <- aggregate(prechist ~ano_mes, data = prechist[prechist$ano > 1979,], FUN = max)
 prechistmes$t <- 1:nrow(prechistmes)
 
 blockmaxhist <- aggregate(prechist ~ano, data =  prechist[prechist$ano > 1979,], FUN = max)
 blockmaxhist$t <- 1:nrow(blockmaxhist)

 #-------------------------------------------------------------------------
 #estatística descritiva

 est_desc <- function(x){
                round(data.frame('Mínimo' = min(x[,2]),'Média'= mean(x[,2]), 'Mediana' = median(x[,2]), 'Máximo' = max(x[,2]),
                'Desvio Padrão' = sd(x[,2]), 'CV' = sd(x[,2])/mean(x[,2])),2)
 }

 tab_desc <- purrr::map_df(list(blockmaxhist, blockmax45, blockmax85,
                            prechistmes ,prec45mes, prec85mes), est_desc )

 #xtable(tab_desc, caption = 'Estatísticas descritivas 
 #das séries de precipitações máximas anuais e mensais, no \textit{baseline} e nas cenários RCP 4.5 e 8.5')
  
  #grpaficos (sem uso)

 df_mes <- purrr::map_df(list(unname(blockmaxhist[,1:2]), 
                unname(blockmax45[,1:2]), unname(blockmax85[,1:2])),
                function(x){names(x) <- c('ano', 'prec')
                x}) 
 df_mes$cenario <- c(rep('Baseline',31), rep('RCP 4.5',31), rep('RCP 8.5',31))

 theme_set(theme_minimal())
 ggplot(df_mes, aes(x = ano, y = prec)) + 
 geom_line(aes(colour = cenario), size = 1.25)

  #plotando os mapas
  source('script_mapas_hists.r')

 # rodando os modelos
 # blockmaxima
 mle_45 <- fevd(x = prec45, data = blockmax45, location.fun=~t, method = "MLE", type="GEV", time.units = 'years')
 mle_85 <- fevd(x = prec85, data = blockmax85, location.fun=~t, method = "MLE", type="GEV", time.units = 'years')
 mle_hist <- fevd(x = prechist, data = blockmaxhist, location.fun=~t, method = "MLE", type="GEV", time.units = 'years')
# pot
 mle_45p <- fevd(x = prec45, data = prec45mes, scale.fun=~t,  type="GP", threshold = 130, use.phi = F,
            time.units = 'months', span = 31)
 mle_85p <- fevd(x = prec85, data = prec85mes, scale.fun=~t,  type="GP", threshold = 130, use.phi = F,
            time.units = 'months', span = 31)
 mle_histp <- fevd(x = prechist, data = prechistmes, scale.fun=~t,  type="GP", threshold = 130, use.phi = F,
            time.units = 'months', span = 31)

 #tabela com os parâmetros e erro padrão
 params_gve <- lapply(list(mle_hist, mle_45, mle_85),  function(x){
                matrix(c(strip(x), summary(x, silent = T)$se), 2,4, byrow = T) }) %>% 
                do.call('rbind', .)

 params_gp <- lapply(list(mle_histp, mle_45p, mle_85p),  function(x){
                matrix(c(strip(x), summary(x, silent = T)$se), 2,3, byrow = T) }) %>% 
                do.call('rbind', .)


 #rodando script para os gráficos de distribuição
 source('script_dist_graf.R')

 rl_trendp<- return.level(mle_45p, conf = 0.05, return.period= c(5,10,20,100))


 # return level plot
 plot(mle_45, type="rl", main="Return Level Plot for Bärnkopf w/ MLE")

 #valores do período de retorno
 erlevd(mle_85,10)
 
 
 #valores dos parâmetros do modelo
 strip(mle_45)
 ci(mle_45p, type =  'parameter')
