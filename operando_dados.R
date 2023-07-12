 #operando os dados extraídos do modelo (rodar o extraindo_dados.R antes) - 06-jul-2023, 15:33h

 #.libPaths('D:\\Dimitri\\Docs 17-out-15\\R\\win-library\\3.1') #caminho dos pacotes
  # Carregando os pacotes
 #library('xlsx')         #para ler/criar arquivos xlsx
 library('ggplot2')      #Gráficos (mais usado)
 library('reshape2')     #para remodular data.frames
 library('dplyr')        #manipulação de dados - tydiverse
 library('stringr')      #funções de string  - tydiverse
 library('magrittr')     #para mudar nome de colunas
 
 library('extRemes') #cálculo de valores extremos
 #funçoes espaciais

 #Block Maxima
 #obtendo os valores máximos mensais

 prec45$ano_mes <- substr(prec45[,1],1,7)
 prec45$ano <- substr(prec45[,1],1,4) %>% as.numeric(.)
 prec45$mes <- substr(prec45[,1],6,7) %>% as.numeric(.)
 prec45$est <- with(prec45, ifelse(mes %in% c(1,2,12), 1,
                                ifelse(mes %in% c(3:5), 2,
                                ifelse(mes %in% c(6:8), 3,4)
                                )))

 prec45$ano_est <- with(prec45, paste0(ano,'_',est))
 prec45$t <- 1:nrow(prec45)

 
 blockmax45 <- aggregate(prec45 ~ano, data = prec45, FUN = max)
 blockmax45$t <- 1:nrow(blockmax45) 

 # maximum likelihood estimation
 mle_45 <- fevd(x = prec45, data = blockmax45, location.fun=~t, method = "MLE", type="GEV")
 rl_trend <- return.level(mle_45, conf = 0.05, return.period= c(2,5,10,20,50,100))

# pot
 mle_45p <- fevd(x = prec45, data = prec45, scale.fun=~t, use.phi = T, type="GP", threshold = 130)
 rl_trendp<- return.level(mle_45p, conf = 0.05, return.period= c(2,5,10,20,50,100))


 # return level plot
 plot(mle_45, type="rl", main="Return Level Plot for Bärnkopf w/ MLE")
