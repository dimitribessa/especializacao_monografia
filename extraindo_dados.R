 #extração daods monografia especialização (u30-jun-2023, 14:49h)
 #saindo das projeções revisadas em 2023
 
 #.libPaths('D:\\Dimitri\\Docs 17-out-15\\R\\win-library\\3.1') #caminho dos pacotes
  # Carregando os pacotes
 #library('xlsx')         #para ler/criar arquivos xlsx
 library('ggplot2')      #Gráficos (mais usado)
 library('reshape2')     #para remodular data.frames
 library('dplyr')        #manipulação de dados - tydiverse
 library('data.table')   #para função fread()
 library('stringr')      #funções de string  - tydiverse
 library('downloader')	  # downloads and then runs the source() function on scripts from github
 library('RCurl')
 library('magrittr')     #para mudar nome de colunas
 
 library('ncdf4') #para conversão dos daods *.nc
 library('extRemes') #cálculo de valores extremos
 #funçoes espaciais
 library('rgeos') #leitura de mapas
 library('rgdal') #leitra de mapas
 library('RColorBrewer')
 library('raster')
 library('leaflet')
 library('sp')
 library('scales')
 library('lattice')
 library('parallel')



 #selecionando diretório...
 setwd('D:/Dimitri/Docs 17-out-15/UEL 2022/monografia/dados')


 #obtendo nome dos arquivos do eta
 session <- ssh::ssh_connect('sdc_nietta@10.111.7.174', passwd = 'N1et4-SDc.d@2020')
 
 files_hist <- ssh::ssh_exec_internal(session, "ls ./Rodada_Eta_HadGEM2-ES_5km/1975_2010/2D") %>%
    .[["stdout"]]  %>%
    rawToChar()    %>%
    strsplit("\n") %>%
    unlist()
    
 files_proj85 <- ssh::ssh_exec_internal(session, "ls ./Rodada_Eta_HadGEM2-ES_5km/RCP8.5/2D") %>%
    .[["stdout"]]  %>%
    rawToChar()    %>%
    strsplit("\n") %>%
    unlist()
 
 files_proj45 <- ssh::ssh_exec_internal(session, "ls ./Rodada_Eta_HadGEM2-ES_5km/RCP4.5/2D") %>%
    .[["stdout"]]  %>%
    rawToChar()    %>%
    strsplit("\n") %>%
    unlist()
    
 ssh::ssh_disconnect(session)
 
 #retirando os repetiudos
 files_hist <- files_hist[(1:length(files_hist))%%2 == 1]
 files_hist <- files_hist[-length(files_hist)]
 
 files_proj45 <- files_proj45[(1:length(files_proj45))%%2 == 1]
 files_proj45 <- files_proj45[-1]
 files_proj45 <- files_proj45[-length(files_proj45)] 
 
 files_proj85 <- files_proj85[(1:length(files_proj85))%%2 == 1]
 
 #nomeando a lista...
 nomes <- substr(files_proj85,30,37)  #formato %aaaammdd (sem as horas)
 nomes1975 <- substr(files_hist,30,37)
 #datas <- unique(as.Date(nomes, '%Y%m%d%H'))
 datas <- unique(nomes)
 datas1975 <- unique(nomes1975)

#----------------------recorte dos municípios (add 04-jul-2023)
 #load('municipiosf.RData')
 load('municipiopoly.RData')
 
 lista_municipios <- c( 'Agrolândia', 'Agronômica', 'Alfredo Wagner', 'Apiúna', 'Ascurra', 'Atalanta', 
                        'Aurora', 'Balneário Camboriú', 'Balneário Piçarras', 'Barra velha', 'Benedito Novo', 
                        'Blumenau', 'Bom Retiro', 'Botuverá', 'Braço do Trombudo', 'Brusque', 'Camboriú', 'Chapadão do Lageado', 
                        'Dona Emma', 'Doutor Pedrinho', 'Gaspar', 'Guabiruba', 'Ibirama', 'Ilhota', 'Imbuia', 'Indaial', 'Itaiópolis', 'Itajaí', 
                        'Ituporanga', 'José Boiteux', 'Laurentino', 'Lontras', 'Luiz Alves', 'Massaranduba', 'Mirim Doce', 'Monte Castelo', 'Navegantes', 
                        'Otacílio Costa', 'Papanduva', 'Penha', 'Petrolândia', 'Pomerode', 'Ponte Alta do Norte', 'Pouso Redondo', 'Presidente Getúlio', 
                        'Presidente Nereu', 'Rio do Campo', 'Rio do Oeste', 'Rio dos Cedros', 'Rio do Sul', 'Rodeio', 'Salete', 'Santa Cecília', 
                        'Santa Terezinha', 'São João do Itaperiú', 'Taió', 'Timbó', 'Trombudo Central', 'Vidal Ramos', 'Vitor Meireles', 'Witmarsum')

 #mapa_comite <- municipiosf[municipiosf$Municipio %in% lista_municipios,]
 mapa_comite <- municipiopoly[municipiopoly$Municipio %in% lista_municipios,]
 mapa_comite <- sf::st_as_sf(mapa_comite)
 mapa_comite$reg_bacia <- 'Itajaí-Açu'
 #recorte <- sf::st_union(mapa_comite)
 recorte <- group_by(mapa_comite, reg_bacia) %>% summarise() 


#----------------------------------------------------------------------------#
 #-------------precipitação cenário 45----------------------------------#
 i <- 1
 sequencia45 <- NULL
 while(i != 19){
 load(paste0('G:/.shortcut-targets-by-id/1NzgdfgQW4Evonx1hUol7ETBjDT7Vd1f1/NIETTA/Dimitri/projecao_2023/prec45'
 ,i,'.RData'))
 if(i == 1){rcp45 <- rcp45[-1]}
 if(i == 18){rcp45 <- rcp45[1:4281]}
 matriz <- lapply(rcp45, function(x){
                           r <-  raster(x, xmn=-57, xmx=-46.05, ymn=-32, ymx=-23.45, 
                                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) 
                      r <-         flip(r, direction = 'y')
                      r <- crop(r, recorte)
                      #values(r)
                      r
                         }) 
 sequencia45 <- append(sequencia45, matriz)

  gc(reset = T)

 print(i)
 i <- i+1
 
 }

  prec45 <- lapply(1:11160, function(x){dia <- sequencia45[1:8 + (x-1)*8]
                             lapply(1:8, function(x){values(dia[[x]])}) %>% #3 == prec
            Reduce('+',.)*1000}) 
 
  names(prec45) <- datas[-length(datas)]

 #tratando os dados, utilizando apenas o maior valor diário do raster
 prec45 <- lapply(prec45, max)
 prec45 <- data.frame(data = names(prec45), prec45 = unlist(prec45))
 prec45[,1] <- as.Date(prec45[,1], format = '%Y%m%d')
 prec45 <- prec45[!is.na(prec45[,1]),]
 #----------------------------------------------------------------------------#
 #-------------precipitação cenário 85----------------------------------#
 i <- 1
 sequencia85 <- NULL
 while(i != 19){
 load(paste0('G:/.shortcut-targets-by-id/1NzgdfgQW4Evonx1hUol7ETBjDT7Vd1f1/NIETTA/Dimitri/projecao_2023/prec85_'
 ,i,'.RData'))
 if(i == 18){rcp85 <- rcp85[1:4280]}
 matriz <- lapply(rcp85, function(x){
                           r <-  raster(x, xmn=-57, xmx=-46.05, ymn=-32, ymx=-23.45, 
                                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) 
                      r <-         flip(r, direction = 'y')
                      r <- crop(r, recorte)
                      #values(r)
                      r
                         }) 
 sequencia85 <- append(sequencia85, matriz)

  gc(reset = T)

 print(i)
 i <- i+1
 
 }

  prec85 <- lapply(1:11160, function(x){dia <- sequencia85[1:8 + (x-1)*8]
                             lapply(1:8, function(x){values(dia[[x]])}) %>% #3 == prec
            Reduce('+',.)*1000}) 
 
  names(prec85) <- datas[-length(datas)] 
 
 #tratando os dados, utilizando apenas o maior valor diário do raster
 prec85 <- lapply(prec85, max)
 prec85 <- data.frame(data = names(prec85), prec85 = unlist(prec85))
 prec85[,1] <- as.Date(prec85[,1], format = '%Y%m%d')
 prec85 <- prec85[!is.na(prec85[,1]),]
 

 #----------------------------------------------------------------------------#
 #-------------precipitação cenário histórico---------------------------------#
 i <- 1
 sequenciahist <- NULL
 while(i != 22){
 load(paste0('G:/.shortcut-targets-by-id/1NzgdfgQW4Evonx1hUol7ETBjDT7Vd1f1/NIETTA/Dimitri/projecao/rcp1975_2010_',
      i,'.RData'))
 if(i == 21){prechist <- rcp1975_2010[1:3680]}else{
             prechist <- rcp1975_2010
 }
 matriz <- lapply(prechist, function(x){
                           x <- x[[3]]
                           r <-  raster(x, xmn=-57, xmx=-46.05, ymn=-32, ymx=-23.45, 
                                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) 
                      r <-         flip(r, direction = 'y')
                      r <- crop(r, recorte)
                      #values(r)
                      r
                         }) 
 sequenciahist <- append(sequenciahist, matriz)

  gc(reset = T)

 print(i)
 i <- i+1
 
 }

  prechist <- lapply(1:12960, function(x){dia <- sequenciahist[1:8 + (x-1)*8]
                             lapply(1:8, function(x){values(dia[[x]])}) %>% #3 == prec
            Reduce('+',.)*1000}) 
 
  names(prechist) <- datas1975[-length(datas1975)] 
 
 #tratando os dados, utilizando apenas o maior valor diário do raster
 prechist <- lapply(prechist, max)
 prechist <- data.frame(data = names(prechist), prechist = unlist(prechist))
 prechist[,1] <- as.Date(prechist[,1], format = '%Y%m%d')
 prechist <- prechist[!is.na(prechist[,1]),]
 