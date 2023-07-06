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
 blockmax45 <- aggregate(prec45 ~ano_mes, data = prec45, FUN = max) 

 teste <- ggplot(blockmax45, aes(x = ano_mes, y = prec45)) + geom_line(aes(group = 1))
