 #script para gráficos de período de retorno

 func_dfrlevel <- function(x){
    dado <- return.level(x, return.period = c(5,10,25,100))
    dado <- sapply(1:4, function(x){dado[,x]}) %>% as.data.frame
    names(dado) <- c('5 anos', '10 anos','25 anos', '100 anos')
    dado
 }

 df_rlevelgev <- lapply(list(mle_hist, mle_45, mle_85), func_dfrlevel)
 df_rlevelgp <- lapply(list(mle_histp, mle_45p, mle_85p), func_dfrlevel)

 #gráficos período retorno gev
 df_rlevelgev[[1]]$ano <- 1980:2010
 df_rlevelgev[[1]] <- reshape2::melt(df_rlevelgev[[1]], id.vars = 'ano')
 df_rlevelgev[2:3] <- lapply(2:3, function(x){
                        dado <- df_rlevelgev[[x]]
                        dado$ano <- 2010:2040
                        dado <- reshape2::melt(dado, id.vars = 'ano')
                        dado

 })

 graf_rlgevhist <- 
 ggplot(data = blockmaxhist, aes(y = prechist, x = ano)) + 
 geom_line() +
 geom_line(data = df_rlevelgev[[1]], aes(x = ano,y = value, colour = variable)) +
 labs(x = 'Período', y = 'mm/dia', 
 colour = 'Período de\nretorno', title = 'Baseline') +
 scale_colour_viridis_d() +
 theme(legend.position = 'bottom') + 
 ylim(70,240)

 legenda <- get_legend(graf_rlgevhist)

 graf_rlgev45 <- 
 ggplot(data = blockmax45, aes(y = prec45, x = ano)) + 
 geom_line() +
 geom_line(data = df_rlevelgev[[2]], aes(x = ano,y = value, colour = variable)) +
 labs(x = 'Período', y = 'mm/dia', 
 colour = 'Período de\nretorno', title = 'RCP 4.5') +
 scale_colour_viridis_d() +
 theme(legend.position = 'hidden') + 
 ylim(70,240)

 graf_rlgev85 <- 
 ggplot(data = blockmax85, aes(y = prec85, x = ano)) + 
 geom_line() +
 geom_line(data = df_rlevelgev[[3]], aes(x = ano,y = value, colour = variable)) +
 labs(x = 'Período', y = 'mm/dia', 
 colour = 'Período de\nretorno', title = 'RCP 8.5') +
 scale_colour_viridis_d() +
 theme(legend.position = 'hidden') + 
 ylim(70,240)

 
 graf_combined <- gridExtra::grid.arrange(graf_rlgevhist  + theme(legend.position = 'hidden'),
                        graf_rlgev45, graf_rlgev85, ncol = 3)

 png('graf_rlgev.png', height = 300, width = 900)
 gridExtra::grid.arrange(graf_combined, legenda, nrow = 2, heights = c(10,1))
 dev.off()

 
#gráficos período retorno gp
 df_rlevelgp[[1]]$ano <- seq(as.Date('1980-01-01'),as.Date('2010-12-01'), by = 'month')
 df_rlevelgp[[1]] <- reshape2::melt(df_rlevelgp[[1]], id.vars = 'ano')
 df_rlevelgp[2:3] <- lapply(2:3, function(x){
                        dado <- df_rlevelgp[[x]]
                        dado$ano <- seq(as.Date('2010-01-01'),as.Date('2040-12-01'), by = 'month')
                        dado <- reshape2::melt(dado, id.vars = 'ano')
                        dado

 })

 graf_rlgphist <- 
 ggplot(data = prechistmes, aes(y = prechist, 
  x = seq(as.Date('1980-01-01'),as.Date('2010-12-01'), by = 'month'))) + 
 geom_line() +
 geom_line(data = df_rlevelgp[[1]], aes(x = ano,y = value, colour = variable)) +
 labs(x = 'Período', y = 'mm/dia', 
 colour = 'Período de\nretorno', title = 'Baseline') +
 scale_colour_viridis_d() +
 theme(legend.position = 'bottom') + 
 geom_hline(yintercept = 130, color = 'red', size = 1.1) +
  geom_label(aes(x = as.Date('1980-01-01'), y = 128, label = 'Limiar'), size = 3, hjust = 0) + 
 ylim(0,220)

 graf_rlgp45 <- 
 ggplot(data = prec45mes, aes(y = prec45, 
  x = seq(as.Date('2010-01-01'),as.Date('2040-12-01'), by = 'month'))) + 
 geom_line() +
 geom_line(data = df_rlevelgp[[2]], aes(x = ano,y = value, colour = variable)) +
 labs(x = 'Período', y = 'mm/dia', 
 colour = 'Período de\nretorno', title = 'RCP 4.5') +
 scale_colour_viridis_d() +
 theme(legend.position = 'hidden') + 
 geom_hline(yintercept = 130, color = 'red', size = 1.1) +
  geom_label(aes(x = as.Date('2010-01-01'), y = 128, label = 'Limiar'), size = 3, hjust = 0) + 
 ylim(0,220)

 graf_rlgp85 <- 
  ggplot(data = prec85mes, aes(y = prec85, 
  x = seq(as.Date('2010-01-01'),as.Date('2040-12-01'), by = 'month'))) + 
 geom_line() +
 geom_line(data = df_rlevelgp[[3]], aes(x = ano,y = value, colour = variable)) +
 labs(x = 'Período', y = 'mm/dia', 
 colour = 'Período de\nretorno', title = 'RCP 8.5') +
 scale_colour_viridis_d() +
 theme(legend.position = 'hidden') + 
 geom_hline(yintercept = 130, color = 'red', size = 1.1) +
  geom_label(aes(x = as.Date('2010-01-01'), y = 128, label = 'Limiar'), size = 3, hjust = 0) + 
 ylim(0,220)
 
 graf_combined <- gridExtra::grid.arrange(graf_rlgphist  + theme(legend.position = 'hidden'),
                        graf_rlgp45, graf_rlgp85, ncol = 3)

 png('graf_rlgp.png', height = 300, width = 900)
 gridExtra::grid.arrange(graf_combined, legenda, nrow = 2, heights = c(10,1))
 dev.off()

