OutputClass = R6Class("OutputClass",
                        
                        #========================#
                        # Metodos publicos       #
                        public = list(
                          
                          write.table = function(tabela, nome, APPEND){
                            
                            write.table(tabela,sprintf("output//csv//%s.csv",nome ), col.names = !APPEND, 
                                        row.names = F, append = APPEND, sep = ";")
                            
                          },
                         
                          write.plot = function(nome, rateMax = 1.1, rateMin = 0.9){
                            
                            tabela = fread(sprintf("output//csv//%s.csv",nome ))
                            
                            vars = strsplit(nome,split = "_")[[1]]
                            
                            tabela[tabela == Inf] = NA
                            
                            tabela.simple = ddply(tabela, .(cultivar , tipo ,parametro,variacao), summarise,
                                                  media_CWAM = round(mean(taxa_CWAM, na.rm = T),5),
                                                  media_HWAM = round(mean(taxa_HWAM, na.rm = T),5),
                                                  media_LAIX = round(mean(taxa_LAIX, na.rm = T),5))
                            
                            tabela.simple = melt(tabela.simple, id.vars =c("cultivar","tipo","parametro","variacao")  , value.name="valor", variable.name="Variable", na.rm=TRUE)
                            
                            if(dim(tabela.simple)[1] > 0){
                              
                              endPoint = tabela[tabela$EOS == T,]
                              endPoint.variacao = unique(endPoint$variacao)
                              
                              g1 = subset(tabela.simple, variacao == endPoint.variacao)
                              
                              titulo = sprintf("Resumo da variacao %s (%s - %s)",vars[2] , vars[1] , vars[3])
                              varia = seq(min(tabela.simple$variacao), max(tabela.simple$variacao), 5)
                              
                              ggplot(tabela.simple, aes(x = variacao, y = valor, group=Variable, colour=Variable)) +
                                geom_point() + ggtitle(titulo) + geom_smooth() + geom_point(data = g1, colour = "red", shape = 5, size = 3) + 
                                facet_grid(Variable ~ .) + theme(legend.position = "none", axis.text.x = element_text(angle = 90)) +
                                labs(y = sprintf("Valor medio (%s - %s)", rateMin, rateMax), x = "Taxa de variacao (%)") + 
                                scale_x_continuous(minor_breaks = varia, labels = varia, breaks = varia)
                              
                              ggsave(sprintf("output//plot//%s.tiff", nome))
                            
                            }else{
                              warning(sprintf("O grafico %s.tiff nao pode ser gerado devido a falta de dados fornecidos.", nome ) )
                            }
                            
                          },
                          
                          write.table.combination = function(tabela.combinacoes, matrix.nome, APPEND){
                            write.table(tabela.combinacoes, matrix.nome, sep = ";", append = !APPEND, row.names = F, col.names = APPEND)
                          }
                          
                        ),
                        #========================#
                        
                        #========================#
                        # Metodos privados       #
                        private = list(
                          
                        )
                        #========================#
)