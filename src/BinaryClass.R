BinaryClass = R6Class("BinaryClass",
                      
                      #========================#
                      # Metodos publicos       #
                      public = list(
                        
                        initialize = function(tabelaMap, limiar = 3, summaryFile = "Summary.OUT", rateMin = 0.9, rateMax = 1.1){
                          private$tabelaMap = tabelaMap
                          private$limiar = limiar
                          private$rateMin = rateMin
                          private$rateMax = rateMax
                          private$summaryFile = summaryFile
                        },
                        
                        start = function(generationObject ,binaryParameters, variationParameters){
                          
                          #binary parameters type, valor_min, valor_max
                          private$type = binaryParameters$type
                          private$valor_min = binaryParameters$valor_min
                          private$valor_max = binaryParameters$valor_max
                          private$generationObject = generationObject
                          private$variationParameters = variationParameters
                          
                          if(binaryParameters$type == "max"){
                            resp = private$limite_binario_max(private$limiar)
                          }else{
                            resp = private$limite_binario_min(private$limiar)
                          }
                          
                          return(resp)
                        }
                      ),
                      #========================#
                      
                      #========================#
                      # Metodos privados       #
                      private = list(
                        
                        type = c(),
                        valor_min = c(),
                        valor_max = c(),
                        generationObject = c(),
                        limiar = c(),
                        rateMax = c(),
                        rateMin = c(),
                        summaryFile = c(),
                        variationParameters = list(),
                        tabelaMap = data.frame(),
                        
                        #=============================================================#
                        #                                                             #
                        
                        limite_binario_min = function(limit = 3){
                          
                          imagem_max = private$valor_max 
                          imagem_min = private$valor_min
                          
                          valor_base = imagem_max
                          para = private$variationParameters
                          para$taxa = valor_base
                          
                          rodadas = 0
                          contador = 0
                          
                          otp = OutputClass$new()
                          
                          while(TRUE){
                            
                            
                            #===================================#
                            # Gerando objeto de variacao
                            variation = DataVariationClass$new(para, private$tabelaMap, arquivoResposta = private$summaryFile)
                            
                            # Iniciando validacao dos dados
                            go = private$generationObject
                            go$analizeData(variation, para$cultivarId)
                            valid = go$isValid()
                            
                            if(!valid){
                              # mover base em direcao ao limite maximo
                              if(imagem_max - imagem_min < 1){
                                contador = contador + 1
                              }
                              
                              imagem_min = valor_base
                              valor_base = round((valor_base + imagem_max)/2, digits = 5)
                              
                            }else{
                              # mover base em direcao ao limite minimo
                              if(imagem_max - imagem_min < 1){
                                contador = contador + 1
                              }
                              
                              imagem_max = valor_base
                              valor_base = round((valor_base + imagem_min)/2, digits = 5)
                              
                            }
                            
                            t.x = para$taxa
                            para$taxa = valor_base

                            end = (contador == limit)
                            
                            rodadas = rodadas + 1
                                                        
                            tabela = go$getValidationTable()
                            
                            tabela$rodada = rodadas
                            tabela$variacao = t.x
                            tabela$tipo = private$type
                            tabela$result = attr(valid, "result")
                            tabela$EOS = end # END OF SIMULATION
                            
                            nome = sprintf("%s_%s_%s_variacao_simples",para$cultivarId, private$type, variation$getParametro() )
                            otp$write.table(tabela, nome, (rodadas != 1) )
                            
                            cat(sprintf("[%s] Simulacao %s\n", Sys.time(), rodadas))
                            
                            if(end){
                              cat(sprintf("Encerrando processamento.\nValor encontrado: %s\nNumero de rodadas: %s\n",valor_base, rodadas))
                              otp$write.plot(nome, private$rateMax, private$rateMin)
                              break;
                            }
                            
                            clear.files.out()
                          }

                          linha_inicial = variation$getDefaultLine()
                          index = which(colnames(linha_inicial) == variation$getParametro())
                          
                          valor_padrao = linha_inicial[index]
                          
                          return(c(valor_base, valor_padrao))
                        },
                        
                        #                                                             #
                        #=============================================================#
 
                        #=============================================================#
                        #                                                             #
                        
                        limite_binario_max = function(limit = 3){
                          
                          imagem_max = private$valor_max 
                          imagem_min = private$valor_min
                          
                          valor_base = imagem_min
                          para = private$variationParameters
                          para$taxa = valor_base
                          
                          rodadas = 0
                          contador = 0

                          otp = OutputClass$new()
                                                    
                          while(TRUE){
                            
                            #===================================#
                            # Gerando objeto de variacao
                            variation = DataVariationClass$new(para, private$tabelaMap, arquivoResposta = private$summaryFile)
                            
                            # Iniciando validacao dos dados
                            go = private$generationObject
                            go$analizeData(variation, para$cultivarId)
                            valid = go$isValid()
                            
                            if(valid){
                              # mover base em direcao ao limite maximo
                              if(imagem_max - imagem_min < 1){
                                contador = contador + 1
                              }
                              
                              imagem_min = valor_base
                              valor_base = round((valor_base + imagem_max)/2, digits = 5)
                              
                            }else{
                              # mover base em direcao ao limite minimo
                              if(imagem_max - imagem_min < 1){
                                contador = contador + 1
                              }
                              
                              imagem_max = valor_base
                              valor_base = round((valor_base + imagem_min)/2, digits = 5)
                              
                            }
                            
                            t.x = para$taxa
                            para$taxa = valor_base
                            
                            end = (contador == limit)
                            
                            rodadas = rodadas + 1
                            
                            tabela = go$getValidationTable()
                            
                            tabela$rodada = rodadas
                            tabela$variacao = t.x
                            tabela$tipo = private$type
                            tabela$invalid = attr(valid, "result")
                            tabela$EOS = end # END OF SIMULATION
                            
                            nome = sprintf("%s_%s_%s_variacao_simples",para$cultivarId, private$type, variation$getParametro() )
                            otp$write.table(tabela, nome, (rodadas != 1) )
                            
                            cat(sprintf("[%s] Simulacao %s\n", Sys.time(), rodadas))
                            
                            if(end){
                              cat(sprintf("Encerrando processamento.\nValor encontrado: %s\nNumero de rodadas: %s\n",valor_base, rodadas))
                              otp$write.plot(nome, private$rateMax, private$rateMin)
                              break;
                            }
                            
                            clear.files.out()
                          }
                          
                          linha_inicial = variation$getDefaultLine()
                          index = which(colnames(linha_inicial) == variation$getParametro())
                          
                          valor_padrao = linha_inicial[index]
                          
                          return(c(valor_base, valor_padrao))
                        }
                        
                        #                                                             #
                        #=============================================================#
                        
                                               
                      )
                      #========================#
)