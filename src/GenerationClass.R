GenerationClass = R6Class("GenerationClass",
                      
                      #========================#
                      # Metodos publicos       #
                      public = list(
                        
                        initialize = function(baselineObject, lim.maxV = 1.1, lim.minV = 0.9){
                          private$lim.max = lim.maxV
                          private$lim.min = lim.minV
                          private$baselineObject = baselineObject
                        },
                        
                        analizeData = function(variationObject, currentCultivar){
                          private$insertRate(currentCultivar, private$baselineObject,variationObject)
                          private$validation()
                        },
                        
                        # metodo de retorno
                        getValidationTable = function(){
                          return(private$validationTable)
                        },
                        
                        isValid = function(){
                          return(private$isValidTable)
                        }
                      ),
                      #========================#
                      
                      #========================#
                      # Metodos privados       #
                      private = list(
                        
                        isValidTable = TRUE,
                        validationTable = data.table(),
                        baselineObject = c(),
                        lim.max = c(),
                        lim.min = c(),
                        state = c(),
                        
                        #=============================================================#
                        #                                                             #
                        
                        # funcao para inserir a taxa de variacao nas variaveis marcadas ex.(valores esperados: entre 1.1 e 0.9)
                        insertRate = function(currentCultivar, baselineObject,variationObject){
                          
                          summaryTable = baselineObject$getSummaryTable()
                          errorControl = summaryTable[1,]
                          summaryTable = summaryTable[summaryTable$cultivar == currentCultivar, ]
                          variationTable = variationObject$getVariationTable()
                          variationTable = variationTable[variationTable$cultivar == currentCultivar, ]
                          
                          vS = (dim(summaryTable)[1] > 0)
                          vV = (dim(variationTable)[1] > 0)
                          
                          if( vS && vV ){
                          
                          validationTable = variationTable[, c(1,2,8,9,13), with = F]
                          
                          variationTable$CWAM[!is.numeric(variationTable$CWAM)] = 0
                          variationTable$HWAM[!is.numeric(variationTable$HWAM)] = 0
                          variationTable$LAIX[!is.numeric(variationTable$LAIX)] = 0
                          
                          copia.st = summaryTable[summaryTable$RUNNO %in% variationTable$RUNNO,]
                            
                          validationTable$baseline_CWAM = copia.st$CWAM
                          validationTable$baseline_HWAM = copia.st$HWAM
                          validationTable$baseline_LAIX = copia.st$LAIX
                          
                          validationTable$taxa_CWAM = as.numeric(variationTable$CWAM) / copia.st$CWAM 
                          validationTable$taxa_HWAM = as.numeric(variationTable$HWAM) / copia.st$HWAM
                          validationTable$taxa_LAIX = as.numeric(variationTable$LAIX) / copia.st$LAIX
                            
                          state = "ALIVE"
                          
                          }else{
                            errorControl = errorControl[, c(1,2,8,9,13), with = F]
                              
                            errorControl$baseline_CWAM = NA
                            errorControl$baseline_HWAM = NA
                            errorControl$baseline_LAIX = NA
                              
                            errorControl$taxa_CWAM = NA 
                            errorControl$taxa_HWAM = NA
                            errorControl$taxa_LAIX = NA
                          
                            validationTable = errorControl
                            state = "DEAD"
                          }
                            
                          validationTable$parametro = variationObject$getParametro()
                          validationTable$cultivar = currentCultivar
                          
                          private$validationTable = validationTable
                          private$state = state

                        },
                        
                        #                                                             #
                        #=============================================================#
                        
                        #=============================================================#
                        #                                                             #
                        
                        # valores esperados: entre 1.1 e 0.9 ex.
                        validation = function(){
               
                          lim.max = private$lim.max
                          lim.min = private$lim.min
                          
                          tabela = private$validationTable
                          variavel.name = unique(tabela$parametro)
                          cultivar = unique(tabela$cultivar)
                          
                          tabela = tabela[,c(9,10,11), with = F]
                          erro = private$state
               
                          tabela[tabela == Inf] = NA
                          tabela = apply(tabela, 2,mean, na.rm = T)
                          tabela = tabela[complete.cases(tabela)]
                          
                          valid.min = tabela >= lim.min 
                          valid.max = tabela <= lim.max
                          
                          tamanho = length(valid.max)
                          
                          resultado = TRUE
                          
                          if(tamanho > 0){
                          
                            for(i in 1:tamanho){
                              resultado = (resultado && (valid.max[i] && valid.min[i]) )
                            }
                          
                          }else{
                            if(erro == "ALIVE"){
                              warning(sprintf("O parametro %s do cultivar %s para uma determinada variacao possui as 3 taxas com o valor infinito.", variavel.name, cultivar ) )
                              erro = "INVALID COLUMNS"
                            }
                          }
                          
                          attr(resultado, "result") = erro
                          private$isValidTable = resultado
                        }
                        
                        #                                                             #
                        #=============================================================#
                        
                      )
                      #========================#
)