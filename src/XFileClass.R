XFileClass = R6Class("XFileClass",
                      
                      #========================#
                      # Metodos publicos       #
                      public = list(
                        
                        initialize = function(arquivoX){
                          
                          private$arquivoX = arquivoX
                          
                          arquivoX = sprintf("DSSAT//%s",arquivoX)
                          con = file(arquivoX)
                          open(con)
                          
                          xLine = readLines(con)
                          
                          markers = grep("\\*",xLine)
                          
                          tMarkers = grep("\\*TREATMENTS", xLine)
                          cMarkers = grep("\\*CULTIVARS", xLine)
                          
                          tMarkers = which(markers == tMarkers)
                          cMarkers = which(markers == cMarkers)
                          
                          tIni = markers[(tMarkers)]
                          cIni = markers[(cMarkers)]
                          
                          tEnd = markers[(tMarkers + 1)] - 1
                          cEnd = markers[(cMarkers + 1)] - 1
                          
                          if(tMarkers == markers[length(markers)] ){
                            tEnd = length(xLine)
                          }
                          
                          if(cMarkers == markers[length(markers)] ){
                            cEnd = length(xLine)
                          }
                          
                          treatmentTable = private$getTreatmentTable(xLine[(tIni + 2):tEnd])
                          cultivarTable = private$getCultivarTable(xLine[(cIni + 2):cEnd])
                          
                          setkey(treatmentTable, ID)
                          setkey(cultivarTable, ID)
                          
                          resultado = merge(treatmentTable, cultivarTable)
                          
                          close(con)
                          private$resultado = resultado
                        },
                        
                        validateCultivar = function(cultivarID, xFile){
                          
                          tmp = private$resultado
                          validId = unique(tmp$cultivar)
                          
                          validation = setdiff(cultivarID, validId)
                          if(length(validation) > 0){
                            stop(sprintf("Os cultivares %s nao estao definidos no arquivo %s.",
                                         paste0(validation,collapse = ", "), private$arquivoX))
                          }
                        },
                        
                        getResultado = function(){
                          return(private$resultado)
                        }
                      ),
                      #========================#
                      
                      #========================#
                      # Metodos privados       #
                      private = list(
                        
                        resultado = data.frame(),
                        arquivoX = c(),
                        
                        #=============================================================#
                        #                                                             #
                        
                        getTreatmentTable = function(linhas){
                          
                          linhas = linhas[linhas != ""]
                          linhas = strsplit(linhas ,split = " ")
                          linhas = sapply(linhas, function(x){
                            
                            x = x[x != ""]
                            if(nchar(x[1]) > 3){
                              
                              base = x[1]
                              x[3:(length(x) + 1)] = x[2:length(x)]
                              x[1] = substr(base,1,3)
                              x[2] = substr(base, 4,nchar(base))
                            }
                            
                            return(x[c(5,6)])
                          })
                          
                          linhasTable = as.data.table(t(linhas))
                          names(linhasTable) = c("TNAM","ID")
                          
                          return(linhasTable)
                        },
                        
                        #                                                             #
                        #=============================================================#
                        
                        #=============================================================#
                        #                                                             #
                        
                        getCultivarTable = function(linhas){
                          
                          linhas = linhas[linhas != ""]
                          linhas = strsplit(linhas ,split = " ")
                          linhas = sapply(linhas, function(x){
                            x = x[x != ""]
                            return(x[c(1,3)])
                          })
                          
                          linhasTable = as.data.table(t(linhas))
                          names(linhasTable) = c("ID","cultivar")
                          
                          return(linhasTable)
                        }
                        
                        #                                                             #
                        #=============================================================#
                        
                        
                      )
                      #========================#
)