BaselineClass = R6Class("BaselineClass",
                        
                        #========================#
                        # Metodos publicos       #
                        public = list(
                          
                          initialize = function(tabelaMap, arquivoResposta = "Summary.OUT", output = "baseline.csv"){
                            
                            # Criando uma instancia da classe de sistema
                            baseline = SystemClass$new()
                            tabela = baseline$R.cropgro(arquivoResposta)
                            
                            tabelaMap$ID = NULL
                            
                            setkey(tabela,TNAM)
                            setkey(tabelaMap,TNAM)
                            
                            tabela = merge(tabela, tabelaMap)
                            
                            private$summaryTable = tabela
                          },
                          
                          # metodo de retorno
                          getSummaryTable = function(){
                            return(private$summaryTable)
                          }
                        ),
                        #========================#
                        
                        #========================#
                        # Metodos privados       #
                        private = list(
                          
                          # Tabela summary
                          summaryTable = data.table()
                        )
                        #========================#
)