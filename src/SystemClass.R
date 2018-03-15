SystemClass = R6Class("SystemClass",
                         
                         #========================#
                         # Metodos publicos       #
                         public = list(
                           
                           #=============================================================#
                           # executando simulacao                                        #
                           
                           R.cropgro = function(arquivoResposta){
                             
                             bat.command = "cd DSSAT && ./dscsm045.exe N DSSBatch.v45"                                                                                                                   
                             system(command = bat.command, ignore.stdout = T)
                             
                             arquivoResposta = sprintf("DSSAT//%s",arquivoResposta);
                             
                             # tratando arquivo summary
                             tabela.final = private$turn_summary(arquivoResposta,"DSSAT//tabela.tmp")
                             
                             clear.files.out()
                             unlink("DSSAT//tabela.tmp")
                             
                             return(tabela.final)     
                           }
                           
                           #                                                             #
                           #=============================================================#
                         ),
                         #========================#
                         
                         #========================#
                         # Metodos privados       #
                         private = list(
                           
                           #=============================================================#
                           #                                                             #
                           
                           # funcoes tratamento
                           turn_summary = function(arquivo.summary,arquivo_saida){
                             
                             if(!file.exists(arquivo.summary)){
                               stop(sprintf("O arquivo %s nao existe, ou os arquivos base estao corrompidos.\nVerifique as configuracoes iniciais, ou o arquivo DSSAT//WARNING.OUT .",arquivo.summary))
                             }
                             
                             
                             con = file(arquivo.summary)
                             open(con)
                             
                             tratamento = function(l.x){
                               l.x = l.x[l.x!= ""]
                               return(l.x[l.x!= ""])
                             }
                             
                             first = T
                             while(T){
                               
                               linhas = readLines(con, n = 100000)
                               
                               if(length(linhas) == 0){
                                 break
                               }
                               
                               x = strsplit(linhas, split = " ")
                               x.1 = lapply(x,tratamento)
                               tamanho = sapply(x.1,length) 
                               
                               maxRepeat = sort(table(tamanho),decreasing=TRUE)[1]
                               
                               pos = which(tamanho == as.numeric(names(maxRepeat)) )
                               
                               x.summ = x.1[pos]
                               x.summ = lapply(x.summ, function(b){
                                 b.1 = b[c(1,2,3,4,5,6,7,8,17,19,20,28)]
                                 return(paste0(b.1, collapse = ";"))
                               })
                               
                               if(first){
                                 
                                 cat(sprintf("RUNNO;TRNO;R#;O#;C#;CR;MODEL;TNAM;HDAT;CWAM;HWAM;LAIX\n"),file = arquivo_saida,append = F)
                                 first_write = F
                               }
                               
                               cat(sprintf("%s\n",x.summ), file = arquivo_saida, append = T)
                             }
                    
                             close(con)
                             
                             if(file.size(arquivo_saida) == 0){
                               browser()
                             }
                             
                             tabela_saida = fread(arquivo_saida)
                             return(tabela_saida)
                             
                           }
                           
                           #                                                             #
                           #=============================================================#
                           
                         )
                         #========================#
)