CombinationClass = R6Class("CombinationClass",
                           
                           #========================#
                           # Metodos publicos       #
                           public = list(
                             
                             initialize = function(tabela.nome, matrix.nome, cultivarId, coverage){
                               
                               private$tabela.nome = sprintf("output//csv//%s", tabela.nome)
                               private$tabela = fread(private$tabela.nome)
                               private$matrix.nome = sprintf("output//csv//%s", matrix.nome)
                               
                               private$createMatrixCombinations(cultivarId, "max", first = T)
                               private$createMatrixCombinations(cultivarId, "min", first = F)
                             },
                             
                             getTable = function(coverage){
                               tabela = fread(private$matrix.nome)
                               
                               ids = unique(tabela$id)
                               tamanho = length(ids)
                               ids.short = sample(ids, tamanho*(coverage/100))
                               
                               index = which(tabela$id %in% ids.short)
                               tabela = tabela[index, with = T]
                               
                               return(tabela)
                             }
                             
                           ),
                           #========================#
                           
                           #========================#
                           # Metodos privados       #
                           private = list(
                             
                             tabela = c(),
                             tabela.combinacao = data.frame(),
                             tabela.nome = c(),
                             matrix.nome = c(),
                             
                             createMatrixCombinations = function(cultivarId, tipo, index = 5000, first = T){
                               
                               resultado.t = private$tabela
                               
                               for(i in cultivarId){
                                 
                                 resultado = subset(resultado.t, cultivar == i)
                                 
                                 tamanho = dim(resultado)[1]
                                 binary = 2^(tamanho + 1) - 1
                                 parametros = resultado$parametro
                                 
                                 #=====================================#
                                 if(tipo == "max"){
                                   valor = resultado$ValorMaximo    
                                 }else{
                                   valor = resultado$ValorMinimo
                                 }
                                 
                                 tabela.final = c()
                                 
                                 contador = 1
                                 while(contador < binary){
                                   
                                   if(contador + index > binary){
                                     range = contador : binary
                                   }else{
                                     range = contador : (contador + index)
                                   }
                                   
                                   matrix.Binaria = sapply(range ,private$doubleToBin, tamanho)     
                                   vetor.resposta = apply(matrix.Binaria, 2, private$getCombination, valor)
                                   
                                   contador = contador + index + 1
                                   tabela.final = c(tabela.final, vetor.resposta)
                                 }
                                 #=====================================#
                                 
                                 arq.tmp = private$matrix.nome
                                 arq.tmp = strsplit(arq.tmp, split = "\\.")[[1]]
                                 arq.tmp[2] = "tmp"
                                 arq.tmp = paste0(arq.tmp, collapse = ".")
                                 
                                 cat(sprintf("%s\n",tabela.final), file = arq.tmp, append = F)
                                 
                                 tabela.combinacoes = fread(arq.tmp, header = F)
                                 names(tabela.combinacoes) = parametros
                                 
                                 tabela.combinacoes$tipo = tipo
                                 tabela.combinacoes$cultivar = i
                                 tabela.combinacoes$id = 1:dim(tabela.combinacoes)[1]
                                 
                                 otp = OutputClass$new()
                                 otp$write.table.combination(tabela.combinacoes, private$matrix.nome, first)
                                 unlink(arq.tmp) 
                                 
                                 if(first){
                                   first = F
                                 }
                                 
                              }
                             },
                             
                             doubleToBin = function(number, precisao){
                               
                               res = rep(0,precisao)
                               contador = precisao
                               
                               while(number >= 1){
                                 resto = number%%2
                                 res[contador] = as.integer(resto)
                                 number = number/2
                                 contador = contador - 1
                               }
                               
                               return(res)
                             },
                             
                             getCombination = function(matr.col, vetor){
                               
                               vetor = as.numeric(vetor)
                               result = matr.col * vetor
                               return(paste0(result, collapse = ";"))
                             } 
                             
                           )
                           #========================#
)