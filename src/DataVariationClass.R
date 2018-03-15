DataVariationClass = R6Class("DataVariationClass",
                             
                             #========================#
                             # Metodos publicos       #
                             public = list(
                               
                               initialize = function(parametros, tabelaMap, arquivoResposta = "Summary.OUT"){
                                 
                                 browser()
                                 
                                 # parametros: arquivoCultivar, cultivarId, idIndex, parametro, taxa
                                 private$defaultLine = private$identify.cul(parametros$arquivoCultivar, parametros$cultivarId)
                                 
                                 tx = parametros$taxa
                                 cg = parametros$parametro
                                 tamanho = length(tx)
                                 
                                 if(!is.data.table(tx)){
                                   tx = data.table(tx)
                                 }
                                 
                                 for(i in 1:tamanho){
                                   var.parametro = cg[i]
                                   var.taxa = tx[,i,with = F]
                                   private$set.variation(var.parametro, var.taxa, i != 1)
                                 }
                                 
                                 # Atualizando arquivoCultivar
                                 private$export.variation(parametros$arquivoCultivar)
                                 
                                 # Criando uma instancia da classe de sistema
                                 variation = SystemClass$new()
                                 
                                 tabela = variation$R.cropgro(arquivoResposta)
                                 
                                 tabelaMap$ID = NULL
                                 
                                 setkey(tabela,TNAM)
                                 setkey(tabelaMap,TNAM)
                                 
                                 tabela = merge(tabela, tabelaMap)
                                 
                                 private$variationTable = tabela
                                 
                                 # Atualizando arquivoCultivar
                                 private$export.variation(parametros$arquivoCultivar, DEFAULT = TRUE)
                               },
                               
                               # metodo de retorno
                               getVariationTable = function(){
                                 return(private$variationTable)
                               },
                               
                               getVariationLine = function(){
                                 return(private$variationLine)
                               },
                               
                               getDefaultLine = function(){
                                 return(private$defaultLine)
                               },
                               
                               getParametro = function(){
                                 return(private$parametro)
                               }
                             ),
                             #========================#
                             
                             #========================#
                             # Metodos privados       #
                             private = list(
                               
                               # Tabela summary
                               variationTable = data.table(),
                               defaultLine = c(),
                               multipleLine = c(),
                               variationLine = c(),
                               parametro = c(),
                               idIndex = c(),
                               
                               #=============================================================#
                               # Identificar cultivares de arquivo .CUL                      #
                               
                               identify.cul = function(arquivo, identificador){
                                 
                                 arquivo = sprintf("DSSAT//%s",arquivo)
                                 
                                 # iniciando conexao com o arquivo
                                 con = file(arquivo)
                                 open(con)
                                 
                                 # removendo comentarios
                                 linhas = suppressWarnings( readLines(con) )
                                 
                                 verificar = linhas[-grep("!",linhas)]
                                 
                                 if(length(verificar) > 0){
                                   
                                   linhas = verificar         
                                 }
                                 
                                 # encerrando conexao
                                 close(con)
                                 
                                 # obtendo cabecalho
                                 cabecalho = linhas[grep("@",linhas)]
                                 cabecalho = strsplit(cabecalho," ")[[1]]
                                 cabecalho = cabecalho[cabecalho != ""]
                                 
                                 # obtendo linha procurada
                                 linha_id = linhas[grep(identificador,linhas)]
                                 
                                 if(length(linha_id) > 1){
                                   stop(sprintf("Foram encontradas %s ocorrencias do Id: '%s'",length(linha_id),identificador))
                                 }
                                 
                                 # tratando linha
                                 
                                 linha_id = strsplit(linha_id," ")[[1]]
                                 linha_id = linha_id[linha_id != ""]
                                 
                                 size_diff = length(linha_id) - length(cabecalho)
                                 
                                 id_name = linha_id[2:(2+size_diff)]
                                 id_name = paste(id_name,collapse = " ")
                                 
                                 linha_id = linha_id[-(2:(2+size_diff))]
                                 linha_id = c(linha_id[1],id_name,linha_id[2:length(linha_id)])
                                 
                                 # gerando matrix
                                 linha_final = matrix(linha_id[1:22],nrow = 1)
                                 
                                 
                                 # adicionando cabecalho
                                 colnames(linha_final) = cabecalho
                                 
                                 # retornando linha
                                 return(linha_final)
                               },
                               
                               #                                                             #
                               #=============================================================#
                               
                               #=============================================================#
                               # gerar variacao                                              #
                               
                               set.variation = function(parametro, taxa, MULTIPLE = F){
                                 
                                 if(!MULTIPLE){
                                   linha_obtida = private$defaultLine
                                   private$multipleLine = linha_obtida
                                   linha_obtida = linha_obtida[,5:dim(linha_obtida)[2]]
                                   
                                 }else{
                                   linha_obtida = unlist(private$multipleLine)
                                 }
                                 
                                 # definindo parametros validos
                                 parametros_validos = names(linha_obtida)
                                 
                                 verificar = which(parametro == parametros_validos)
                                 
                                 if(length(verificar) == 0){
                                   stop(sprintf("O parametro '%s' nao existe ou nao pode ser alterado",parametro))
                                 }
                                 
                                 # gerando variacao
                                 valor_base = linha_obtida[verificar]
                                 valor_base = as.numeric(as.character(valor_base))
                                 
                                 if(valor_base == 0){
                                   stop(sprintf("O valor de '%s' e 0",parametro))
                                 }
                                 
                                 # variando valor
                                 valor_variado = valor_base * ((100 + taxa)/100)
                                 
                                 linha_obtida[verificar] = valor_variado
                                 
                                 private$variationLine = linha_obtida
                                 private$parametro = parametro
                                 private$multipleLine = linha_obtida
                               },
                               
                               #                                                             #
                               #=============================================================#
                               
                               #=============================================================#
                               # exportando linha                                            #
                               
                               export.variation = function(arquivo_saida, DEFAULT = F){
                                 
                                 arquivo_saida = sprintf("DSSAT//%s",arquivo_saida)
                                 
                                 if(DEFAULT){
                                   linha = private$defaultLine  
                                 }else{
                                   linha = private$defaultLine[1:4]
                                   linha = c(linha,private$variationLine)
                                 }
                                 
                                 id = linha[1]
                                 id = as.character(id)
                                 
                                 # iniciando conexao com o arquivo
                                 con = file(arquivo_saida)
                                 open(con,open = "r+")
                                 
                                 # removendo comentarios
                                 linhas = suppressWarnings( readLines(con) )
                                 
                                 verificar = linhas[-grep("!",linhas)]
                                 
                                 if(length(verificar) > 0){
                                   
                                   linhas = verificar         
                                 }
                                 
                                 linha_id = grep(id,linhas)
                                 
                                 padrao = "------ ---------------- ----- ------ ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- "
                                 padrao.sinal = c("-","-","+","+","+","+","+","+","+","+","+","+","+","+","+","+","+","+","+","+","+","+")
                                 padrao.space = strsplit(padrao,split = " ")[[1]]     
                                 padrao.space = nchar(padrao.space)
                                 
                                 for(i in 1:length(padrao.space)){
                                   
                                   valor_variavel = as.character(linha[i])
                                   if(nchar(valor_variavel) > padrao.space[i]){
                                     
                                     valor_variavel = substring(valor_variavel,1,padrao.space[i])
                                   }
                                   
                                   local_space = paste("%",padrao.sinal[i],padrao.space[i],"s",collapse = "")
                                   local_space = gsub(" ","",local_space)
                                   
                                   variavel = sprintf(local_space, valor_variavel)
                                   
                                   if(i == 1){
                                     
                                     saida = variavel   
                                   }else{
                                     
                                     saida = c(saida, variavel)
                                   }
                                 }
                                 
                                 saida = paste(saida, collapse = " ")
                                 
                                 linhas[linha_id] = saida
                                 
                                 # sobrescrevedo arquivo cultivar
                                 first = T
                                 for(i in linhas){
                                   cat(sprintf("%s\n",i), file = con, append = !first)
                                   first = F
                                 }
                                 
                                 # encerrando conexao
                                 close(con)
                               }
                               
                               #                                                             #
                               #=============================================================#
                               
                               
                             )
                             #========================#
)