start.binary.limit = function(arq.config){
     
     load.datas()
     
     input = config.treatment(arq.config)
     input$VMax = as.numeric(input$VMax)
     input$VMin = as.numeric(input$VMin)
     input$RateMax = as.numeric(input$RateMax)
     input$RateMin = as.numeric(input$RateMin)
     
     identificadores = input$IDs
     coeficientesGeneticos = input$CGs
     
     header = c("cultivar","parametro","ValorMaximo(%)","ValorMinimo(%)","ValorBase")
     resultado = matrix(header,nrow = 1)
     
     xf = XFileClass$new(input$XFile)
     xf$validateCultivar(input$IDs)
     
     tabelaMap = xf$getResultado()
     
     
     for( i in 1:length(identificadores) ){
          
          cat(sprintf("[%s] Iniciando variacao para o cultivar: %s\n", Sys.time(), identificadores[i]))
          
          baseline = BaselineClass$new(tabelaMap, arquivoResposta = input$SummaryFile, output = input$baselineTableFile)
          
          for(cg in coeficientesGeneticos){
               
               parametros = list(arquivoCultivar = input$CultivarFile, cultivarId = identificadores[i], parametro = cg)
               
               gen = GenerationClass$new(baseline, input$RateMax, input$RateMin)
               bin = BinaryClass$new(tabelaMap, limiar = input$Limit, summaryFile = input$SummaryFile, rateMin = input$RateMin, rateMax = input$RateMax)
               
               cat(sprintf("[%s] Calculando variacao maxima para o Coeficiente genetico %s\n", Sys.time(), cg))
               
               binParamet = list(type = "max",valor_min = 0, valor_max = input$VMax)
               maxV = bin$start(gen, binParamet, parametros)
               
               cat(sprintf("[%s] Calculando variacao minima para o Coeficiente genetico %s\n", Sys.time(), cg))
               
               binParamet = list(type = "min",valor_min = input$VMin, valor_max = 0)
               minV = bin$start(gen, binParamet, parametros)
               
               if(maxV[2] != minV[2]){
                    stop(sprintf("O valor base utilizado para a variacao maxima-minima sao diferentes.\nValor Base Max: %s\nValor Base Min: %s\n", maxV[2], minV[2]))
               }
               
               resultado = rbind(resultado,c(parametros$cultivarId,parametros$parametro, maxV[1], minV[1], maxV[2]))
          }
          
     }
     
     arquivoResultado = sprintf("output//csv//%s",input$OutputTableFile)
     
     write.table(resultado,file = arquivoResultado, sep = ";", col.names = F, row.names = F, append = F)
     res = read.csv(arquivoResultado, sep = ";")
     res$ValorBase = NULL
     
     tbl = melt(res, id.vars =c("cultivar","parametro")  , value.name="valor", variable.name="Variable", na.rm=TRUE)
     
     tbl$cultivar = as.factor(tbl$cultivar)
     tbl$parametro = as.factor(tbl$parametro)
     
     ggplot(tbl, aes(x=parametro, y=valor, fill=Variable, group = Variable)) + geom_point(stat="identity", aes(colour = Variable, shape = Variable), size = 5) +
          geom_hline(yintercept = 0) + ggtitle("Valores de variacao para os CG's dos cultivares") +
          facet_grid(. ~ cultivar) + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) +
          labs(y = "Valor(%)", x = "Coeficiente genetico")
     
     ggsave(sprintf("output//plot//%s",input$OutputPlotFile))
}

start.binary.combination = function(arq.config){
     
     load.datas()
     
     input = config.treatment(arq.config)
     
     tabela.nome = input$OutputTableFile
     matrix.nome = input$APCOutput
     
     xf = XFileClass$new(input$XFile)
     xf$validateCultivar(input$IDs)
     
     tabelaMap = xf$getResultado()
     identificadores = unique(tabelaMap$cultivar)
     
     combination = CombinationClass$new(tabela.nome, matrix.nome, identificadores)
     
     variationTable = combination$getTable(as.numeric(input$Coverage))
     
     variationTable$valid = NA
     variationTable$status = NA
     variationTable$mean_CWAM = NA
     variationTable$mean_HWAM = NA
     variationTable$mean_LAIX = NA
     
     otp = OutputClass$new()
     
     nome.saida = input$APCOutput
     nome.saida = strsplit(nome.saida, split = "\\.")[[1]]
     nome.saida = nome.saida[1]
     
     for( i in 1:length(identificadores) ){
       
       cat(sprintf("[%s] Iniciando variacao composta para o cultivar: %s\n", Sys.time(), identificadores[i]))
       
       variationCultivarTable = variationTable[variationTable$cultivar == identificadores[i],]
       baseline = BaselineClass$new(tabelaMap, arquivoResposta = input$SummaryFile, output = input$baselineTableFile)
       
       tamanho.max = dim(variationCultivarTable)[1]
       for(linha in 1:tamanho.max){ # 

         cat(sprintf("[%s] Calculando variacao composta para a combinacao %s de %s\n", Sys.time(), linha,tamanho.max))
         
         tx = variationCultivarTable[linha,-c(18:25),with = F]
         cg = names(tx)
         parametros = list(arquivoCultivar = input$CultivarFile, cultivarId = identificadores[i], parametro = cg, taxa = tx)
         
         #===================================#
         # Gerando objeto de variacao
         variation = DataVariationClass$new(parametros, tabelaMap, arquivoResposta = input$SummaryFile)
         
         # Iniciando validacao dos dados
         go = GenerationClass$new(baseline, input$RateMax, input$RateMin)
         go$analizeData(variation, parametros$cultivarId)
         
         valid = go$isValid()
         
         tabela = go$getValidationTable()
         tabela$id = linha
         
         variationCultivarTable$valid[linha] = valid
         variationCultivarTable$status[linha] = attr(valid,"result")
         variationCultivarTable$mean_CWAM[linha] = mean(tabela$taxa_CWAM,na.rm = T)
         variationCultivarTable$mean_HWAM[linha] = mean(tabela$taxa_HWAM,na.rm = T)
         variationCultivarTable$mean_LAIX[linha] = mean(tabela$taxa_LAIX,na.rm = T)
         
         nome = sprintf("%s_variacao_composta",identificadores[i])
         otp$write.table(tabela, nome, (linha != 1) )
         
         browser()
        }
       
       otp$write.table(variationCultivarTable,nome.saida, (i != 1))
     }
}

config.treatment = function(arq.config){
     
     con = file(arq.config)
     open(con)
     
     linhas = readLines(con)
     linhas = linhas[linhas != ""]
     linhas = linhas[-grep("!",linhas)]
     
     input = list()
     for(i in linhas){
          i = strsplit(i,"=")[[1]]
          
          index = gsub(" ","",i[1])
          valor = gsub(" ","",i[2])
          valor = strsplit(valor,",")[[1]]
          
          input[[index]] = valor
     }
     
     close(con)
     
     return(input)
}

clear.files.out = function(){
     
     arquivos.out = list.files("DSSAT", full.names = T, pattern = ".OUT")
     unlink(arquivos.out)
     gc()
}

load.datas = function(){
     
     require(data.table)
     require(plyr)
     require(reshape2)
     require(ggplot2)
     require(grid)
     require(R6)
     
     src = list.files("src",full.names = T)
     status = sapply(src,source)
     
     clear.files.out()  
}