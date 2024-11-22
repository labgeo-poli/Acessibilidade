####### Accessibility computation and visualisation #######
############ 2SFCA accessibility computation ##############
############## LABGEO USP - Beatriz Moura ################
##################### Maio 2018  #########################


  ###   Carrega as bibliotecas instaladas anteriormente
library(classInt)
library(stringr)
library(sf)
library(reshape)
library(ggplot2)
library(RColorBrewer)

### define o diretório de trabalho. << ATENCAO!!! IMPORTANTE ALTERAR PARA DIRETORIO CORRETO ANTES DE RODAR O SCRIPT!!!>>
  setwd("C:\Laboratorios\ae_acessibilidade")
  
  ### Leitura do arquivo de empregos (separador de colunas ";")
  jobs<-read.csv2("Jobs_Dj_2017_05_xy_SP_v3.csv", sep = ";")
  
  ### Leitura do arquivo de populacaos (separador de colunas ";")
  poptot<-read.csv2("SP_occupational_population.csv", sep=";")
  
  ### Leitura da matriz OD de tempos de viagem (separador de colunas ";" e não tem cabeçalho)
  matrix<-read.csv2("PT_Time_SP_v3.csv", sep=";", header = FALSE)
  
  ### transpondo matriz
  matrix <- t(matrix)
  
  ### Cria matriz vazia com o número de subzonas da RMSP
  tax <-array(, dim = c(1,1895))
  
  ### Cria variavel vazia
  acctot <-c()
  
  ### matriz onde sera armazenada a frequencia
  freq <- array(, dim = c(1895,1895))
  
  ## pre step
  ## Cria a matriz binária indicando as posições onde a zona está dentro do tempo.
  ## A função a seguir, marca com 1 a zona dentro do tempo .
  processaLinhaMatriz <- function (linha) {
    linhaFreq <- rep (NA, length(linha))
    ## Tempo abaixo do limite.
    linhaFreq[which(linha <= 60)] <- 1
    return(linhaFreq)
  }
  # Executa a função para cada linha.
  freq <- apply(matrix, 1, processaLinhaMatriz)
  
  ### transforma os registros sem valor (NA) em 0 para podermos somar a coluna e mostrar o valor da frequencia por zona.
  freq[is.na(freq)] <- 0
  f <- array(, dim = c(1,1895))
  for (w in 1:1895){
    f[w] <- sum(freq[w,1:1895])
  }
  
  ### step 1
  for (i in 1:length(matrix[,1])) {
    
    # matriz de tempo de viagem
    t <- matrix[,i]
    popt<-0
    
    ### Primeiro passo
    ### soma da população e calculo da taxa (empregos/soma populacao)
    ### para cada setor censitario
    
    for (j in 1:length(t)){
      
      # Se o tempo de viagem menor que 60 minutos
      if (t[j] <= 60){
        
        # soma da populacao
        popt<-popt + sum(poptot[j, 2:9])/f[j] 
      }
    }
    
    # obtem a taxa
    tax[i] <- (jobs[i, 5])/popt
    
  }
  
  ### segundo passo
  ### soma de todas as taxas para cada setor censitario
  
  ### considerando o tamanho da matriz OD de tempos de viagem
  for (i in 1:length(matrix[,1])) {
    
    ### matriz de tempos de viagem 
    t <- matrix[,i]
    taxTotal<-0
    
    for (j in 1:length(t)){
      
      ### Se tempo de viagem menor do que 60 minutos
      if (t[j]<=60){
        taxTotal<-taxTotal+tax[j]
      }
    }
    
    ### cria matriz contendo o total das taxas por subzonas
    acctot[i] <- taxTotal
    
  }
  
  ###calculo do valor final da acessibilidade 2SFCA
  identif <- seq(1,1895)
  acctot <- data.frame(identif,acctot)
  acctot2 <- replace(acctot$acctot, is.infinite(acctot$acctot), NA)
  acctot <- data.frame(identif,acctot, acctot2)
  
  ############# ATENCAO!!! PERGUNTA PARA RESPONDER NO FORMULARIO ################
  
  ###calcule a acessibilidade Cumulativa para o registro com ID 1294 (subzona da USP)
  acctot[1294,]
  
  ###############################################################################
  
  ### Criação dos intervalos de classes do mapa
  natural.interval <- classIntervals(acctot$acctot2,10, style = "jenks")$brks
  std.interval <- classIntervals(acctot$acctot2,10, style = "sd")$brks
  quantile.interval <- classIntervals(acctot$acctot2,10, style = "quantile")$brks
  equal.interval<- classIntervals(acctot$acctot2,10, style = "equal")$brks
  kmeans.interval <- classIntervals(acctot$acctot2,10, style = "kmeans")$brks
  lable.natural <- natural.interval[1:length(natural.interval)]
  lable.std <- std.interval[1:length(std.interval)]
  lable.quantile <- quantile.interval[1:length(quantile.interval)]
  lable.equal <- equal.interval[1:length(equal.interval)]
  lable.kmeans <- kmeans.interval[1:length(kmeans.interval)]
  acctot$interval0 <- findInterval(acctot$acctot2,natural.interval)
  acctot$natural <- round(natural.interval[acctot$interval0],0)
  acctot$interval1 <- findInterval(acctot$acctot2,std.interval)
  acctot$std <- round(std.interval[acctot$interval1],0)
  acctot$interval2 <- findInterval(acctot$acctot2,quantile.interval)
  acctot$quantile <- round(quantile.interval[acctot$interval2],0)
  acctot$interval3 <- findInterval(acctot$acctot2,equal.interval)
  acctot$equal <- round(equal.interval[acctot$interval3],0)
  acctot$interval4 <- findInterval(acctot$acctot2,kmeans.interval)
  acctot$kmeans <- round(kmeans.interval[acctot$interval4],0)
  acctot$natural <- as.character(acctot$natural)
  acctot$std <- as.character(acctot$std)
  acctot$quantile <- as.character(acctot$quantile)
  acctot$equal <- as.character(acctot$equal)
  acctot$kmeans <- as.character(acctot$kmeans)
  acctot$equal <- str_pad(acctot$equal,7,"left",pad="0")
  acctot$natural <- str_pad(acctot$natural,7,"left",pad="0")
  acctot$std <- str_pad(acctot$std,7,"left",pad="0")
  acctot$quantile <- str_pad(acctot$quantile,7,"left",pad="0")
  acctot$kmeans <- str_pad(acctot$kmeans,7,"left",pad="0")
  
  ### Leitura do arquivo shapefile da area construida da RMSP e criação de novo arquivo shapefile com acessibilidades
  shp <- st_read("CPTM_RMSP.shp")
  acctot <- rename(acctot, c(identif="CODE"))
  combine <- merge (shp, acctot, by='CODE')
  st_write(combine, "Acessibilidade_2SFCA_PopTotal_PuT_60Min.shp", append=FALSE)
  map <- st_read(dsn=".",layer= "Acessibilidade_2SFCA_PopTotal_PuT_60Min")
  map <- st_transform(map,crs="epsg:32723")
  
    ### Se escolher classificar o mapa em quebras naturais
    colourCount = length(unique(acctot$natural[!is.na(acctot$natural)]))
    col <- rev(brewer.pal(10, "Spectral"))
    pal.natural <- colorRampPalette(col)(colourCount)
    ncolour.natural <- sort(unique(acctot$interval0[!is.na(acctot$interval0)]))
    pal.natural2 <- pal.natural[ncolour.natural]
    plot.natural = ggplot(map[!is.na(map$natural),]) +
      geom_sf(aes(fill=natural), colour = NA) +
      scale_fill_manual(values =pal.natural2) +
      theme(axis.text=element_blank()) +
      labs(title="Acessibilidade aos empregos PuT 60 minutos 2SFCA", x="", y="")
    plot(plot.natural)
    
    ### Se escolher classificar o mapa em quantil
    pal.quantile <- colorRampPalette(col)(colourCount)
    ncolour.quantile <- sort(unique(acctot$interval2))
    pal.quantile2 <- pal.quantile[ncolour.quantile]
    plot.quantile = ggplot(map[!is.na(map$quantile),]) +
      geom_sf(aes(fill=quantile), colour = NA) +
      scale_fill_manual(values =pal.quantile2) +
      theme(axis.text=element_blank()) +
      labs(title="Acessibilidade aos empregos PuT 60 minutos 2SFCA", x="", y="")
    plot(plot.quantile)
    
    ### Se escolher classificar o mapa em intervalos iguais
    colourCount = length(unique(acctot$equal[!is.na(acctot$equal)]))
    pal.equal <- colorRampPalette(col)(colourCount)
    ncolour.equal <- sort(unique(acctot$interval3))
    pal.equal2 <- pal.equal[ncolour.equal]
    plot.equal = ggplot(map[!is.na(map$equal),]) +
      geom_sf(aes(fill=equal), colour = NA) +
      scale_fill_manual(values =pal.equal2) +
      theme(axis.text=element_blank()) +
      labs(title="Acessibilidade aos empregos PuT 60 minutos 2SFCA", x="", y="")
    plot(plot.equal)

    ############# ATENCAO!!! PERGUNTA PARA RESPONDER NO FORMULARIO ################
    
    ### QUAL O VALOR MÁXIMO DE ACESSIBILIDADE ENCONTRADO NO MAPA DE INTERVALOS IGUAIS (está na legenda)??
    
    
    ###############################################################################
