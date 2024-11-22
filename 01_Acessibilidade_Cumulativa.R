####### Accessibility computation and visualisation #######
######### Cumulative accessibility computation ###########
###################### CASA UCL ##########################
##################### June 2017  #########################


  ###   Carrega as bibliotecas instaladas anteriormente
  library(classInt)
  library(stringr)
  library(sf)
  library(reshape)
  library(ggplot2)
  library(RColorBrewer)
  
  ### define o diretório de trabalho. << ATENCAO!!! IMPORTANTE ALTERAR PARA DIRETORIO CORRETO ANTES DE RODAR O SCRIPT!!!>>
  setwd("C:\Laboratorios\ae_acessibilidade")
  
  ### Leitura do arquivo de empregos (Possui cabeçalho, separador de colunas ";" e decimal ",")
  Dj0 <- read.csv("Jobs_Dj_2017_05_xy_SP_v3.csv", header = TRUE, sep = ";", dec = ",")
  
  ### Remove a linha 1317 do arquivo de empregos, pois não existe essa zona no arquivo shapefile
  Dj0 <- Dj0[-1317,] 
  
  ### Cria uma variável vazia com os campos abaixo 
  Dj1 <- c("OBJECTID","code","X","Y", "workplaceP")
  Dj2 <- c("workplaceP")
  
  ### Atribui a nova variável Dj o nome do campo "workplaceP" e os valores de empregos 
  Dj <- Dj0[Dj2]
  
  ### Leitura da matriz OD de tempos de viagem por transporte público (não tem cabeçalho, separador de colunas ";" e decimal ",")
  dij2 <- read.csv("PT_Time_SP_v3.csv", header=FALSE, sep = ";", dec = ",")
  
  ### Remove a linha e a coluna 1317 da matriz de tempos, pois não existe essa zona no arquivo shapefile
  dij2 <- dij2[-1317, -c(1317)]
  
  ### Trata os tempos de viagem da Matriz OD e aplica o limiar de busca de empregos
  dij2[dij2==0] <- 1
  dij2[dij2>60] <- 0
  dij2[dij2>0] <- 1
  
  ### Cria matriz de empregos com o mesmo número de linhas e colunas que a matriz de tempos
  pop <- apply(Dj,1,sum)
  pop <- matrix(pop,NROW(dij2),NCOL(dij2))
  pop <- t(pop)
  
  ### Calcula os empregos que estão dentro do limiar de tempo de viagem parametrizado 
  total <- dij2*pop
  result <- apply(total,1,sum)
  result <- result[1:NCOL(dij2)]
  
  ### Calculo da acessibilidade cumulativa
  cumacc <- result
  Dj<-Dj0[Dj1]
  result <- cbind (Dj,cumacc)
  
  ############# ATENCAO!!! PERGUNTA PARA RESPONDER NO FORMULARIO ################
  
  ###calcule a acessibilidade Cumulativa para o registro com ID 1294 (subzona da USP)
  result[1294,]
  
  ###############################################################################
  
  ### Criação dos intervalos de classes do mapa
  natural.interval <- classIntervals(result$cumacc,10, style = "jenks")$brks
  std.interval <- classIntervals(result$cumacc,10, style = "sd")$brks
  quantile.interval <- classIntervals(result$cumacc,10, style = "quantile")$brks
  equal.interval<- classIntervals(result$cumacc,10, style = "equal")$brks
  kmeans.interval <- classIntervals(result$cumacc,10, style = "kmeans")$brks
  lable.natural <- natural.interval[1:length(natural.interval)]
  lable.std <- std.interval[1:length(std.interval)]
  lable.quantile <- quantile.interval[1:length(quantile.interval)]
  lable.equal <- equal.interval[1:length(equal.interval)]
  lable.kmeans <- kmeans.interval[1:length(kmeans.interval)]
  result$interval0 <- findInterval(result$cumacc,natural.interval)
  result$natural <- round(natural.interval[result$interval0],0)
  result$interval1 <- findInterval(result$cumacc,std.interval)
  result$std <- round(std.interval[result$interval1],0)
  result$interval2 <- findInterval(result$cumacc,quantile.interval)
  result$quantile <- round(quantile.interval[result$interval2],0)
  result$interval3 <- findInterval(result$cumacc,equal.interval)
  result$equal <- round(equal.interval[result$interval3],0)
  result$interval4 <- findInterval(result$cumacc,kmeans.interval)
  result$kmeans <- round(kmeans.interval[result$interval4],0)
  result$natural <- as.character(result$natural)
  result$std <- as.character(result$std)
  result$quantile <- as.character(result$quantile)
  result$equal <- as.character(result$equal)
  result$kmeans <- as.character(result$kmeans)
  result$equal <- str_pad(result$equal,7,"left",pad="0")
  result$natural <- str_pad(result$natural,7,"left",pad="0")
  result$std <- str_pad(result$std,7,"left",pad="0")
  result$quantile <- str_pad(result$quantile,7,"left",pad="0")
  result$kmeans <- str_pad(result$kmeans,7,"left",pad="0")
  
  ### Leitura do arquivo shapefile da area construida da RMSP e criação de novo arquivo shapefile com acessibilidades
  shp <- st_read("CPTM_RMSP.shp")
  result <- rename(result, c(code="CODE"))
  combine <- merge(shp, result, by='CODE')
  st_write(combine, "Acessibilidade_Cumulativa_PuT_60min.shp", append=FALSE)
  map <- st_read(dsn=".",layer= "Acessibilidade_Cumulativa_PuT_60min")
  map <- st_transform(map,crs="epsg:32723")

  ### Se escolher classificar o mapa em quebras naturais
    colourCount = length(unique(result$natural))
    col <- rev(brewer.pal(10, "Spectral"))
    pal.natural <- colorRampPalette(col)(colourCount)
    ncolour.natural <- sort(unique(result$interval0))
    pal.natural2 <- pal.natural[ncolour.natural]
    plot.natural =  ggplot(map) +
      geom_sf(aes(fill=natural), colour = NA) +
      scale_fill_manual(values =pal.natural2) +
      theme(axis.text=element_blank()) +
      labs(title="Acessibilidade Cumulativa aos Empregos PuT 60 Minutos", x="", y="")
    plot(plot.natural)
  
  ### Se escolher classificar o mapa em quantil
    pal.quantile <- colorRampPalette(col)(colourCount)
    ncolour.quantile <- sort(unique(result$interval2))
    pal.quantile2 <- pal.quantile[ncolour.quantile]
    plot.quantile = ggplot(map) +
      geom_sf(aes(fill=quantile), colour = NA) +
      scale_fill_manual(values =pal.quantile2)+
      theme(axis.text=element_blank())+
      labs(title="Acessibilidade Cumulativa aos Empregos PuT 60 Minutos", x="", y="")
    plot(plot.quantile)
  
  ### Se escolher classificar o mapa em intervalos iguais
    colourCount.equal = length(unique(result$equal))
    pal.equal <- colorRampPalette(col)(colourCount)
    ncolour.equal <- sort(unique(result$interval3))
    pal.equal2 <- pal.equal[ncolour.equal]
    plot.equal = ggplot(map) +
      geom_sf(aes(fill=equal), colour = NA) +
      scale_fill_manual(values =pal.equal2)+
      theme(axis.text=element_blank())+
      labs(title="Acessibilidade Cumulativa aos Empregos PuT 60 Minutos", x="", y="")
    plot(plot.equal)

    ############# ATENCAO!!! PERGUNTA PARA RESPONDER NO FORMULARIO ################
    
    ### QUAL O VALOR MÁXIMO DE ACESSIBILIDADE ENCONTRADO NO MAPA DE INTERVALOS IGUAIS (está na legenda)??
    
    
    ###############################################################################
