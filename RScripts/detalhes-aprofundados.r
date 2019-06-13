library(dplyr)   

start <- function(language){
  ComTesteRejeitado <- 0
  ComTesteAceito <- 0
  SemTesteRejeitado <- 0
  SemTesteAceito <- 0
  ComTeste <- 0
  SemTeste  <- 0
  
}


open <- function(language, cat){
  start()
  
  savelink <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Categoria/DetalhesPorcentagem",cat,".csv", sep="")
  if(!file.exists(savelink)){
    rowsFst <- data.frame("Repository", "Test", "Situation", "Total", "% com/sem", "% total",sep=",") 
    write.table(rowsFst,file = savelink, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
  }
  
  link <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Categoria/Summary",cat,".csv", sep="")
  repoCat <- read.csv(link)
  
  repos <- unique(repoCat$Repository)
  
  i <- 1
  while(i <= length(repos)){# para cada repositório
    repo <- noquote(toString(repos[i]))
    print(repo)
    rep <- repoCat[repoCat$Repository == repo,] #todas as linhas do repositório
    com <- rep[rep$Test == "TRUE",]
    sem <- rep[rep$Test == "FALSE",]
    
    comOpen <- com[com$Situation == "Open",]
    comClosed <- com[com$Situation == "Closed",]
    comMerged <- com[com$Situation == "Merged",]
    semOpen <- sem[sem$Situation == "Open",]
    semClosed <- sem[sem$Situation == "Closed",]
    semMerged <- sem[sem$Situation == "Merged",]
    
    comOpen <- length(comOpen[,1])
    comClosed <- length(comClosed[,1])
    comMerged <- length(comMerged[,1])
    semOpen <- length(semOpen[,1]) 
    semClosed <- length(semClosed[,1]) 
    semMerged <- length(semMerged[,1])
    
    totalCom <- comOpen + comClosed + comMerged
    totalSem <- semOpen + semClosed + semMerged
    totalComSem <- totalCom + totalSem
      
    perComOpen <- round((comOpen*100)/totalCom, 2)
    perComClosed <- round((comClosed*100)/totalCom, 2)
    perComMerged <-round((comMerged*100)/totalCom, 2)
    perSemOpen <- round((semOpen*100)/totalSem, 2)
    perSemClosed <- round((semClosed*100)/totalSem, 2)
    perSemMerged <- round((semMerged*100)/totalSem, 2)
      
    perTTComOpen <- round((comOpen*100)/totalComSem, 2)
    perTTComClosed <- round((comClosed*100)/totalComSem, 2)
    perTTComMerged <- round((comMerged*100)/totalComSem, 2)
    perTTSemOpen <- round((semOpen*100)/totalComSem, 2)
    perTTSemClosed <- round((semClosed*100)/totalComSem, 2)
    perTTSemMerged <- round((semMerged*100)/totalComSem, 2)
    perTTCom <- perTTComOpen + perTTComClosed + perTTComMerged
    perTTSem <- perTTSemOpen + perTTSemClosed + perTTSemMerged
    
    
    ComTesteRejeitado <- round(ComTesteRejeitado + perTTComClosed, 2)
    ComTesteAceito <- round(ComTesteAceito + perTTComMerged, 2)
    SemTesteRejeitado <- round(SemTesteRejeitado +  perTTSemClosed, 2)
    SemTesteAceito <- round(SemTesteAceito + perTTSemMerged, 2)
    ComTeste <- round(ComTeste + perTTCom, 2)
    SemTeste  <- round(SemTeste + perTTSem, 2)

    rowcomOpen <- data.frame(repo, "TRUE", "Open", comOpen, perComOpen, perTTComOpen, sep = ",")
    write.table(rowcomOpen, file = savelink, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
    
    rowcomClosed <- data.frame(repo, "TRUE", "Closed", comClosed, perComClosed, perTTComClosed, sep = ",")
    write.table(rowcomClosed, file = savelink, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
    
    rowcomMerged <- data.frame(repo, "TRUE", "Merged", comMerged, perComMerged, perTTComMerged, sep = ",")
    write.table(rowcomMerged, file = savelink, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
    
    rowsemOpen <- data.frame(repo, "FALSE", "Open", semOpen, perSemOpen, perTTSemOpen, sep = ",")
    write.table(rowsemOpen, file = savelink, append = T, col.names = F, row.names = F, sep = ",",  quote = F)

    rowsemClosed <- data.frame(repo, "FALSE", "Closed", semClosed, perSemClosed, perTTSemClosed, sep = ",")
    write.table(rowsemClosed, file = savelink, append = T, col.names = F, row.names = F, sep = ",",  quote = F)

    rowsemMerged <- data.frame(repo, "FALSE", "Merged", semMerged, perSemMerged, perTTSemMerged, sep = ",")
    write.table(rowsemMerged, file = savelink, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
    
    i <- i + 1
  }

  resumo(cat, ComTesteRejeitado, ComTesteAceito, SemTesteRejeitado, SemTesteAceito, ComTeste, SemTeste)
    
}

resumo <- function(categoria, ComTesteRejeitado, ComTesteAceito, SemTesteRejeitado, SemTesteAceito, ComTeste, SemTeste){
  
  
  #divide por 10 pra fazer a media. as variaveis vêm com o somatorio dos 10 repositorios
  ComTesteRejeitado <- round(ComTesteRejeitado / 10, 2)
  ComTesteAceito <- round(ComTesteAceito / 10, 2)
  SemTesteRejeitado <- round(SemTesteRejeitado / 10, 2)
  SemTesteAceito <- round(SemTesteAceito / 10, 2)
  ComTeste <- round(ComTeste / 10, 2)
  SemTeste  <- round(SemTeste / 10, 2)

 
 
  savelink <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Categoria/DetalhesPorcentagemResumo.csv", sep="")
  
  if(!file.exists(savelink)){
    rowsFst <- data.frame("Categoria", "ComTesteRejeitado", "ComTesteAceito","SemTesteRejeitado", "SemTesteAceito", "ComTeste","SemTeste",sep=",") 
    write.table(rowsFst,file = savelink, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
  }
  row <- data.frame(categoria, ComTesteRejeitado, ComTesteAceito, SemTesteRejeitado, SemTesteAceito, ComTeste, SemTeste, sep = ",")
  write.table(row, file = savelink, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
  
}

ComTesteRejeitado <- 0
ComTesteAceito <- 0
SemTesteRejeitado <- 0
SemTesteAceito <- 0
ComTeste <- 0
SemTeste  <- 0

language <- "CPlusPlus"

open(language, "Novato")
open(language, "Casual")
open(language, "Regular")





