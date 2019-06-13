library(dplyr)  
library(tibble)


inicia <- function(language, cat){
  
  linkProv <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Categoria/Provisorio-",cat,".csv", sep="")

  # todosUsuariosTeste(language, cat, linkProv, "Merged") #Conta os ususarios com teste e merged
  # todosUsuariosTeste(language, cat, linkProv, "Closed") #Conta os ususarios com teste e closed
  

  selecionarUsuariosAleatorios(language, cat, "Merged")
  selecionarUsuariosAleatorios(language, cat, "Closed")
  
}
todosUsuariosTeste <- function(language, cat, linkProv, tipo){
  linkUser <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Categoria/Users",tipo,"WithTests",cat,".csv", sep="")
  ll <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Categoria/Summary",cat,".csv", sep="")
  arquivo <-  read.csv(ll)
  
  
  ### verificar os com teste
  comTeste <- arquivo[arquivo$Test == "TRUE",]

  verificaArquivoExiste(linkProv)
  write.table(comTeste, file = linkProv, append = T, col.names = F, row.names = F, sep = ",",  quote = F)

  ### verifica os merged
  arquivoMerged <-  read.csv(linkProv)
  
  merged <- arquivoMerged[arquivoMerged$Situation == tipo,]
  verificaArquivoExiste(linkUser)
  write.table(merged, file = linkUser, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
  
}

verificaArquivoExiste<- function(link){
  if(!file.exists(link)){
    rowsFst <- data.frame("Repository","User", "Pull number", "Test", "Situation", sep=",")
    write.table(rowsFst,file = link, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
  }
}

selecionarUsuariosAleatorios <- function(language, cat, tipo){
  linkUser <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Categoria/Users",tipo,"WithTests-",cat,".csv", sep="")
  linkQtd <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/QuantidadesParaAmostras.csv", sep="")
  arq <- read.csv(linkQtd)
  qtd <- arq[arq$Language==language, ]
  qtd <- qtd[qtd$Type==tipo,]
  qtdTotal <- qtd[[3]]
  qtdAmostra <- qtd[[4]]

  lines <- sample(1:qtdTotal, qtdAmostra, replace=FALSE)
  
  
  link <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Categoria/RandomUsers.csv", sep="")
  arqu <- read.csv(linkUser)
 
  print(language)
  print(tipo)
  
  for(l in lines){
    row <- arqu[l,]
    row <- add_column(row, Type = tipo, .after = 1)
    
    # p <- paste(row[[1]], row[[3]], row[[4]], sep = " - ")
    # print(p)
    
    write.table(row, file = link, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
    
  }
  print("####################")
}


start <- function(language){
  inicia (language, "Novato")
  # inicia (language, "Casual")
  # inicia (language, "Regular")
}

# language <- "C"
# start(language)
language <- "CPlusPlus"
start(language)
language <- "CSharp"
start(language)
language <- "Java"
start(language)
language <- "PHP"
start(language)
language <- "Python"
start(language)
language <- "Ruby"
start(language)

