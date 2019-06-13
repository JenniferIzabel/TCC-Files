library(dplyr)    

open <- function(language, cat){
  link <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Categoria/",language,"-",cat,".csv", sep="")
  repoCat <- read.csv(link)
  
  repos <- unique(repoCat$Repository)
  
  i <- 1
  while(i <= length(repos)){# para cada repositório
    repo <- noquote(toString(repos[i]))
    rep <- repoCat[repoCat$Repository == repo,]
    users <- unique(rep$User)
    
    
    print(repo)
    qtduser <- length(users)
    j <- 1
    while(j <= qtduser){# para cada usuário de um repositório
      

      user <- toString(users[j])
      
      #nomes de usuario que eram compostos apenas por numeros estavam dando problema lá na frente, 
      #então se ele conseguir converter o nome pra int vai dar pau
      #por isso só vai executar quando der erro nessa conversão
      tryCatch(
        user <- integer(user)
      , error = function(e){
       
        print(user)
        search(language, repo, user, cat)
      })
      
      
      
      j <- j + 1
    }
    i <- i + 1
  }
}
search <- function(language, repo, user, cat){
  linkgrouped <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Categoria/GroupedPulls-",repo, sep = "")
  groupedfile <- read.csv(linkgrouped)
  
  linhas <- findline(toString(language), toString(repo), toString(user), toString(cat))
  
  situation <- type(linhas[1,])
  
  pull <- toString(linhas[1,]$Pull.number)
  
  row <- data.frame(repo, user, pull, globalTest, situation, sep=",")

  savesummary(row, language, cat)
}

findline <- function(language, repo, user, cat){
  rep <- gsub(".csv", "", repo)
  linknotest <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/",rep, "-NoTest.csv", sep = "")
  repnotest <- read.csv(linknotest)
  linktest <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/",rep,".csv", sep = "")
  reptest <- read.csv(linktest)

  row <- filter(reptest, Users == user)
  rowno <- filter(repnotest, Users == user)
  
  rows <- getAllLines(row, rowno)
  saveAllLines(row, language, cat)
  
  return(rows)
}

type <- function(linha){
  dateCreated <- strsplit(toString(linha$DateCreated), ",")
  dateUpdated <- strsplit(toString(linha$dateUpdated), ",")
  dateClosed <- strsplit(toString(linha$dateClosed), ",")
  dateMerged <- strsplit(toString(linha$dateMerged), ",")
  
  if(nchar(dateMerged[1]) > 3){
    return("Merged")
  }else if(nchar(dateClosed[1]) > 3){
    return("Closed")
  }else{
    return("Open")
  }
}

savesummary <- function(row, language, cat){
  link <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Categoria/Summary",cat,".csv", sep="")
  if(!file.exists(link)){
    rowsFst <- data.frame("Repository","User", "Pull Number", "Test", "Situation", sep=",")
    write.table(rowsFst,file = link, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
  }
  write.table(row, file = link, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
}

getAllLines <- function(row, rowno){
  r <- NULL
  s <- NULL
  x <- 1
  while(toString(row[x,1]) != "NA"){
    r <- row[x,]
    x <- x + 1
  }
  x <- 1
  while(toString(rowno[x,1]) != "NA"){
    s <- rowno[x,]
    x <- x + 1
  }
  
  if(is.null(r) && !is.null(s)){
    globalTest <<- FALSE
    t <- data.frame(s)
  }else if(is.null(s) && !is.null(r)){
    globalTest <<- TRUE
    t <- data.frame(r)
  }else if(!is.null(s) && !is.null(r)){
    globalTest <<- TRUE
    t  <- data.frame(rbind(as.matrix(r), as.matrix(s)))
  }
  return(t)
}

saveAllLines <- function(row, language, cat){
  link <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Categoria/AllLines",cat,".csv", sep="")
  if(!file.exists(link)){
    rowsFst <- data.frame("Repository", "Pull state", "Pull number", "sha", "Filename", "File status", "Additions", "Deletions", "Users", "DateCreated", "DateUpdated", "dateClosed", "dateMerged", sep=",") 
    write.table(rowsFst,file = link, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
  }
 write.table(row, file = link, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
  
}

globalTest <<- FALSE

##########################

language <- "CPlusPlus"

cat <- "Novato"
open(language, cat)

cat <- "Casual"
open(language, cat)

cat <- "Regular"
open(language, cat)


