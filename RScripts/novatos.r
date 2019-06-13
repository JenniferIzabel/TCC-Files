open <- function(language){
  link <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Categoria/Novato-",language,".csv", sep="")
  repoNovatos <- read.csv(link)
  
  repos <- unique(repoNovatos$Repository)
  
  i <- 1
  while(i < length(repos)){# para cada repositório
    repo <- noquote(toString(repos[i]))
    rep <- repoNovatos[repoNovatos$Repository == repo,]
    users <- rep$User
    
    print(repo)
    j <- 1
    while(j < length(users)){# para cada usuário de um repositório
      #user <- toString(noquote(toString(users[j])))
      user <- toString(users[j])
      
      print(user)
      row<- search(language, repo, user)
      savesummary(row, language)
      
      j <- j + 1
    }
    i <- i + 1
  }
}
search <- function(language, repo, user){
  linkrepofile <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/",repo, sep = "")
  repofile <- read.csv(linkrepofile)
  
  linhas <- repofile[repofile$Users == user,]
  
  test <- TRUE
  if(nchar(toString(linhas[1,1])) < 3){# não está no arquivo de testes
    test <- FALSE
    repot <- gsub(".csv", "", repo)
    #linkrepofiletest <- "/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/C/D-Programming-Language_dmd-NoTest.csv"
    linkrepofiletest <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/",repot, "-NoTest.csv", sep = "")
    repofiletest <- read.csv(linkrepofiletest)
    
    linhas <- repofiletest[repofiletest$Users == user,]
  }
  linha <- linhas[1,]
  situation <- type(linha)
  
  row <- data.frame(repo, user, test, situation, sep=",")
  
  return(row)
}

type <- function(rows){
  dateCreated <- strsplit(toString(rows$DateCreated), ",")
  dateUpdated <- strsplit(toString(rows$dateUpdated), ",")
  dateClosed <- strsplit(toString(rows$dateClosed), ",")
  dateMerged <- strsplit(toString(rows$dateMerged), ",")
  
  if(nchar(dateMerged[1]) > 3){
    return("Merged")
  }else if(nchar(dateClosed[1]) > 3){
    return("Closed")
  }else{
    return("Open")
  }
}

savesummary <- function(row, language){
  link <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Categoria/SummaryNovato",".csv", sep="")
  if(!file.exists(link)){
    rowsFst <- data.frame("Repository","User", "Test", "Situation", sep=",")
    write.table(rowsFst,file = link, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
  }
  write.table(row, file = link, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
}

language = "C"
open(language)
