Files <- function(language){
  link <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Junto", sep = "")
  files <- list.files(path = link, pattern = NULL, all.files = FALSE,
                      full.names = FALSE, recursive = FALSE,
                      ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
  # iterate through files
  i <- 1
  while(i <= length(files)){# for each file
    linkjunto <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Junto/",files[i], sep="")
    
    repo <- files[i]
    print(repo)
    
    #agrupar pelo numero do pull request
    groupedlink <- Group(language, repo, linkjunto)
    
    #fazer a frequencia de cada usuario
    
    UserFreq(repo, groupedlink, language)
    
    i <- i + 1
  }
}

Group <- function(language, repo, linkjunto){
  repojunto <- read.csv(linkjunto)
  numbers <- unique(repojunto$Pull.number)
  
  groupedlink <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Categoria/GroupedPulls-",repo, sep="")
  if(!file.exists(groupedlink)){
    rowsFst <- data.frame("Repository", "Pull number","Users", "Test", "Change", "Type", sep=",")
    write.table(rowsFst,file = groupedlink, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
  }
  
  i <- 1
  while(i <= length(numbers)){
    pullnumber <-  numbers[i]
    rows <- repojunto[repojunto[, "Pull.number"] == pullnumber,,drop=FALSE] # todas as linhas de um pull
    users <- toString(unique(rows$Users))
    test <- teste(language, repo, pullnumber)
    changes <- gsub(","," AND",toString(unique(rows$File.status)))
    linha <- rows[1,]
    type <- type(linha)
    
    data <- data.frame(repo, pullnumber, users, test, changes, type)
    write.table(data, file = groupedlink, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
    
    print(pullnumber)
    i <- i+1
  }
  return(groupedlink)
}

teste <- function(language, repo, pullnumber){
  linkrepofile <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/",repo, sep = "")
  repofile <- read.csv(linkrepofile)
  l <- repofile[repofile$Pull.number == pullnumber,]
  
  test <- TRUE
  if(length(l[1]) < 1){# não está no arquivo de testes
    test <- FALSE
  }
  return(test)
}

type <- function(linha){
  dateCreated <-toString(linha$DateCreated)
  dateUpdated <- toString(linha$dateUpdated)
  dateClosed <- toString(linha$dateClosed)
  dateMerged <- toString(linha$dateMerged)
  
  if(nchar(dateMerged[1]) > 3){
    return("Merged")
  }else if(nchar(dateClosed[1]) >3){
    return("Closed")
  }else{
    return("Open")
  }
}

UserFreq <- function(name, link, language){
  savelink <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Categoria/Users-",language,".csv", sep="") 
  
  if(!file.exists(savelink)){
    rowsFst <- data.frame("Repository","User", "Frequency", sep=",")
    write.table(rowsFst,file = savelink, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
  }
  
  file <- read.csv(link)
  #user <- levels(file$Users) #os nomes da cada usuário presente
  users <- as.data.frame(table(unlist(file$Users)))
  users <- c(name, users)
  
  write.table(users, file = savelink, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
}

language <- "CPlusPlus"
Files(language)
