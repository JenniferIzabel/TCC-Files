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
    
    #separar as categorias de usuario
    a(language)
    
    i <- i + 1
  }
}

a <- function(language){
  link <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Categoria/Users-",language,".csv", sep="")
  file <- read.csv(link)
  
  novatos <- file[file$Frequency == 1,]
  casual <- file[file$Frequency == 2,]
  regular <- file[file$Frequency > 2,]
  
  save(language, "Novato", novatos)
  save(language, "Casual", casual)
  save(language, "Regular", regular)
}

save <- function(language, cat, data){
  savelinknovato <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results2/",language,"/Categoria/",language,"-",cat,".csv", sep="")
  if(!file.exists(savelinknovato)){
    rowsFst <- data.frame("Repository","User", "Frequency", sep=",")
    write.table(rowsFst,file = savelinknovato, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
  }
  write.table(data, file = savelinknovato, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
}

language <- "CPlusPlus"
Files(language)
