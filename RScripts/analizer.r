
setwd("/home/jennifer/Documents/UTFPR/TCC")


initTestsPercentage <- function(){
  percentageLink <- "/home/jennifer/Documents/UTFPR/TCC/Scripts/results/Testspercentage.csv"
  if(!file.exists(percentageLink)){
    rowsFst <- data.frame("Language", "Repository", "Tests %", "No tests %") 
    write.table(rowsFst,file = percentageLink, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
  }
  language <- "C"
  testsPercentage(percentageLink, language)
  
  language <- "CPlusPlus"
  testsPercentage(percentageLink, language)
  
  language <- "CSharp"
  testsPercentage(percentageLink, language)

  language <- "Java"
  testsPercentage(percentageLink, language)

  language <- "Python"
  testsPercentage(percentageLink, language)

  language <- "PHP"
  testsPercentage(percentageLink, language)

  language <- "Ruby"
  testsPercentage(percentageLink, language)

}
initChanges <- function(){
  link <- "/home/jennifer/Documents/UTFPR/TCC/Scripts/results/ChangesPercentage.csv"
  if(!file.exists(link)){
    rowsFst <- data.frame("language", "name", "added", "modified", "removed", "renamed") 
    write.table(rowsFst,file = link, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
  }
  language <- "C"
  openFiles(link, language)

  language <- "CPlusPlus"
  openFiles(link, language)

  language <- "CSharp"
  openFiles(link, language)

  language <- "Java"
  openFiles(link, language)

  language <- "Python"
  openFiles(link, language)

  language <- "PHP"
  openFiles(link, language)

  language <- "Ruby"
  openFiles(link, language)
  
}

testsPercentage <- function(percentageLink, language){
    link <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results/",language,"/summary.csv", sep = "")
    file <- read.csv(link)
    
    repos <- unique(file$Repository)
    
    i <- 1
    total <- 0
    sumtotal <- 0
    sumptest <- 0
    sumpnotest <- 0
    while(i < length(repos)){ # iterate through each distinct repo in summary
      rows <- file[file[, "Repository"] == repos[i],,drop=FALSE] # get all row of some repo
      reponame <- unique(rows[1])
      
      total <- colSums(rows[4] , na.rm = FALSE, dims = 1) # sum(total.pulls)
      tests <- colSums(rows[5] , na.rm = FALSE, dims = 1) # sum(tests)
      notests <- colSums(rows[6] , na.rm = FALSE, dims = 1) # sum(without tests)
      
      ptest <- tests * 100 /total 
      pnotest <- notests * 100 /total
      ptest <- round(ptest, 2)
      pnotest <- round(pnotest, 2)
      
      sumtotal <- sumtotal + total
      sumptest <- sumptest + tests
      sumpnotest <- sumpnotest + notests
      
      rows <- data.frame(language, reponame, ptest, pnotest, sep=",")
      write.table(rows,file = percentageLink, append = T, col.names = F, row.names = F, sep = ",",  quote = F)

     i <- i + 1
    }
    
    
    avgtest <- sumptest/sumtotal * 100
    avgnotest <- sumpnotest/sumtotal * 100
    avgtest <- round(avgtest, 2)
    avgnotest <- round(avgnotest, 2)
    langavg <- paste(language, " average", sep = "")
    
    rows <- data.frame(language, langavg, avgtest, avgnotest, sep=",")
    write.table(rows,file = percentageLink, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
    
}

openFiles <- function(link, language){
  # get all files names in some directory
  linko <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results/",language, sep = "")
  files <- list.files(path = linko, pattern = NULL, all.files = FALSE,
                      full.names = FALSE, recursive = FALSE,
                      ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
  # iterate through files
  i <- 1
  while(i <= length(files)){
    linkFile <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results/",language,"/",files[i], sep="")
    
    name <- files[i]
    # for each file
    if(name != "summary.csv"){
      Changes(link, language, name, linkFile)
    }
    
    
    i <- i + 1
  }
}

Changes <- function(link, language, name, linkFile){
  file <- read.csv(linkFile)

  added <- 0
  modified <- 0
  removed <- 0
  renamed <- 0
  
  i <- 1
  while( i <= length(file$File.status)){
    print(c(i, name))
    if(file[i, 6] == "added"){added <- added + 1}
    if(file[i, 6] == "modified"){modified <- modified + 1}
    if(file[i, 6] == "removed"){removed <- removed + 1}
    if(file[i, 6] == "renamed"){renamed <- renamed + 1}
    
    i <- i + 1
  }
  rows <- data.frame(language, name, added, modified, removed, renamed, sep=",")
  write.table(rows,file = link, append = T, col.names = F, row.names = F, sep = ",",  quote = F)

}


initTestsPercentage()
initChanges()


