library(jsonlite)
library(httpuv)
library(httr)

setwd("/home/jennifer/Documents/UTFPR/TCC")

begin <- function(){
  language <- "C"
  CRepositories <- read.csv("/home/jennifer/Documents/UTFPR/TCC/Scripts/Repositories/CRepositories.csv")
  parser(CRepositories, language)
  
  # language <- "CPlusPlus"
  # CPlusPlusRepositories <- read.csv("/home/jennifer/Documents/UTFPR/TCC/Scripts/Repositories/CPlusPlusRepositories.csv")
  # parser(CPlusPlusRepositories, language)
  
  # language <- "CSharp"
  # CSharpRepositories <- read.csv("/home/jennifer/Documents/UTFPR/TCC/Scripts/Repositories/CSharpRepositories.csv")
  # parser(CSharpRepositories, language)
  
  # language <- "Java"
  # JavaRepositories <- read.csv("/home/jennifer/Documents/UTFPR/TCC/Scripts/Repositories/JavaRepositories.csv")
  # parser(JavaRepositories, language)
  
  # language <- "Python"
  # PythonRepositories <- read.csv("/home/jennifer/Documents/UTFPR/TCC/Scripts/Repositories/PythonRepositories.csv")
  # parser(PythonRepositories, language)
  
  # language <- "PHP"
  # PHPRepositories <- read.csv("/home/jennifer/Documents/UTFPR/TCC/Scripts/Repositories/PHPRepositories.csv")
  # parser(PHPRepositories, language)
  
  # language <- "Ruby"
  # RubyRepositories <- read.csv("/home/jennifer/Documents/UTFPR/TCC/Scripts/Repositories/RubyRepositories.csv")
  # parser(RubyRepositories, language)
  # 
}

oauth <- function(index){
  # Can be github, linkedin etc depending on application
  oauth_endpoints("github")
  
  # Change based on what you 
  myapp1 <- oauth_app(appname = "TCCGit", key = "be9b401df3e661a6e20a", secret = "1f4905dcd6e03536dd845d5d96096e6278ab7c70")
  myapp2 <- oauth_app("Empirica","ba6b20a34710fe851e46", secret="16c202c72eb4c763d78530f17008af0cebe08c29") #token Geeo
  myapp3 <- oauth_app("Tcc-Jennifer","c59dccc35596ceca955a", secret="1e5ae13c1575bccc351855b7708fee2bfb451e1f") #token Augusto
 
  if(index == 1){
    # Get OAuth credentials
    github_token <- oauth2.0_token(oauth_endpoints("github"), myapp1)
  }else if(index == 2){
    # Get OAuth credentials
    github_token <- oauth2.0_token(oauth_endpoints("github"), myapp2)
  }else{
    # Get OAuth credentials
    github_token <- oauth2.0_token(oauth_endpoints("github"), myapp3)
  }
  
  # Use API
  gtoken <- config(token = github_token)
  
  return(gtoken)
}

parser <- function(repo, language){# repo = lists of biggest repositories 
  ### TEST
  #repo <- RubyRepositories
  #language <- "Ruby"
  ### END TEST
  maxPerLanguage <- 10
  
  #link where are all the open pulls from each repo
  links <- paste("https://api.github.com/repos/",repo$repository,"/pulls", sep= "")
  
  repoName <- paste("", repo$repository, sep = "")
  
 print("Open")
 counter <- 3
 while(counter <= 3){ #open pulls from each repository
   repo <- gsub("/", "_", repoName[counter])
   re(links[counter], language, repo, "open")
   counter <- counter + 1
 }


  print("Closed")
  counter <- 3
  while(counter <= 3){ #closed pulls from each repository
    repo <- gsub("/", "_", repoName[counter])
    re(links[counter], language, repo, "closed")
    counter <- counter + 1
  }

}

checkTestName <- function(n){
  ### TEST
  #n <- "test/functional/ui/inccommand_spec.lua"
  #n <- "include/mrbconf.h"
  ## END TEST
  upperName <- toupper(n)
  b <- grepl("TEST", upperName, fixed=TRUE)
  return(b)
}

re <- function(pullLink, language, repo, states, linkSummary){
  ### TEST
  #pullLink <- "https://api.github.com/repos/linuxmint/Cinnamon/pulls"
  #language <- "C"
  #repo <- "linuxmint_Cinnamon"
  #states <- "closed"
  #states <- "open"
  ### END TEST
  linkSummary <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results/",language,"/summary.csv", sep="")
  
  if(!file.exists(linkSummary)){
    rowsFst <- data.frame("Repository", "Estado", "Page", "Total pulls", "Pulls wiht tests", "pulls without testes") 
    write.table(rowsFst,file = linkSummary, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
  } 
  print(repo)
  
  noTestCounter <- 0 #pulls without tests
  testCounter <- 0 #pulls with tests
  totalCounter <- 0 #total pulls
  
  indexT <- 1 # flag to iterate between 3 diferent tokens
  
  
  flag <- 1 # flag to iterate pages
  
  while(flag != 0){
    pages <- flag #number os pages
    tryCatch({
      ###### make iterate between 3 diferent tokens
      gtoken <- oauth(indexT)
      indexT <- indexT + 1
      if(indexT > 3){
        indexT <- 1
      }
      ###### end
      
      if(states == "closed"){
        pullLinkClosed <- paste(pullLink, "?q=is%3Apr+is%3Aclosed", sep = "")
        req <- GET(pullLinkClosed, query = list(state = states, per_page = 100, page = flag))
        
      }else{
        req <- GET(pullLink, query = list(state = states, per_page = 100, page = flag))
      }
      print(c("page: ", pages))
      
      headers <- req$all_headers
      aux <- 1
      while(aux <= length(headers)){# check if it has next page
        has_next <- grepl(headers[aux], "next")
        if(has_next){
          break
        }
        aux <- aux + 1
      }
      
        
      flag <- flag + 1
      
      #json
      json_file <- content(req, as="parsed", encoding="UTF-8")
      json = jsonlite::fromJSON(jsonlite::toJSON(json_file))
      
      #pull number
      pullNumber <- json$number
      
      pulltest <- TRUE
      if(length(pullNumber) < 1){# if there isn't a pull
        print("no pulls")
        break
      }
      else{
        totalCounter <- totalCounter + length(pullNumber)
        
        #pull state (open or closed)
        pullState <- json$state
        
        #dates (created, updated,closed and merged)
        dateCreated <- json$created_at
        dateUpdated <- json$updated_at
        dateClosed <- json$closed_at
        dateMerged <- json$merged_at
        
        #type = 0 -> open   -> has created, 
        #type = 1 -> closed -> has created, closed
        #type = 2 -> merged -> has created, closed, merged
        
        # checking for possible errors
        tryCatch({
          dateClosed[1]
        },
        error = function(e){
          dateClosed = NULL
        })
        
        tryCatch({
          dateMerged[1]
        },
        error = function(e){
          dateMerged = NULL
        })
        
        
        #pulls  users
        usersPull <- NULL
        pullUsers <- paste(pullLink, "/", pullNumber, sep = "")
        u <- 1
        while(u <= length(pullUsers)){
          req <- GET(pullUsers[u], gtoken) 
          json_users <- content(req, as="parsed", encoding="UTF-8")
          jsonUsers = jsonlite::fromJSON(jsonlite::toJSON(json_users))
          usersPull <- c(usersPull, jsonUsers$user$login)
          u <- u + 1
        }
        
        #pulls link files
        pullLinkFiles <- paste(pullLink, "/", pullNumber, "/files", sep = "")
        
        
        link <- paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/results/",language,"/",repo,".csv", sep= "")
        #First Line
        if(!file.exists(link)){
          rowsFst <- data.frame("Repository", "Pull state", "Pull number", "sha", "Filename", "File status", "Additions", "Deletions", "Users", "DateCreated", "DateUpdated", "dateClosed", "dateMerged", sep=",") 
          write.table(rowsFst,file = link, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
        }
        pullCount <- 1 
        while(pullCount <= length(pullNumber)){ #for each pull 
          
          req <- GET(pullLinkFiles[pullCount], gtoken) 
          json_file_files <- content(req, as="parsed", encoding="UTF-8")
          jsonFiles = jsonlite::fromJSON(jsonlite::toJSON(json_file_files))
          
          #sha
          filesSha <- jsonFiles$sha
          
          #additions
          fileAdd <- jsonFiles$additions
          
          #exclusions
          fileDel <- jsonFiles$deletions
          
          
          #check test and write
          filesNames <- jsonFiles$filename
          
          #check test and write
          filesStatus <- jsonFiles$status
          
          print(pullNumber[pullCount])
          
          filesCount <- 1
          while(filesCount <= length(filesNames)){ #for each file in a pull
            hasTest <- checkTestName(filesNames[filesCount])
            if(hasTest){
              pulltest <- TRUE
              
              #check if sth is null
              if(is.null(pullState[pullCount])){pullState[pullCount] <- 0}
              if(is.null(pullNumber[pullCount])){pullNumber[pullCount] <- 0}
              if(is.null(filesSha[filesCount])){filesSha[filesCount] <- 0}
              if(is.null(filesNames[filesCount])){filesNames[filesCount] <- 0}
              if(is.null(filesStatus[filesCount])){filesStatus[filesCount] <- 0}
              if(is.null(fileAdd[filesCount])){fileAdd[filesCount] <- 0}
              if(is.null(fileDel[filesCount])){fileDel[filesCount] <- 0}
              if(is.null(usersPull[pullCount])){usersPull[pullCount]  <- 0}

              #write csv
              brepo <- gsub("_", "/", repo)
              rows <- data.frame(brepo, pullState[pullCount], pullNumber[pullCount], filesSha[filesCount], filesNames[filesCount], filesStatus[filesCount], fileAdd[filesCount], fileDel[filesCount], usersPull[pullCount] , sep=",")
              write.table(rows,file = link, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
            }else{
              pulltest <- FALSE
            }
            filesCount <- filesCount + 1
          } 
          pullCount <- pullCount + 1
          if(pulltest){
            testCounter <- testCounter + 1
          }else{
            noTestCounter <- noTestCounter + 1
          }
        }
      }
      
    },
    pages <- flag,
    flag = 0
    )
    rows <- data.frame(repo, states, pages, totalCounter, testCounter, noTestCounter, sep=",")
    write.table(rows,file = linkSummary, append = T, col.names = F, row.names = F, sep = ",",  quote = F)
    print(rows)
    }
}

begin()