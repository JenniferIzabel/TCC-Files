library(gmailr)

principal <- function(language){
  nome_arquivo = paste("/home/jennifer/Documents/UTFPR/TCC/Scripts/Questionario/emails-UsersMergedWithTests-Novato-",language,".csv", sep = "")
  arq = read.csv(nome_arquivo)
  
  print(language)
  
  users <- arq[2]
  emails <- arq[6]
  repos <- arq[1]
  pulls <- arq[3]
  
  
  
  for(i in 1:nrow(arq)){
    msg <- monta_mensagem(users[i,1], repos[i,1], pulls[i,1])
    send_email(emails[i,1], msg)
  }
    

  
}

send_email <- function(email, message){
  try({
    mime() %>%
      to(email) %>%
      from("jennifersouza@alunos.utfpr.edu.br") %>%
      subject("A 5 min survey about GitHub") %>%
      text_body(message) -> text_msg
    
    strwrap(as.character(text_msg))
    send_message(text_msg)
  }, print("email invalido"))
  
}


monta_mensagem <- function(fulano, repo, pull){
  f <- paste("-",fulano)
  print(f)
  
  mensagem <- paste("Dear GitHub user ",fulano,
                ",

My name is Jennifer Souza, and I'm a researcher from Federal University of Technology - Parana, Brazil. Along with Igor Wiese, we are studying how open source collaborators write tests in their first contribution.

We found the Pull Request https://github.com/",repo,"/pull", pull, 
                ",
that you have authored/committed.

While mining this Pull Request, we found that you may have written tests.

If we got it correctly, you perfectly fit in our research and it would be of great help if you could spare some minutes(~5min) to answer the following questionnaire:
https://forms.gle/UBZaUKF8KNbZwmdx9

Your participation in answering the survey is greatly appreciated, but in no way required. The information that is collected will be used to both develop tools and guideline resources for open
collaborators/integrators and write up scientific papers. All results will be presented as summaries.

Your individual responses or any personal information about you will not be identifiable in any published reports of the data. Your IP address is not collected. There will be no follow-up.

If you provide your name in the last question of the survey, your identity will be made available only to us. However, you will not be identified in any reports of the data, nor will your identity be made available to anyone outside the research team.

Thank you so much for your time, and please do not hesitate in contacting me if you have any question.

Thanks in advance,


Jennifer Souza", 
                sep = "")
  return(mensagem)
}


# language <- "CPlusPlus"
# principal(language)
# language <- "CSharp"
# principal(language)
# language <- "Java"
# principal(language)
# language <- "PHP"
# principal(language)
language <- "Python"
principal(language)
language <- "Ruby"
principal(language)
