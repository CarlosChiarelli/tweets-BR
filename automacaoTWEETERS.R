### AUTOMACAO PUXAR DADOS TWITTER ###

#######################

puxaTWEET <- function(PREPOSICAO){
  # essa funcao puxa um tweeter do suja ou algum outro periodo
  
  # biblioteca para puxar os dados
  biblio <- c(
    "rtweet",
    "magrittr"
  )
  
  # instalando e importando pacotes
  for(pacote in biblio){
    
    # checa se o pacote está instalado
    if( !(pacote %in% rownames(installed.packages()))  )
      install.packages(pacote)
    
    library(pacote, character.only = TRUE)
  }
  
  # acesso API tweeter
  create_token(
    app = "RPrepositionTestApp",
    consumer_key <- "WE33fxF5SDQeOaCO1wbqpIzog",
    consumer_secret <-"MZ5Gukb4ZD8fO76fQ02xv0iOrejtx4CEzK2irTj1qQE2EOymtB",
    access_token <- "1100203825527287809-QDgVkxQ9nCZL6bG0UQQqtT3AHE1007",
    access_secret <- "ihroRFd0Nm9PDD0JMOOdixmw7jBmxj83xI0s2ts9FBnIR"
  )
  
  # puxar tweets BR
  bz <- lookup_coords("brazil")
  
  # busca dos tweets
  tweets <- search_tweets(
    q = PREPOSICAO,
    n = 200,
    type = "recent",
    lang = "pt",
    include_rts = FALSE,
    geocode = bz, 
    max_id = NULL,
    parse = TRUE,
    token = NULL, 
    retryonratelimit = FALSE,
    verbose = TRUE
  )
  
  # retorna tabela de tweeters suja
  return(tweets)
}

#######################

limpaTWEET <- function(tabelaTWEET){
  # essa funcao pega uma tabela de twweter suja e limpa 
  
  # bibliotecas
  biblio <- c(
    "tidyverse",
    "devtools",
    "magrittr"
  )
  
  # instalando e importando pacotes
  for(pacote in biblio){
    
    # checa se o pacote está instalado
    if( !(pacote %in% rownames(installed.packages()))  )
      install.packages(pacote)
    
    library(pacote, character.only = TRUE)
  }
  
  # selecionando nomes colunas q sao listas
  nomeslistas = tabelaTWEET %>% 
    select_if(is.list) %>% 
    names()
  
  # removendo listas
  tabelasemlistas = tabelatweet %>% 
    select(-nomeslistas)
  
  # retornando tabela limpa
  return(tabelasemlistas)
}

#######################

criaTABELA_principal <- function(PREPOSICAO){
  # essa funcao cria a tabela principal que irá guardar TODOS os tweets diários
  # Ela só deve ser executada uma vez pois cria uma tabela que receberá outros tweeters diários
  
  # puxa o tweet do dia
  tabelaTWEET_dia <- puxaTWEET(PREPOSICAO)
  
  # limpa o tweet
  tabelaTWEET_dia <- tabelaTWEET_dia %>% 
    limpaTWEET()
    
  # selecionar apenas uma linha (manter tipos das colunas quando salvar tabela no PC)
  tabelaTWEET_dia <- tabelaTWEET_dia %>% 
    slice(1)
  
  # salva tabela no computador (PC)
  tabelaTWEET_dia %>% 
    write_csv("tabelaTWEET_principal.csv")
}

#######################

atualizaTWEETS_principal <- function(PREPOSICAO){
  # essa funcao atualiza a nossa tabela principal de TWEETS com as novas buscas (tweets diarios)

  # biblioteca para puxar os dados
  biblio <- c(
    "tidyverse",
    "devtools",
    "magrittr"
  )
    
  # instalando e importando pacotes
  for(pacote in biblio){
    
    # checa se o pacote está instalado
    if( !(pacote %in% rownames(installed.packages()))  )
      install.packages(pacote)
    
    library(pacote, character.only = TRUE)
  }
    
  # primeiramente checar se existe tabelaTWEET_principal
  arquivos_PASTA <- list.files()
  
  # se existe tabelaPRINCIPAL executa o codigo, caso contrario nao
  existeTABELA <- arquivos_PASTA %>% 
    str_detect("tabelaTWEET_principal.csv") %>% 
    sum()
  
  if(existeTABELA) criaTABELA_principal(PREPOSICAO)
  
  # agora vamos puxar os novos tweeters
  tabelaTWEET_novo <- puxaTWEET(PREPOSICAO)
  
  # limpa tweet novo
  tabelaTWEET_novo <- tabelaTWEET_novo %>% 
    limpaTWEET()
  
  # para manter compatibilidades das colunas vamos salvar o TWEET NOVO, lê-lo novamente
  tabelaTWEET_novo %>% 
    write_csv("tabelaTEMPORARIA.csv")
  
  tabelaTWEET_novo <- read_csv("tabelaTEMPORARIA.csv")
  
  # excluindo tabela temporaria
  file.remove("tabelaTEMPORARIA.csv")
  
  # lê a tabela de TWEETS principal para juntir os TWEETS NOVOS a ela
  tabelaTWEET_principal <- read_csv("tabelaTWEET_principal.csv")
  
  # fazendo a uniao e REMOVENDO LINHAS REPETIDAS
  tabelaTWEET_principal <- tabelaTWEET_principal %>% 
    bind_rows(
      tabelaTWEET_novo
    ) %>% 
    distinct()
  
  # salvando tabela ATUALIZADA
  tabelaTWEET_principal %>% 
    write_csv("tabelaTWEET_principal.csv")
    
  # limpar memoria
  rm(list = ls())
}