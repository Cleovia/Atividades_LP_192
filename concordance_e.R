library(quanteda)
library(ggplot2)
library(rtweet)

source('funcoes_concordance.R')

# script para concordance de palavras
KWIC.2 <- function(my.corpus.l){
  # Exibe os arquivos no diretorio
  file.listing(names(my.corpus.l))
  
  # Pergunta qual arquivo ler
  file.id <- as.numeric(
    readline("Which file would you like to examine? Enter a number: \n")
  )
  
  # Pergunta quantas palavras a esquerda ou direita voce quer examinar
  context <- as.numeric(
    readline("How much context do you want to see? Enter a number: \n")
  )
  
  # Pergunta qual a palavra chave
  keyword <- tolower((readline("Enter a keyword: \n")))
  
  # Procura a palavra chave
  hits.v <- which(my.corpus.l[[file.id]] == keyword)
  
  # Loop para exibir as concordancias a esquerda
  if (length(hits.v) > 0) {
    result <- NULL
    
    for (h in 1:length(hits.v)) {
      start <- hits.v[h] - context
      
      if(start < 1) {
        start <- 1
      } 
      
      # Para exibir as ocorrencias a direita ?
      end <- hits.v[h] + context 
      
      # cat a linha com concordancia
      cat(my.corpus.l[[file.id]][start:end], "\n")
      
      # organização
      myrow <- cbind(hits.v[h],
                     # começo
                     paste(my.corpus.l[[file.id]][start:(hits.v[h] - 1)], collapse=" "),
                     
                     # hit (achado)
                     paste(my.corpus.l[[file.id]][hits.v[h]], collapse=" "),
                     
                     # fim
                     paste(my.corpus.l[[file.id]][(hits.v[h] + 1):end], collapse=" "))
      
      result <- rbind(result, myrow)
    } 
  }
  
  else {
    cat("YOUR KEYWORD WAS NOT FOUND\n")
  }
  
  colnames(result) <- c("position", "left", "keyword", "right")
  
  return(result)
}


# Script para extrair corpus de livro / concordance frase

gera_corpus_livro <- function(nome_livro) {
  # Cria uma string para localizar o diretorio do livro 
  diretorio_livro = paste("../phr/", nome_livro, ".txt", sep="")

  # Ler as linhas do livro no diretório indicado
  linhas_livro <- readLines(diretorio_livro)
  
  # Passo pra minusculo
  linhas_livro <- char_tolower(linhas_livro)
  
  # Crio o corpus das linhas do livro
  corpus_livro <- corpus(linhas_livro)
  
  return(corpus_livro)
}

gera_lista_freq_palavras_de_livro <- function(nome_livro) {
  diretorio_livro = paste("../phr/", nome_livro, ".txt", sep="")
  
  linhas_livro <- readLines(diretorio_livro)
  
  palavras_livro <- tokens(linhas_livro, remove_punct = TRUE) %>% as.character()

  dfm_livro <- dfm(palavras_livro)
  
  lista_frequencias_de_palavras <- textstat_frequency(dfm_livro, n = 50)
  
  return(lista_frequencias_de_palavras)
}

corpus_jean = gera_corpus_livro("jean")

a_pdagogia_jean = kwic(corpus_jean, pattern = phrase("a pedagogia"))

# --------------------------------------------------------------
# Gráfico de ocorrências de palavras chave

theme_set(theme_minimal())
resul %>% 
  ggplot(aes(x = rank, y = frequency)) +
  geom_point() +
  labs(x = "Frequency rank", y = "Term frequency")

# --------------------------------------------------------------
# Concordance de palavra chave
input.dir <- "phr"
my.files <- dir(input.dir, "\\.txt$")
my.corpus <- corpus.list(my.files, input.dir)
my.results <- KWIC.2(my.corpus)


View(my.results)

