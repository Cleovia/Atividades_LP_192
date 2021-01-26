library(tidytext)
library(dplyr)
library(readr)
library(tidyverse)
library(tm)
library(tidyr)
library(scales)

stopwords <- data.frame(read_csv("../stopwords_cleovia.csv", col_names = FALSE),stringsAsFactors = FALSE)
colnames(stopwords)<-"word"

extrai_wordfrequency_from_livro <- function(nome_livro) {
  
  # Cria uma string para localizar o diretorio do livro 
  diretorio_livro = paste("../phr/", nome_livro, ".txt", sep="")
  
  # Ler as linhas do livro no diretório indicado
  linhas_livro <- readLines(diretorio_livro)
  
  # dataframe com o as linhas
  livro_dataframe <- data.frame(text = linhas_livro, stringsAsFactors = F)
  
  # Cria uma lista de palavras e remove as palavras da stopwords
  livro_wordlist <- livro_dataframe %>% unnest_tokens(word, text) 
  
  # Armazeno a quantidade de palavras antes de remover as stopwords
  quantidade_palavras = count(livro_wordlist)["n"]
  
  # Removo as stopwords
  livro_wordlist_clean = livro_wordlist %>% anti_join(stopwords, by="word")
  
  # Conta e agrupa as palavras por frequencia sem as stopwords
  livro_wordfrequency <- livro_wordlist_clean %>% count(word, sort = TRUE)
  
  # Retorno um conjunto contendo a quantidade de palavras e frequencia
  return(list(
    "quantidade" = quantidade_palavras$n[[1]],
    "wordfrequency" = livro_wordfrequency,
    "wordlist" = livro_wordlist
    ))
}

mariane <- extrai_wordfrequency_from_livro("marianne")
wf_marianne <- mariane$wordfrequency
qtd_mariane <- mariane$quantidade
wl_mariane <- mariane$wordlist
wf_marianne$title <- "MF"
View(mariane)

olinda <- extrai_wordfrequency_from_livro("olinda")
wf_olinda <- olinda$wordfrequency
qtd_olinda <- olinda$quantidade
wl_olinda <- olinda$wordlist
wf_olinda$title <- "OG"


olvera <- extrai_wordfrequency_from_livro("olvera")
wf_olvera <- olvera$wordfrequency
qtd_olvera <- olvera$quantidade
wl_olvera <- olvera$wordlist
wf_olvera$title <- "AO"


jean <- extrai_wordfrequency_from_livro("jean")
wf_jean <- jean$wordfrequency
qtd_jean <- jean$quantidade
wl_jean <- jean$wordlist
wf_jean$title <- "JL"

saberes <- extrai_wordfrequency_from_livro("saberes")
wf_saberes <- saberes$wordfrequency
qtd_saberes <- saberes$quantidade
wl_saberes <- saberes$wordlist
wf_saberes$title <- "SS"

melo <- extrai_wordfrequency_from_livro("melo")
wf_melo <- melo$wordfrequency
qtd_melo <- melo$quantidade
wl_melo <- melo$wordlist
wf_melo$title <- "FM"

livros.bruto <- rbind(wf_marianne,
                      wf_olinda,
                      wf_olvera,
                      wf_jean,
                      wf_melo,
                      wf_saberes)

livros.total <- livros.bruto %>%
  group_by(title) %>%
  summarize(total = sum(n))

livros.palavras <- left_join(livros.bruto, livros.total)

ggplot(livros.palavras, aes(n/total, fill = title)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~title, ncol = 2, scales = "free_y")

livros.palavras.rank <- livros.palavras %>%
  group_by(title) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

livros.palavras.rank  %>%
  ggplot(aes(rank, `term frequency`, color = title)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) +
  scale_x_log10() +
  scale_y_log10()

rank_subset <- livros.palavras.rank %>%
  filter(rank < 500,
         rank > 15)
lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

livros.palavras.rank %>%
  ggplot(aes(rank, `term frequency`, color = title)) +
  geom_abline(intercept = -0.966, slope = -1.0004, color = "black", linetype = 1) +
  geom_line(size = 1.1, alpha = 0.9, show.legend = TRUE) +
  scale_x_log10() +
  scale_y_log10()

livros.palavras.imp  <- livros.palavras  %>%
  bind_tf_idf(word, title, n)

livros.palavras.imp  %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(title) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = title)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~title, ncol = 2, scales = "free") +
  coord_flip()

# ---------------------------- WORD CLOUD ----------------------------

library(wordcloud)
library(RColorBrewer)
library(tm)

extrai_dados_para_wordcloud_de_livro <- function(nome_livro) {
  # Cria uma string para localizar o diretorio do livro 
  diretorio_livro = paste("../phr/", nome_livro, ".txt", sep="")
  
  linhas_livro <- readLines(diretorio_livro)
  linhas_livro <- gsub("—", "", linhas_livro, fixed = TRUE)
  linhas_livro <- gsub("\"", "", linhas_livro, fixed = TRUE)
  linhas_livro <- gsub("•", "", linhas_livro, fixed = TRUE)
  
  
  corpus_livro <- Corpus(VectorSource(linhas_livro))
  
  # transformo tudo em minusculo lowercase
  corpus_livro <- tm_map(corpus_livro, content_transformer(tolower))
  
  # removo numeros
  corpus_livro <- tm_map(corpus_livro, removeNumbers)
  
  # removo as stopwords carregadas anteriormente de stopwords_cleovia.csv
  corpus_livro <- tm_map(
    corpus_livro,
    removeWords,
    stopwords$word
  )
  
  # removo espacos vazios
  corpus_livro <- tm_map(corpus_livro, stripWhitespace)
  
  # removo pontuacoes
  corpus_livro <- tm_map(corpus_livro, removePunctuation)
  
  # crio uma matriz de termos para esse corpus
  dtm_livro <- TermDocumentMatrix(corpus_livro)
  
  # passo para uma matrix comum
  matriz_livro <- as.matrix(dtm_livro)
  
  # ordeno a matriz de acordo com a soma de linhas ??? 
  livro.v <- sort(rowSums(matriz_livro), decreasing=TRUE)
  
  # crio uma data frame com as palavras e a frequencia
  livro.df <- data.frame(word = names(livro.v), freq = livro.v)
  
  return(livro.df)
}

cria_word_cloud_de_livro <- function(nome_livro) {
  dataframe_livro = extrai_dados_para_wordcloud_de_livro(nome_livro)
  
  set.seed(1234)
  wordcloud(words = dataframe_livro$word, freq = dataframe_livro$freq, min.freq = 15,
            max.words=100, random.order=FALSE, rot.per=0.35,
            colors=brewer.pal(8, "Dark2"))
}

cria_word_cloud_de_livro("saberes")
cria_word_cloud_de_livro("marianne")
cria_word_cloud_de_livro("olvera")
cria_word_cloud_de_livro("melo")
cria_word_cloud_de_livro("olinda") 
cria_word_cloud_de_livro("jean")

warnings()





