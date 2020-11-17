library(gutenbergr)
library(ggplot2)
library(dplyr)
library(tidytext)
gutemberger
# Livro da plataforma

livro_dom_casmurro <- gutenberg_download (55752 )

# livro com caracteres válidos  M.1 <- M.0 %>% 


livro_limpo <- livro_dom_casmurro %>% mutate(text=iconv(text, from = "latin1", to = "UTF-8"))
View(livro_limpo)

# livro transformado em vetor de linhas      M.1 <- M.1$text

livro_limpo_vetor <- livro_limpo $ text

View(livro_limpo)
# número da linha onde começa o capítulo 01 which(M.1== "CAPITULO I")

linha_inicial <- which(livro_limpo_vetor == "Do titulo.")
View(linha_inicial)

## número da linha onde termina o livro /// which(M.1 == "FIM")

linha_final <- which(livro_limpo_vetor == "FIM")
View(linha_final)

#conjunto de linhas do início do capítulo 01 até o fim da obra /// M.1.linhas <- M.1[inicio:fim]

bloco_de_linhas<- livro_limpo_vetor[linha_inicial:linha_final]
View(bloco_de_linhas)

# livro transformado em uma única linha
livro_colapsado <- paste(bloco_de_linhas, collapse=" ")
View(livro_colapsado)

#livro transformado em uma única linha com todas as letras em minúsculo
livro_colapsado_minusculo <- tolower(livro_colapsado)
View(livro_colapsado_minusculo)

# livro transformado novamente em vetor de palavras/ 

livro_colapsado_vetor <- strsplit (livro_colapsado_minusculo, "\\W")


# Transformo para um vetor unico
livro_lista_palavras <- unlist (livro_colapsado_minusculo)


# identificando os espaços vazios not.blanks 
livro_lista_palavras<- which(lista_palavras != "")
View(livro_lista_palavras)

 # limpando os espaços vazios /// not.blanks  <- que ( M1.lv ! = " " )
not.blanks <- which(livro_lista_palavras!="")

# cria meu vetor de palavras sem espaços  //// M1.l.v <- M1.l.v[not.blanks] 

lista_palavras_limpas <- livro_lista_palavras[not.blanks]
View(lista_palavras_limpas)

# verificar quais palavras estão nesta posição

mypositions.v<c(300,151,621)
lista_palavras_limpas[mypositions.v]


# quais palavras ocorrem da posição 1 até 42
lista_palavras_limpas[ 1 : 30 ]

# ocorrência da palavra bentinho
bentinho.f <- which(lista_so_palavras=="bentinho")
View(lista_so_palavras=="bentinho")

length(bentinho.f)
bentinho.f


#Gráfico com a palavra bentinho
plot(bentinho.f)
