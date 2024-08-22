#'##############################################################################
#'       CEN5815 - Análise de Dados Agronômicos e Ambientais                   #
#'       Prof. Deoclecio Jardim Amorim                                         #
#'       Aula 01: Introdução à linguagem de programação R                      #
#'##############################################################################
# Limpar área de trabalho ---------------------------------------------------------------------
rm(list = ls())      # Remove todos os objetos do ambiente de trabalho
gc(reset = TRUE)     # Libera memória
graphics.off()       # Fecha todas as janelas gráficas abertas
#'
#'Conteúdo
#'
#'-Instalação do R e RStudio;
#'-Operações básicas; Instalação de pacotes;
#'-Importação de dados;
#'-Manipulação de dados e estatística descritiva;
#'-Projetos.
#'
#'
# Principais operações aritméticas e lógicas com o R ---------------------------------
#'
#' Operador | Descrição
#' ---------|----------
#' +        | adição
#' -        | subtração
#' *        | multiplicação
#' /        | divisão
#' :        | sequência
#' ^        | exponecial
#' <        | menor que
#' <=       | menor ou igual
#' >        | maior que
#' >=       | maior ou igual
#' ==       | igual
#' !=       | diferente
#' !        | não
#' \|       | ou
#' &        | e
#' %in%     | pertence
#'
## Exemplos
#'
#Soma
2 + 2

#Subrtração
2 - 2

#Divisão
2 / 2

#Operador lógico
#Maior
3 > 4

#Menor
3 < 4

#igualdade
3 == 4

# Funções matemáticas ---------------------------------------------------------------------------

#Número pi= 3,14...
pi

#função sin()
sin(pi)

#função cos()
cos(pi)

#função log() e exp()

log(0)

log(1)

exp(1)

log(exp(1))

#log na base 10
log10(100)

# Preciso de ajuda --------------------------------------------------------------------------

?log()


# Objetos -------------------------------------------------------------------------------------

#'Tipos comuns
#'
#
#'Vetores
a <- c(2, 4, 6) #esse é um vetor 
b <- c(3, 5, 7)

#Posição do elemento
a[3]

#Soma dos elementos
sum(a)
2+4+6
#Contagem de elementos de um vetor
length(a)
#'
#'-Matrizes
#'Transformando os vetores em uma matriz por coluna
c <- cbind(a, b)
c
# Transformando os vetores em uma matriz por linha
d <- t(c)
d

# Transformando os vetores em uma matriz por linha com função rbind()
d <- rbind(a, b)
d
class(d)


#'-dataframes
# vetores
(fisiologia <- c("c3", "c4", "c3", "c4"))
(especie <- rep(c("soja", "milho"), times = 2))
(d13c <- c(-24,-13,-26,-11))
# criar um dataframe com esses vetores
(dados <- data.frame(fisiologia, especie , d13c))
#'
#'
#'-listas
#'
lista<- list(vetor_a = a,
              matrix_d = d,
              df = dados)
lista$df
#'
#'-funções
#'Média
#Na unha
media <- function(x) {
  soma <- sum(x)
  n <- length(x)
  xbarra <- soma / n
  return(xbarra)
}

media(a)
mean(a)



#'
#'##########################################################################
#                           EXERCÍCIO                                     #
#'#########################################################################
#'
#'Calcular o peso médio e a mediana de uma cana de certa variedade no canavial
#'de uma usina. Segue-se os dados:
#'
pc <- c(1.58, 1.76, 1.38, 1.71, 1.50, 1.32, 1.51, 1.55, 1.54, 1.67)
#Na unha
#'
#'-Média 
media(pc)

#'-Mediana 
#'Passo 1: ordene os valores em ordem crescente
pc_ord<-sort(pc,decreasing = FALSE)
pc_ord

#'Passo 2: calcular as estatísticas de ordem
n<-length(pc_ord)
n
#'Calcular X(n/2) e X(n/2 + 1)
#'
(Est1 <-n/2)
(Est2 <-n/2 + 1)

Md<-(pc_ord[Est1]+pc_ord[Est2])/2
Md

#Pela função do R base
Md_rbase <- median(pc_ord)
Md_rbase


# Instalando pacotes -------------------------------------------------------------------
#'
#'O que é um pacote R?
#'
#'Um pacote agrupa código, dados, documentação e testes e é fácil
#'de compartilhar com outras pessoas.
#'
#'
#'Refêrencia básica
#'Livro: https://r-pkgs.org/
#'
library(readxl)
library(tidyverse)

#'
#'
install.packages("readxl", "tidyverse", dep=TRUE)
#Tools -> Install packages

#Auto instalação
if(!require(readxl))install.packages("readxl", dep = TRUE)

#'##########################################################################
#                           EXERCÍCIO                                     #
#'#########################################################################
#'
#'Instale o pacote: "ExpDes.pt".
#'
#Carregar pacote
library(ExpDes.pt)

# Importação e exportação de dados -----------------------------------------------------
#'
#'Formas de importação
#'
#'1) File -> importe dataset
#'
#'2) via linha de comando formado .xlsx
dados_xlsx <- readxl::read_excel("dados/bezerros.xlsx", sheet = 1)
head(dados_xlsx)
view(dados_xlsx)


#'
#'3) formato .csv
#'
#'Classificar as células como "geral" ou "texto" e NUNCA como 
#'númerico, para evitar a perda de casas decimais.
#'
dados_csv <-
  read.table("dados/bezerro.csv",
             header = T,
             sep = ",")
head(dados_csv)
view(dados_csv)
print(dados_csv)
dados_csv

#'
#'Exportar dados no formato .xlsx
#'
library(writexl)
writexl::write_xlsx(dados_xlsx, "dados/dados_xlsx.xlsx")

#Exportar dados no formato .csv
write.table(dados_xlsx, file = "dados/bezerro.csv", sep = ",")

#Agora seu R tem muita coisa que tal fazer uma limpeza
rm(list = ls(all = T))#Limpar memória


#'##########################################################################
#                           EXERCÍCIO                                     #
#'#########################################################################
#'
#'Calcule as seguintes estimativas para o conjunto de dados bezerros:
#'
#'1 - Média aritmética;
#'2 - Mediana;
#'3 - Variância;
#'4 - Erro padrão da média;
#'5 - Desvio padrão;
#'6 - Coeficiente de variação.
#'



