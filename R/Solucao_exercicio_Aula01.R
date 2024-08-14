#'##############################################################################
#'       CEN5815 - Análise de Dados Agronômicos e Ambientais                   #
#'       Prof. Deoclecio Jardim Amorim                                         #
#'       Aula 01: Introdução à linguagem de programação R                      #
#'##############################################################################
# Limpar área de trabalho ---------------------------------------------------------------------
rm(list = ls())      # Remove todos os objetos do ambiente de trabalho
gc(reset = TRUE)     # Libera memória
graphics.off()       # Fecha todas as janelas gráficas abertas

# Pacotes -------------------------------------------------------------------------------------
if(!require(readxl))install.packages("readxl", dep = TRUE)


# Importar dados ------------------------------------------------------------------------------

pesobezerro <- readxl::read_excel("dados/bezerros.xlsx", sheet = 1)
str(pesobezerro) # verificar a estrutura dos dados
view(pesobezerro) #visualizar
#'
#'Calcule as seguintes estimativas para o conjunto de dados bezerros:
#'
#'1 - Média aritmética;
#'2 - Mediana;
#'3 - Variância;
#'4 - Desvio padrão;
#'5 - Erro padrão da média;
#'6 - Coeficiente de variação.
#'
# Cálculos na unha ----------------------------------------------------------------------------

# Calculando o número de elementos 
n <- length(pesobezerro$peso )
n

#Cálculo na unha
#--Média
xbar_unha <- sum(pesobezerro$peso)/n
xbar_unha


#--Mediana
#'Passo 1: ordene os valores em ordem crescente
peso_ord<-sort(pesobezerro$peso,decreasing = FALSE)
peso_ord

#'Passo 2: calcular as estatísticas de ordem
#'Calcular X(n/2) e X(n/2 + 1)
#'
(Est1 <-n/2)
(Est2 <-n/2 + 1)

Md_unha<-(peso_ord[Est1]+peso_ord[Est2])/2
Md_unha

#--Variância

Somax_i2<-sum(pesobezerro$peso^2)
somax_i<-sum(pesobezerro$peso)

var_unha <- 1/(n-1)*(Somax_i2-somax_i^2/n)
var_unha

#--Desvio padrão
Sx <- sqrt(var_unha)
Sx

#--Erro padrão da média

erro_pa_xbar_unha <- Sx/sqrt(n)
erro_pa_xbar_unha

#--Coeficiente de variação
CV_unha <- (Sx/xbar_unha)*100
CV_unha




# Cálculos com funções  -----------------------------------------------------------------------

#--Média
(xbar <- mean(pesobezerro$peso))

#--Mediana
(Md <- median(pesobezerro$peso))


#--Variância
(vari <- var(pesobezerro$peso))


#--Desvio padrão
(DP <- sd(pesobezerro$peso))

#--Erro padrão da média
(Erro_pa_media <- DP/sqrt(n))

# Calculando o coeficiente de variação (CV)
(CV <- (DP * 100) / xbar)


# Exibindo os resultados na forma de tabela
resultados <- data.frame(
  Estatística = c("Média", "Mediana","Variância", "Desvio Padrão",
                  "Erro Padrão da Média","Coeficiente de Variação"),
  Valor = c(xbar, Md, vari, DP,Erro_pa_media, CV)
)

print(resultados)





