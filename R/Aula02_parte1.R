#'##############################################################################
#'       CEN5815 - Análise de Dados Agronômicos e Ambientais                   #
#'       Prof. Deoclecio Jardim Amorim                                         #
#'       Aula 02: DIC e DBC                                                    #
#'##############################################################################
# Limpar área de trabalho ---------------------------------------------------------------------
rm(list = ls())      # Remove todos os objetos do ambiente de trabalho
gc(reset = TRUE)     # Libera memória
graphics.off()       # Fecha todas as janelas gráficas abertas
#'
#'Conteúdo
#'
#'-Estatística descritiva
#'-ANOVA DIC
#'-ANOVA DBC
#'
#'###############################################################################
#'               Instalação e carregamento de pacotes necessários               #
#'###############################################################################
# Pacotes utilizados
pacotes <- c("readxl", #Ler arquivos do Excel
             "tidyverse" #carregar outros pacotes do R
) 

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Exemplo 1: DIC ------------------------------------------------------------------------------
#'###############################################################################
#'        Análise de um experimento inteiramente casualizado - DIC              #
#'###############################################################################
#'
#'Exemplo de alimentação de porcos em que se usaram quatro rações (A, B, C, D)
#'
#'Carregamento da base de dados
dados_dic <- read_excel("dados/aula02.xlsx", sheet = 3)

# Exibindo as primeiras e últimas da base de dados
cat("Primeiras e últimas linhas da base de dados:\n")
print(head(dados_dic)); print(tail(dados_dic))

# Verificando a estrutura dos dados
str(dados_dic)

# Transformando progenies em uma variável qualitativa 
dados_dic <- transform(dados_dic, trat = as.factor(trat))
str(dados_dic)

# Algumas estatísticas descritivas: média e desvio
# Média geral
(ybar <- mean(dados_dic$peso))

#Combinando operações com pipe (%>%)
estat_descr_dic <- dados_dic %>%
  group_by(trat) %>% summarise(
    media_trat = mean(peso, na.rm = TRUE),
    desvio_trat = sd(peso, na.rm = TRUE),
    EPM =desvio_trat/sqrt(length(peso)) #erro padrão da média
  )

estat_descr_dic

# Modelo estatístico: y_ij = m + t_i + e_ij
#'
#'Hipóteses
#'
#'H0: mu1 = mu2 =...= muI = mu
#'Ha: pelo menos duas médias populacionais diferem entre si.
#'
mod_dic <- lm(peso ~ trat, data = dados_dic)
anova(mod_dic)

# F tabelado 1%
qf(0.99, 3, 16)

# F tabelado 5%
qf(0.95, 3, 16)

# Nível descritivo
1-pf(3.9939, 3, 16)

#'
#'Conclusão: Como Fcal>Ftab rejeitamos a hipótese H0 e concluímos que existem
#'pelo menos duas médias que diferem entre si.
#' 


