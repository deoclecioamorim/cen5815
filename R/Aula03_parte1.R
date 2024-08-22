#'##############################################################################
#'       CEN5815 - Análise de Dados Agronômicos e Ambientais                   #
#'       Prof. Deoclecio Jardim Amorim                                         #
#'       Aula 03: DQL                                                          #
#'##############################################################################
#'
options(scipen = 999)# Este valor alto (999) evita o retorno dos números em 
#notação científica.
# Limpar área de trabalho ---------------------------------------------------------------------
rm(list = ls())      # Remove todos os objetos do ambiente de trabalho
gc(reset = TRUE)     # Libera memória
graphics.off()       # Fecha todas as janelas gráficas abertas
#'
#'Conteúdo
#'
#'-Estatística descritiva
#'-ANOVA DQL
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


# Exemplo 1: DQL ------------------------------------------------------------------------------
#'###############################################################################
#'        Análise de um experimento instalado em quadrado latino - DQL          #
#'###############################################################################
#'
#'Exemplo de experimento instalado segundo o DQL, para avaliar a produção de cana-
#'de-açúcar em kg/parcela, de cinco variedades.
#'
#'Carregamento da base de dados
dados_dql <- read_excel("dados/aula03.xlsx", sheet = 1)
view(dados_dql)

# Verificando a estrutura dos dados
str(dados_dql)

#'
#'ATENÇÃO: tratamentos qualitativos devem ser sempre tratados
#'como fatores!!!
#'
# Transformando em fatores
dados_dql <- transform(dados_dql, trat = as.factor(trat), linha=as.factor(linha),
                       coluna=as.factor(coluna))
str(dados_dql)

# Algumas estatísticas descritivas: média e desvio
# Média geral
(ybar <- mean(dados_dql$prod))

#Combinando operações com pipe (%>%)
estat_descr_dql <- dados_dql %>%
  group_by(trat) %>% summarise(
    media_trat = mean(prod, na.rm = TRUE),
    desvio_trat = sd(prod, na.rm = TRUE),
    EPM =desvio_trat/sqrt(length(prod)) #erro padrão da média
  )

estat_descr_dql

# Modelo estatístico: y_ijk = m + l_j+ c_k + t_i + e_ijk
#'
#'Hipóteses
#'
#'H0: mu1 = mu2 =...= muI = mu
#'Ha: pelo menos duas médias populacionais diferem entre si.
#'
mod_dql <- lm(prod ~ linha+coluna+trat, data = dados_dql)
anova(mod_dql)

# F tabelado 5%
qf(0.95, 4, 12)

# Nível descritivo
1-pf(12.0905, 4, 12)

#'
#'Conclusão: Como Fcal>Ftab rejeitamos a hipótese H0 e concluímos que existem
#'pelo menos duas médias que diferem entre si.
#' 

