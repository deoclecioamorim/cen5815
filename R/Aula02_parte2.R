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



# Exemplo 2: DBC ------------------------------------------------------------------------------
#'###############################################################################
#'        Análise de um experimento em blocos casualizados- DBC                 #
#'###############################################################################
#'
#'Os dados de um experimento instalado no delineamento casualizado em blocos, 
#'cujo objetivo é comparar nove porta-enxertos para laranjeira Valência. Cada parcela 
#'era constituı́da por duas plantas e as produções de laranja (número médio de frutos
#'por planta) tomadas dois anos após a instalação do experimento.
#'
#'Carregamento da base de dados
dados_dbc <- read_excel("dados/aula02.xlsx", sheet = 4)

# Exibindo as primeiras e últimas da base de dados
cat("Primeiras e últimas linhas da base de dados:\n")
print(head(dados_dbc)); print(tail(dados_dbc))

# Verificando a estrutura dos dados
str(dados_dbc)

# Transformando progenies em uma variável qualitativa 
dados_dbc <- transform(dados_dbc, trat = as.factor(trat), bloco=as.factor(bloco))
str(dados_dbc)

# Algumas estatísticas descritivas: média e desvio
# Média geral
(ybar <- mean(dados_dbc$num_fruto))

#Combinando operações com pipe (%>%)
estat_descr_dbc <- dados_dbc %>%
  group_by(trat) %>% summarise(
    media_trat = mean(num_fruto, na.rm = TRUE),
    desvio_trat = sd(num_fruto, na.rm = TRUE),
    EPM =desvio_trat/sqrt(length(num_fruto)) #erro padrão da média
  )

estat_descr_dbc

# Modelo estatístico: y_ij = m + b_j + t_i + e_ij
#'
#'Hipóteses
#'
#'H0: mu1 = mu2 =...= muI = mu
#'Ha: pelo menos duas médias populacionais diferem entre si.
#'
mod_dbc <- lm(num_fruto ~ 1+bloco+trat, data = dados_dbc)
summary(mod_dbc)
anova(mod_dbc)

# F tabelado 1%
qf(0.99, 8, 16)

# F tabelado 5%
qf(0.95, 8, 16)

# Nível descritivo
1-pf(11.4114, 3, 16)

#'
#'Conclusão: Como Fcal>Ftab rejeitamos a hipótese H0 e concluímos que existem
#'pelo menos duas médias que diferem entre si.
#' 


# Bonus: gráfico da distribuição F ------------------------------------------------------------

# Definindo os parâmetros para a distribuição F
df1 <- 8  # graus de liberdade do numerador
df2 <- 16 # graus de liberdade do denominador
x <- seq(0, 12,length.out = 200)  # valores de x para o gráfico

# Calculando os valores da distribuição F
y <- df(x, df1, df2)

# Criando um data frame para o ggplot
data <- data.frame(F_value = x, Density = y)

# Calculando o valor crítico F para o nível de significância alpha
alpha <- 0.05
critical_value <- qf(1 - alpha, df1, df2)
p_valor <-1-pf(11.41,8,16)

# Criando um data frame para a área de rejeição
data_rejection <- subset(data, F_value >= critical_value)
data_rejection
# Criando o gráfico com ggplot2
distF<-ggplot(data, aes(x = F_value, y = Density)) +
  geom_line(color = "blue", size = 1.2) +
  geom_vline(xintercept = critical_value, color = "black", linetype = "dashed", size = 1) +
  geom_area(data = data_rejection, aes(x = F_value, y = Density), fill = "red", alpha = 0.4) +
  labs(title = "Distribuição F",
       x = "Valor de F", y = "Densidade") +
  annotate("text", x = critical_value + 2, y = max(y)/2,
           label = paste("Valor crítico F (α = 0.05):", round(critical_value, 2)),
           color = "black", angle = 0, vjust = 1.5, size=5) +
  annotate("text", x = critical_value + 2, y = max(y)/10,
           label = paste("Área de rejeição (α = 0.05)"),
           color = "red", size = 6)+ theme_bw() +
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size = 1, colour = "black"),
    panel.border = element_rect(fill = NA, linetype = 1, colour = "black"),
    panel.background = element_blank(),
    axis.text.x = element_text(colour = "black", size = 12, face = "plain"),
    axis.title.x = element_text(colour = "black", size = 12, face = "plain"),
    axis.text.y = element_text(colour = "black", size = 12, face = "plain"),
    axis.title.y = element_text(colour = "black", size = 12, face = "plain"),
    legend.title = element_text(color = "black", size = 12),
    legend.text = element_text(color = "red", size = 14),
    plot.title = element_text(color = "blue")  # Título em vermelho
  ) 

distF

png(filename="figuras/distF.png", # Nome do arquivo e extensão
    width = 11,    # largura
    height = 6,   # Altura
    res= 400,# Resolução em dpi
    family = "serif", #fonte
    units = "in")  # Unidades.
distF
dev.off() # Fecha a janela gráfica



