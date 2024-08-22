#'##############################################################################
#'       CEN5815 - Análise de Dados Agronômicos e Ambientais                   #
#'       Prof. Deoclecio Jardim Amorim                                         #
#'       Aula 03: Figuras auxiliares                                                         #
#'##############################################################################
#'
options(scipen = 999)# Este valor alto (999) evita o retorno dos números em 
#notação científica.
# Limpar área de trabalho ---------------------------------------------------------------------
rm(list = ls())      # Remove todos os objetos do ambiente de trabalho
gc(reset = TRUE)     # Libera memória
graphics.off()       # Fecha todas as janelas gráficas abertas
#'
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


# Entrada dos dados
y<- c( 2,  2,  1,  1,  0,
       1,  0,  0,  1,  1,
       12, 10, 14, 17, 11,
       7,  9, 15,  8, 10)
trat<- rep(c("A","B","C","D"), each=5)
dados<- data.frame(trat, y)

# Verificação dos dados
head(dados)

#'
#'Modelo estatístico: y_ij = m + t_i + e_ij

modelo<-lm(y ~ trat, dados)

# Obtenção dos resíduos
#' 
#' Resíduos simples
res <- residuals(modelo)
res
#' Resíduos estudentizados
res_Stud <- rstandard(modelo)
res_Stud

#' Resíduos obtidos
round(head(data.frame(res,res_Stud)),5)



# Gráfico 1: Resíduos estudentizados vs tratamentos
graf1 <- ggplot(data = dados, aes(x = trat, y = res_Stud)) +
  geom_point() +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "red") +
  labs(x = "Tratamentos", y = "Resíduos Estudentizados") +
  theme_bw() +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    panel.border = element_rect(fill = NA, colour = "black"),
    panel.background = element_blank(),
    axis.text.x = element_text(colour = "black", size = 14),  # Aumenta o texto do eixo X
    axis.title.x = element_text(colour = "black", size = 16), # Aumenta o título do eixo X
    axis.text.y = element_text(colour = "black", size = 14),  # Aumenta o texto do eixo Y
    axis.title.y = element_text(colour = "black", size = 16)  # Aumenta o título do eixo Y
  )

graf1



# Gráfico 2: Boxplot dos resíduos estudentizados (sem considerar tratamentos e sem escala no eixo x)
graf2 <- ggplot(data = data.frame(res_Stud), aes(y = res_Stud)) +
  geom_boxplot() +
  labs(y = "Resíduos Estudentizados") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),  # Remove o título do eixo X
    axis.text.x = element_blank(),   # Remove os textos do eixo X
    axis.ticks.x = element_blank(),  # Remove as marcações do eixo X
    axis.text.y = element_text(colour = "black", size = 14),  # Aumenta o texto do eixo Y
    axis.title.y = element_text(colour = "black", size = 16), # Aumenta o título do eixo Y
    panel.border = element_rect(fill = NA, colour = "black"), # Borda do painel
    panel.background = element_blank(),                      # Fundo do painel em branco
    axis.line.y = element_line(size = 1, colour = "black")   # Linha do eixo Y em preto
  )

graf2

png(filename="figuras/resid1.png", # Nome do arquivo e extensão
    width = 11,    # largura
    height = 6,   # Altura
    res= 400,# Resolução em dpi
    family = "serif", #fonte
    units = "in")  # Unidades.
gridExtra::grid.arrange(graf1, graf2, nrow=1)
dev.off() # Fecha a janela gráfica



plot(res)
