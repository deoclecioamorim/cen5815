#'##############################################################################
#'      CEN5815 - Análise de Dados Agronômicos e Ambientais                    #
#'      Prof. Deoclecio Jardim Amorim                                          #
#'      Aula 03: Pressuposições do modelo estatístico e transformação de dados #                                                       #
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
             "tidyverse", #Carregar outros pacotes do R
             "lawstat", # Teste de homogeneidade
             "MASS" #Transformação - Box-Cox
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



# Exemplo (Barbin, 1994) ----------------------------------------------------------------------

#'
#'Um pesquisador pretende comparar quatro variedades de pêssego quanto 
#'ao enraizamento de estacas. Para tanto, realizou um experimento de acordo 
#'com o delineamento inteiramente casualizado com cinco repetições, sendo 
#'cada parcela um vaso com vinte estacas. Passado o tempo necessário, o 
#'pesquisadoranotou o número de estacas enraizadas, apresentado na Tabela 
#'a seguir.
#'

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

modelo <- lm(y ~ trat, dados)

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


# Observações discrepantes --------------------------------------------------------------------

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

#Salvar figuras na pasta figuras
png(filename="figuras/resid1.png", # Nome do arquivo e extensão
    width = 11,    # largura
    height = 6,   # Altura
    res= 400,# Resolução em dpi
    family = "serif", #fonte
    units = "in")  # Unidades.
gridExtra::grid.arrange(graf1, graf2, nrow=1)
dev.off() # Fecha a janela gráfica



# Homogeneidade de variâncias --------------------------------------------------
#'
#'Hipóteses
#' 
#'H0: as variâncias dos tratamentos são homogêneas;
#'Ha: as variâncias dos tratamentos NÃO são homogêneas;
#'
#'
# Análise gráfica
graf3 <- ggplot(dados, aes(x = trat, y = res_Stud)) + 
  geom_dotplot(binaxis = "y", stackdir = "center", fill = "blue") +
  xlab("Tratamentos")+ylab("Resíduos Studentizados")+theme_bw() +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    panel.border = element_rect(fill = NA, colour = "black"),
    panel.background = element_blank(),
    axis.text.x = element_text(colour = "black", size = 15),  # Aumenta o texto do eixo X
    axis.title.x = element_text(colour = "black", size = 17), # Aumenta o título do eixo X
    axis.text.y = element_text(colour = "black", size = 15),  # Aumenta o texto do eixo Y
    axis.title.y = element_text(colour = "black", size = 17)  # Aumenta o título do eixo Y
  )

graf3

#Salvar figuras na pasta figuras
png(filename="figuras/homogen.png", # Nome do arquivo e extensão
    width = 12,    # largura
    height = 6,   # Altura
    res= 400,# Resolução em dpi
    family = "serif", #fonte
    units = "in")  # Unidades.
graf3
dev.off() # Fecha a janela gráfica


# Teste de Levene para verificação da homogeneidade de variâncias
anova(lm(abs(res) ~ trat, dados))

#'Ou

#library(lawstat)
lawstat::levene.test(dados$y, dados$trat, location = "mean")

#'
#'Conclusão: As variâncias podem ser consideradas homogêneas.
#'

# Normalidade ---------------------------------------------------------------------------------
#'
#'Distribuição dos resíduos 
#'
#Criando o histograma com a curva normal teórica

graf4 <- ggplot(data = data.frame(res_Stud), aes(x = res_Stud)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "blue", 
                 color = "black", alpha = 0.4) +
  stat_function(fun = dnorm, args = list(mean = mean(res_Stud), sd = sd(res_Stud)), 
                color = "red", size = 1) + xlab("Resíduos")+
  ylab("Densidade")+theme_bw()+
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    panel.border = element_rect(fill = NA, colour = "black"),
    panel.background = element_blank(),
    axis.text.x = element_text(colour = "black", size = 15),  # Aumenta o texto do eixo X
    axis.title.x = element_text(colour = "black", size = 17), # Aumenta o título do eixo X
    axis.text.y = element_text(colour = "black", size = 15),  # Aumenta o texto do eixo Y
    axis.title.y = element_text(colour = "black", size = 17)  # Aumenta o título do eixo Y
  )


graf4

#Salvar figura na pasta figura
png(filename="figuras/normali.png", # Nome do arquivo e extensão
    width = 12,    # largura
    height = 6,   # Altura
    res= 400,# Resolução em dpi
    family = "serif", #fonte
    units = "in")  # Unidades.
graf4
dev.off() # Fecha a janela gráfica


#'
#'Hipóteses
#' 
#'H0: a variável dependente segue distribuição normal;
#'Ha: a variável dependente NÃO segue distribuição normal;
#'
#'
#'Análise gráfica
qqnorm(res_Stud)
qqline(res_Stud, col=2)

#'
#'Com ggplot2
#'
ggplot(dados, aes(sample = res_Stud)) +
  stat_qq() +               # Add QQ plot
  stat_qq_line(color = "red", size = 1.5) +  # Add QQ line
  theme_minimal() + xlab("Quantis da distribuição 
       normal")+ylab("Sample Quantiles")+
  ggtitle("Normal Q-Q Plot") # Add title


#'
#'Teste formal
#'Teste Shapiro-Wilk para verificação da normalidade dos erros
shapiro.test(res_Stud)

#'
#'Conclusão: Os dados não seguem uma distribuição normal, 
#'então podemos usar métodos não paramétricos ou buscar 
#'uma transformação para normalizar os dados. 
#'


# Transformação de dados ----------------------------------------------------------------------
#'
#'Verificando a necessidade de transformação
plot(res_Stud~fitted(modelo),ylab="Resíduos Studentizados",
     xlab="Valores esperados (médias)")
abline(h=0, col=2)

#'
#'Como existem valores zero no conjunto dados vamos adicionar uma constante
#'aos dados.
#'
#'Testando algumas transformações
dados_aum <- dados %>% mutate(y_orig_c= y+0.001, raiz_y = sqrt(y+0.001),
                              log_y = log(y+0.001), raiz_cubica_y=(y+0.001)^1/3)

dados_aum


# Transformar os dados no formato longo para facilitar a plotagem
data_long <- dados_aum %>%
  pivot_longer(cols = c(y_orig_c, raiz_y, log_y, raiz_cubica_y), names_to = "variavel", values_to = "valor")



# Verificando a distorção
ggplot(data_long, aes(x = y, y = valor, color = variavel)) +
  # Adicionar as linhas ao gráfico
  geom_line(size = 1) +  # Ajuste o tamanho da linha para 1.2 para uma aparência mais robusta
  # Adicionar pontos ao gráfico
  geom_point(size = 1.4) +  # Ajuste o tamanho dos pontos para 3 para melhor visibilidade
  # Adicionar rótulos ao gráfico
  labs(
    title = "",
    x = "Dados escala original",
    y = "Valor Transformado",
    color = "Tipo de Variável"  # Rótulo da legenda para as cores
  ) +
  # Definir tema do gráfico
  theme_bw() +
  # Personalizar as cores das linhas
  scale_color_manual(values = c("y_orig_c" = "black", "raiz_y" = "blue", "log_y" = "red", "raiz_cubica_y" = "green")) +
  # Remover a estética `linetype` e garantir que todas as linhas sejam sólidas
  theme(
    legend.position = "top",  # Colocar a legenda na parte superior
    legend.title = element_text(size = 10),  # Ajustar o tamanho do título da legenda
    legend.text = element_text(size = 9),  # Ajustar o tamanho do texto da legenda
    axis.title = element_text(size = 12),  # Ajustar o tamanho dos títulos dos eixos
    axis.text = element_text(size = 10),  # Ajustar o tamanho dos textos dos eixos
    plot.title = element_text(size = 14, face = "bold")  # Ajustar o tamanho e estilo do título do gráfico
  )


#Correlações
cor(dados_aum$y_orig_c, dados_aum$raiz_y)
cor(dados_aum$y_orig_c, dados_aum$log_y)
cor(dados_aum$y_orig_c, dados_aum$raiz_cubica_y)


# Calculando a correlação
correlacao <- cor(dados_aum$y_orig_c, dados_aum$raiz_y)

# Exemplo de gráfico de dispersão com linha de tendência e valor da correlação
graf5 <- ggplot(dados_aum, aes(x = y_orig_c, y = raiz_y)) +
  geom_point(color = "blue", size = 2) +  # Adiciona os pontos de dispersão
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "solid") +  # Adiciona a linha de tendência
  labs(title = "",
       x = "Escala original",
       y = "Escala transformada") +
  annotate("text", x = 10, y = 4, label = paste("r =", round(correlacao, 2)),
           hjust = 1.1, vjust = 1.5, size = 5, color = "black") +  # Adiciona o valor da correlação
  theme_bw()+ # Define um tema minimalista para o gráfico
theme(
    axis.line = element_line(size = 1, colour = "black"),
    panel.border = element_rect(fill = NA, colour = "black"),
    panel.background = element_blank(),
    axis.text.x = element_text(colour = "black", size = 15),  # Aumenta o texto do eixo X
    axis.title.x = element_text(colour = "black", size = 17), # Aumenta o título do eixo X
    axis.text.y = element_text(colour = "black", size = 15),  # Aumenta o texto do eixo Y
    axis.title.y = element_text(colour = "black", size = 17)  # Aumenta o título do eixo Y
  )


graf5


#Salvar figura
png(filename="figuras/cor_transf.png", # Nome do arquivo e extensão
    width = 12,    # largura
    height = 6,   # Altura
    res= 400,# Resolução em dpi
    family = "serif", #fonte
    units = "in")  # Unidades.
graf5
dev.off() # Fecha a janela gráfica


#'Transformação Box-cox

MASS::boxcox(dados_aum$y_orig_c ~ dados_aum$trat,ylab="logaritmo da 
       verossimilhança") #lambda=0,5.

#' Análise dos Dados Transformados
dados$yt<- (y+0.01)^0.5
modelot<- lm(yt ~ trat, dados)

#' ## Verificação da normalidade dos resíduos
qqnorm(rstandard(modelot), xlab="Quantis da distribuição 
       normal", ylab="Resíduos Studentizados")
qqline(rstandard(modelot), col=2)
shapiro.test(rstandard(modelot))

#'Verificação da homogeneidade de variâncias
#'
ggplot(dados, aes(x = trat, y = rstandard(modelot))) + 
  geom_dotplot(binaxis = "y", stackdir = "center", fill = "blue") +
  xlab("Tratamentos")+ylab("Resíduos Studentizados")+theme_bw() +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    panel.border = element_rect(fill = NA, colour = "black"),
    panel.background = element_blank(),
    axis.text.x = element_text(colour = "black", size = 15),  # Aumenta o texto do eixo X
    axis.title.x = element_text(colour = "black", size = 17), # Aumenta o título do eixo X
    axis.text.y = element_text(colour = "black", size = 15),  # Aumenta o texto do eixo Y
    axis.title.y = element_text(colour = "black", size = 17)  # Aumenta o título do eixo Y
  )


#'
#'Teste de Levene
#'
levene.test(dados$yt, dados$trat, location = "mean")

#'
#'Verificando a necessidade de nova transformação dos dados
plot(rstandard(modelot)~fitted(modelot),ylab="Resíduos
     Studentizados",xlab="Valores esperados (médias)")
abline(h=0)
#'
#'Transformação Box-Cox 
#'
MASS::boxcox(modelot,ylab="logaritmo da verossimilhança")

#'
#'Análise de variância para os dados transformados
anova(modelot)
