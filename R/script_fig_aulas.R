# Pacotes -------------------------------------------------------------------------------------
if(!require(readxl))install.packages("readxl", dep = TRUE)
if(!require(tidyverse))install.packages("tidyverse", dep = TRUE)

# Limpar area de trabalho ---------------------------------------------------------------------
rm(list = ls())
gc(reset = T)
graphics.off()


# Fig 3 da aula 1 -----------------------------------------------------------------------------



# Gerar alguns dados de exemplo
set.seed(123)
dados <- data.frame(grupo = rep(c("A", "B"), each = 50), valores = c(rnorm(50, mean = 5), rnorm(50, mean = 7)))

# Criar o gráfico box-plot
box_plot <- ggplot(dados, aes(x = grupo, y = valores)) +
  geom_boxplot() +
  labs(title = "Estatística descritiva", y = "Valores", x="Grupo")+
  theme_bw() +
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size = 1, colour = "black"),
    #panel.grid.major = element_line(size = 0.5, colour = "grey80"),
    #panel.grid.minor = element_line(size = 0.25, colour = "grey90"),
    panel.border = element_rect(fill = NA, linetype = 1, colour = "black"),
    panel.background = element_blank(),
    axis.text.x = element_text(colour = "black", size = 11, face = "plain"),
    axis.title.x = element_text(colour = "black", size = 11, face = "plain"),
    axis.text.y = element_text(colour = "black", size = 11, face = "plain"),
    axis.title.y = element_text(colour = "black", size = 11, face = "plain"),
    legend.title = element_text(color = "black", size = 11),
    legend.text = element_text(color = "black", size = 10.5),
    plot.title = element_text(color = "blue")  # Título em vermelho
  ) 

print(box_plot)


# Calcular a média e o intervalo de confiança
dados_resumo <- dados %>%
  group_by(grupo) %>%
  summarise(media = mean(valores),
            sd = sd(valores),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci_lower = media - qt(0.975, df = n-1) * se,
         ci_upper = media + qt(0.975, df = n-1) * se)

# Criar o gráfico de intervalo de confiança
ci_plot <- ggplot(dados_resumo, aes(x = grupo, y = media)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1) +
  labs(title = "Estatística inferêncial", y = "Média dos Valores", x="Grupo")+
  theme_bw() +
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size = 1, colour = "black"),
    #panel.grid.major = element_line(size = 0.5, colour = "grey80"),
    #panel.grid.minor = element_line(size = 0.25, colour = "grey90"),
    panel.border = element_rect(fill = NA, linetype = 1, colour = "black"),
    panel.background = element_blank(),
    axis.text.x = element_text(colour = "black", size = 11, face = "plain"),
    axis.title.x = element_text(colour = "black", size = 11, face = "plain"),
    axis.text.y = element_text(colour = "black", size = 11, face = "plain"),
    axis.title.y = element_text(colour = "black", size = 11, face = "plain"),
    legend.title = element_text(color = "black", size = 11),
    legend.text = element_text(color = "red", size = 10.5),
    plot.title = element_text(color = "blue")  # Título em vermelho
  ) 

  

# Exibir o gráfico de intervalo de confiança
print(ci_plot)


png(filename="figuras/fig3_aula1.png", # Nome do arquivo e extensão
    width = 11,    # largura
    height = 6,   # Altura
    res= 400,# Resolução em dpi
    family = "serif", #fonte
    units = "in")  # Unidades.
gridExtra::grid.arrange(box_plot,ci_plot, nrow=1)
dev.off() # Fecha a janela gráfica



# Figura Distribuição F ---------------------------------------------------

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


