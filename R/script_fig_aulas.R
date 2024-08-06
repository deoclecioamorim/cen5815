#'##############################################################################################
#'Código para extração de dados climaticos do Worldclim: https://www.worldclim.org/
#'

#'
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




