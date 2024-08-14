# Instale o pacote ggplot2 se ainda não o tiver
# install.packages("ggplot2")

library(ggplot2)

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
  geom_vline(xintercept = critical_value, color = "red", linetype = "dashed", size = 1) +
  geom_area(data = data_rejection, aes(x = F_value, y = Density), fill = "red", alpha = 0.4) +
  labs(title = "Distribuição F",
       x = "F-value", y = "Densidade") +
  annotate("text", x = critical_value + 2, y = max(y)/2,
           label = paste("Valor crítico F (α = 0.05):", round(critical_value, 2)),
           color = "red", angle = 0, vjust = 1.5) +
  annotate("text", x = critical_value + 2, y = max(y)/8,
           label = paste("Área de rejeição (α = 0.05)"),
           color = "red", size = 4) +
  theme_minimal()

