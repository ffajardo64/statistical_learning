library(ggplot2)

# Parâmetros
mu0 <- 0
mua <- 1
sigma <- 1
alpha <- 0.1

# Valor crítico
crit_val <- qnorm(1 - alpha, mean = mu0, sd = sigma)

# Criar dados para as curvas
x <- seq(-4, 4, length.out = 1000)
df <- data.frame(
  x = rep(x, 2),
  y = c(dnorm(x, mu0, sigma), dnorm(x, mua, sigma)),
  grupo = rep(c("H0 (Verdadeira)", "H1 (Alternativa)"), each = 1000)
)

# Plot
ggplot(df, aes(x = x, y = y, color = grupo)) +
  geom_line(size = 1) +
  
  # Região Erro Tipo I (Alpha)
  geom_area(data = subset(df, x > crit_val & grupo == "H0 (Verdadeira)"), 
            aes(y = y), fill = "red", alpha = 0.5) +
  
  # Região Erro Tipo II (Beta)
  geom_area(data = subset(df, x < crit_val & grupo == "H1 (Alternativa)"), 
            aes(y = y), fill = "blue", alpha = 0.3) +
  
  # Linha vertical do valor crítico
  geom_vline(xintercept = crit_val, linetype = "dashed", color = "black") +
  
  # Anotações
  annotate("text", x = 2.5, y = -0.01, label = "Erro Tipo I (Alpha)", color = "red") +
  annotate("text", x = 0, y = -0.01, label = "Erro Tipo II (Beta)", color = "blue") +
  
  labs(
    #title = "Visualização de Erros Tipo I e Tipo II",
    #   subtitle = "Vermelho = Falso Positivo, Azul = Falso Negativo",
       x = "Média Amostral",
       y = "Densidade") +
  # theme_minimal()
theme(text = element_text(size=20),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_blank())
