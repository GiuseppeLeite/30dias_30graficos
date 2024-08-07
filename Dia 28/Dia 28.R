# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar as bibliotecas necessárias
library(ggplot2)
library(tidyr)
library(dplyr)

# Carregar os dados
data <- read.csv("multiTimeline.csv", sep = ";")

# Transformar os dados para o formato longo
data_long <- data %>%
  pivot_longer(cols = c(GraphPad.Prism, Rstudio, spss),
               names_to = "Software",
               values_to = "Popularity")

# Converter a coluna data para o formato de data
data_long$data <- as.Date(paste0(data_long$data, "-01"))

# Filtrar os dados para o período de 2004 a 2024
data_filtered <- data_long %>%
  filter(data >= as.Date("2004-01-01") & data <= as.Date("2024-12-31"))

# Encontrar o ano em que o uso do R superou o uso do SPSS
crossover_year <- data %>%
  filter(Rstudio > spss) %>%
  summarize(first_crossover = min(as.Date(paste0(data, "-01")))) %>%
  pull(first_crossover)

# Criar o gráfico de linhas com uma linha vertical no ano de crossover
p <- ggplot(data_filtered, aes(x = data, y = Popularity, color = Software, group = Software)) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.numeric(crossover_year), linetype = "dashed", color = "black", size = 0.3) +
  scale_color_manual(values = c("GraphPad.Prism" = "#e0cb7e", "Rstudio" = "#2b48b3", "spss" = "#b32b48")) +
  scale_x_date(breaks = as.Date(c("2004-01-01", "2009-01-01", "2014-01-01", "2019-01-01", "2024-01-01")), date_labels = "%Y", limits = as.Date(c("2004-01-01", "2024-12-31"))) +
  labs(title = "Popularidade dos Softwares ao Longo do Tempo (2004-2024)",
       x = "Ano",
       y = "Popularidade",
       color = "Software") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Exibir o gráfico
print(p)

# Salvar o gráfico em PDF
ggsave("R_melhor que SPSS.pdf", plot = p, widt
       