# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregando bibliotecas necessárias
library(ggplot2)
library(scales)

# Importação de dados
world_data <- read.delim("world_data.txt")

# Transformação de Dados
# Convertendo as colunas de Deaths (Mortes) e Births (Nascimentos) para milhões
world_data$Deaths_Millions <- world_data$Deaths / 1e6
world_data$Births_Millions <- world_data$Births / 1e6

# Criação de Gráfico
ggplot(world_data, aes(x = Year)) +
  # Adicionando faixa (ribbon) entre Deaths e Births para o período de 1059 a 2086
  geom_ribbon(data = subset(world_data, Year >= 1059 & Year <= 2086), 
              aes(ymin = pmin(Deaths_Millions, Births_Millions), 
                  ymax = pmax(Deaths_Millions, Births_Millions)), 
              fill = "#eeeeff", alpha = 0.5) +
  # Adicionando faixa (ribbon) entre Deaths e Births para o período de 2086 a 2100
  geom_ribbon(data = subset(world_data, Year > 2086 & Year <= 2100), 
              aes(ymin = pmin(Deaths_Millions, Births_Millions), 
                  ymax = pmax(Deaths_Millions, Births_Millions)), 
              fill = "#ff9c9c", alpha = 0.5) +
  # Adicionando linhas para Deaths e Births
  geom_line(aes(y = Deaths_Millions, color = "Deaths", linetype = Projected), size = 1) +
  geom_line(aes(y = Births_Millions, color = "Births", linetype = Projected), size = 1) +
  # Adicionando linha vertical no ano de 2086
  geom_vline(xintercept = 2086, linetype = "dotted", color = "black", size = 0.55) +
  # Configurando os eixos y com um eixo secundário para Births
  scale_y_continuous(
    name = "Deaths (Millions)",
    sec.axis = sec_axis(~ . * 1e6, name = "Births (Millions)")
  ) +
  # Definindo cores personalizadas para as linhas
  scale_color_manual(
    values = c("Deaths" = "#e20000", "Births" = "#5b5bff")
  ) +
  # Definindo estilos de linha personalizados para projeções
  scale_linetype_manual(
    name = "Projected",
    values = c("no" = "solid", "yes" = "dotted"),
    labels = c("no" = "Not Projected", "yes" = "Projected")
  ) +
  # Aplicando um tema clássico ao gráfico
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 11)
  ) +
  # Adicionando título e rótulos ao gráfico
  labs(
    title = "Global Births and Deaths Over Time",
    x = "Year",
    color = "Legend",
    linetype = "Projection"
  ) +
  # Configurando os intervalos do eixo x
  scale_x_continuous(breaks = seq(min(world_data$Year), max(world_data$Year), by = 10))

# Salvando o gráfico como um arquivo PDF
ggsave("dia_02.pdf", width = 10, height = 6)
