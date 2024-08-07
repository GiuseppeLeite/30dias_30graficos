# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar bibliotecas necessárias
library(ggplot2)
library(tidyr)
library(dplyr)
library(forcats)
library(gganimate)

# Carregar os dados
data <- read.csv("cumulative-number-of-large-scale-ai-systems-by-country.csv")
str(data)

# Filtrar fora a entidade "total" e selecionar apenas os países especificados
selected_countries <- c("United States", "China", "United Kingdom", "Multinational", "Israel", "France", "United Arab Emirates", "Germany")
data <- data %>% filter(Entity %in% selected_countries)

# Verificar a estrutura dos dados
head(data)

# Definir a paleta de cores neon para os países
neon_colors <- c(
  "United States" = "#FF3131",        # Red
  "China" = "#FFF01F",                # Yellow
  "United Kingdom" = "#9ed161",       # Fuchsia
  "Multinational" = "#0FF0FC",        # Aqua
  "Israel" = "#ffaa00",               # Orange
  "France" = "#8A2BE2",               # BlueViolet
  "United Arab Emirates" = "#f5f5dc", # LightSeaGreen
  "Germany" = "#FF1493"               # DeepPink
)

# Criar o gráfico animado
my_animation <- data %>%
  ggplot(aes(x = Year, y = Cumulative.number.of.large.scale.AI.systems.by.country, group = Entity, color = Entity)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = neon_colors) +
  labs(
    title = "Número Cumulativo de Sistemas de IA de Grande Escala por País", 
    y = "Número Cumulativo de Sistemas de IA", 
    color = "País"
  ) +
  scale_x_continuous(breaks = seq(min(data$Year), max(data$Year), by = 1)) +
  theme_minimal(base_family = "Arial", base_size = 15) +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_line(color = "gray"),
    text = element_text(color = "white"),
    axis.text.x = element_text(color = "white"),
    axis.title.x = element_blank(),
    axis.text.y = element_text(color = "white"),
    axis.title.y = element_text(color = "white"),
    plot.title = element_text(color = "white"),
    legend.background = element_rect(fill = "black"),
    legend.key = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  ) +
  transition_reveal(Year)

# Salvar a animação
animate(my_animation, height = 1080, width = 1920, res = 200, duration = 7, fps = 30)
anim_save("participacao_mercado_animacao.gif")
