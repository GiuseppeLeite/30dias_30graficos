# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar as bibliotecas necessárias
library(ggplot2)
library(tidyr)
library(dplyr)
library(forcats)
library(gganimate)

# Carregar o conjunto de dados
GlobalTemperatures <- read.csv("GlobalTemperatures.csv")

# Verificar a estrutura do conjunto de dados
str(GlobalTemperatures)

# Converter a coluna de datas para o formato Date
GlobalTemperatures$dt <- as.Date(GlobalTemperatures$dt)

# Filtrar dados para o período de 1824 a 2024
GlobalTemperatures <- GlobalTemperatures %>%
  filter(dt >= as.Date("1824-01-01") & dt <= as.Date("2024-12-31"))

# Calcular a média anual de temperatura
GlobalTemperatures <- GlobalTemperatures %>%
  mutate(Year = as.numeric(format(dt, "%Y"))) %>%
  group_by(Year) %>%
  summarize(LandAverageTemperature = mean(LandAverageTemperature, na.rm = TRUE))

# Calcular a média de referência para o período pré-industrial (1850-1900)
preindustrial_mean <- GlobalTemperatures %>%
  filter(Year >= 1850 & Year <= 1900) %>%
  summarize(ReferenceTemperature = mean(LandAverageTemperature, na.rm = TRUE)) %>%
  pull(ReferenceTemperature)

# Calcular a variação em relação à média de referência pré-industrial
GlobalTemperatures <- GlobalTemperatures %>%
  mutate(TemperatureVariation = LandAverageTemperature - preindustrial_mean)

# Definir um tema limpo e cores distinguíveis
clean_theme <- theme_minimal() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "none",
    axis.text = element_text(colour = "black", size = 17), # Aumentar o tamanho dos números dos eixos
    axis.title.x = element_blank(), # Remover o rótulo do eixo x
    axis.title.y = element_text(size = 17, colour = "black"), # Aumentar o tamanho do título do eixo y
    legend.text = element_text(size = 12), # Aumentar o tamanho do texto da legenda
    legend.title = element_text(size = 12), # Aumentar o tamanho do título da legenda
    plot.title = element_text(size = 17, hjust = 0.5), # Centralizar o título e ajustar o tamanho
    plot.margin = margin(t = 20, r = 20, b = 50, l = 20)
  )

# Criar o gráfico animado
my_animation <- GlobalTemperatures %>%
  ggplot(aes(x = Year, y = TemperatureVariation, fill = TemperatureVariation)) +
  geom_col() +
  geom_line(aes(color = ifelse(TemperatureVariation < 0, "negative", "positive")), size = 1) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  scale_color_manual(values = c("negative" = "blue", "positive" = "red")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) + # Linha horizontal no 0
  labs(title = "Variação da Temperatura Global em Relação ao Período Pré-Industrial (1824-2024)", 
       y = "Variação da Temperatura (°C)", fill = "Variação da Temperatura") +
  scale_x_continuous(breaks = seq(1824, 2024, by = 50)) +
  clean_theme +
  transition_reveal(Year)

# Animar e salvar o gráfico
animate(my_animation, height = 9, width = 16, units = "in", res = 200, duration = 20, fps = 20)
anim_save("GlobalTemperatures_Variation_Animation.gif")
