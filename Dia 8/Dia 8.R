# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar bibliotecas necessárias
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Carregar os dados de geração de eletricidade por fonte no mundo
data <- read.csv("Electricity_generation_by_source_World.csv", sep=";")

# Verificar o tipo de dado e converter para data frame se necessário
typeof(data)
data <- as.data.frame(data)

# Filtrar os dados para os anos de 2001 e 2021
data_filtered <- data %>% filter(Year %in% c(2001, 2021))

# Transformar os dados para formato longo
data_long <- data_filtered %>%
  tidyr::pivot_longer(cols = -Year, names_to = "Source", values_to = "Value")

# Criar uma paleta de cores personalizada
color_palette <- c("Biofuels" = "#FFB3BA", "Coal" = "#FFDFBA", "Geothermal" = "#FFFFBA",
                   "Hydro" = "#BAFFC9", "Natural.gas" = "#BAE1FF", "Nuclear" = "#B0E0E6",
                   "Oil" = "#D3B0E6", "Other.sources" = "#FFB3E6", "Solar.PV" = "#FFC3A0",
                   "Solar.thermal" = "#FFEFBA", "Tide" = "#BAE1FF", "Wind" = "#C3B0E6")

# Criar o gráfico de rosca para 2001
data_2001 <- data_long %>% filter(Year == 2001)

p1 <- ggplot(data_2001, aes(x = 2, y = Value, fill = Source)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Value, 1), "%")), position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = color_palette) +
  theme_void() +
  xlim(0.5, 2.5) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 14)) +
  ggtitle("Geração de Eletricidade por Fonte em 2001") +
  labs(fill = "Fonte de Energia")

# Criar o gráfico de rosca para 2021
data_2021 <- data_long %>% filter(Year == 2021)

p2 <- ggplot(data_2021, aes(x = 2, y = Value, fill = Source)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Value, 1), "%")), position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = color_palette) +
  theme_void() +
  xlim(0.5, 2.5) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 14)) +
  ggtitle("Geração de Eletricidade por Fonte em 2021") +
  labs(fill = "Fonte de Energia")

# Arranjar os gráficos lado a lado
grid.arrange(p1, p2, ncol = 2)

# Salvar os gráficos em formato PDF
dev.copy2pdf(file="circular.pdf", width = 10, height = 7)
