# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar bibliotecas necessárias
library(ggplot2)
library(tidyr)
library(dplyr)
library(forcats)
library(gganimate)
library(viridis)

# Carregar o conjunto de dados
dados <- read.csv("vendor-ww-monthly-201003-202406.csv")

# Combinar todos os outros fornecedores em "Other"
vendors_to_keep <- c("Samsung", "Apple", "Nokia", "Xiaomi", "Huawei", "LG", "Motorola", "Sony")
dados$Other <- rowSums(dados[ , !(names(dados) %in% c("Date", vendors_to_keep))])
dados <- dados %>%
  select(Date, all_of(vendors_to_keep), Other)

# Converter a coluna Date para objeto Date
dados$Date <- as.Date(paste0(dados$Date, "-01"), format = "%Y-%m-%d")

# Transformar os dados para o formato longo
dados_long <- gather(dados, Vendor, MarketShare, -Date)

# Remover "Other" dos dados
dados_long <- dados_long %>% filter(Vendor != "Other")

# Definir um tema limpo e cores distinguíveis
clean_theme <- theme_minimal() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "bottom",
    axis.text = element_text(colour = "black", size = 12), # Aumentar o tamanho dos números dos eixos
    axis.title.x = element_blank(), # Remover o rótulo do eixo x
    axis.title.y = element_text(size = 14, colour = "black"), # Aumentar o tamanho do título do eixo y
    legend.text = element_text(size = 12), # Aumentar o tamanho do texto da legenda
    legend.title = element_text(size = 12), # Aumentar o tamanho do título da legenda
    plot.title = element_text(size = 16, hjust = 0.5) # Centralizar o título e ajustar o tamanho
  )

# Escolher uma paleta de cores amigável para daltônicos
color_palette <- c(
  "Apple" = "#144c73",    # Blue
  "LG" = "#ff7f0e",       # Orange
  "Nokia" = "#ffcc00",    # Green
  "Xiaomi" = "#d62728",   # Red
  "Huawei" = "#9467bd",   # Purple
  "Motorola" = "#8c564b", # Brown
  "Samsung" = "#e377c2",  # Pink
  "Sony" = "#595959"      # Grey
)

# Definir os intervalos para o eixo x
x_breaks <- seq(as.Date("2010-01-01"), as.Date("2024-01-01"), by = "2 years")

# Criar o gráfico animado
my_animation <- dados_long %>%
  ggplot(aes(x = Date, y = MarketShare, group = Vendor, color = Vendor)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = color_palette) +
  labs(
    title = "Mercado mundial de fabricantes de celulares", 
    y = "Participação de Mercado (%)", 
    color = "Fabricantes"
  ) +
  scale_x_date(breaks = x_breaks, date_labels = "%Y") +
  clean_theme +
  transition_reveal(Date)

# Animar e salvar o gráfico
animate(my_animation, height = 1080, width = 1920, res = 200, duration = 10, fps = 30)
anim_save("participacao_mercado_animacao.gif")
