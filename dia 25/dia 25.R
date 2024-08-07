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
treecover_loss_by_region__ha <- read.csv("treecover_loss_by_region__ha.csv")
adm1_metadata <- read.csv("adm1_metadata.csv", sep=";")

# Mesclar os dados
data <- merge(treecover_loss_by_region__ha, adm1_metadata, by = "adm1")

# Filtrar os dados para os estados do norte do Brasil
norte_states <- c("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins")
data_norte <- data %>% filter(name %in% norte_states)

# Converter o campo de ano para o formato Date
data_norte$umd_tree_cover_loss__year <- as.Date(paste0(data_norte$umd_tree_cover_loss__year, "-01-01"))

# Definir a paleta de cores para as regiões
unique_names <- unique(data_norte$name)
color_palette <- setNames(colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(length(unique_names)), unique_names)

# Criar o gráfico animado
my_animation <- data_norte %>%
  ggplot(aes(x = umd_tree_cover_loss__year, y = umd_tree_cover_loss__ha, group = name, color = name)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = color_palette) +
  labs(
    title = "Perda de Cobertura Florestal ao Longo dos Anos nos Estados do Norte do Brasil", 
    y = "Perda de Cobertura Florestal (ha)", 
    color = "Regiões"
  ) +
  scale_x_date(date_labels = "%Y") +
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
  transition_reveal(umd_tree_cover_loss__year)

# Salvar a animação
animate(my_animation, height = 1080, width = 1080, res = 200, duration = 1.5, fps = 100)
anim_save("participacao_mercado_animacao.gif")
