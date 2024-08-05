# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar as bibliotecas necessárias
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Carregar os dados
data <- read.csv("data.csv")

# Converter comprimento para numérico e remover valores NA
data$length <- as.numeric(gsub("m", "", data$length))
data <- na.omit(data)

# Criar um vetor de tradução para os tipos de dinossauros
translation <- c(
  "sauropod" = "Saurópode",
  "large theropod" = "Terópode grande",
  "ceratopsian" = "Ceratopsídeo",
  "small theropod" = "Terópode pequeno",
  "armoured dinosaur" = "Dinossauro blindado",
  "euornithopod" = "Euornitópode"
)

# Traduzir a coluna 'type'
data$type_pt <- translation[data$type]

# Definir cores personalizadas amigáveis para daltônicos usando ColorBrewer
custom_colors <- brewer.pal(length(unique(data$type_pt)), "Dark2")

# Criar o gráfico de caixa combinado com pontos jittered e rótulos em português
p <- ggplot(data, aes(x = type_pt, y = length, color = type_pt)) +
  geom_boxplot(color = "black") +  # Box plots em preto
  geom_jitter(width = 0.2, alpha = 0.40) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black", fill = "black") +  # Adicionar pontos da média
  theme_bw() +
  labs(title = "Comprimento dos Dinossauros por Tipo",
       y = "Comprimento (m)",
       x = NULL,  # Remover título do eixo x
       color = "Tipo de Dinossauro") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        plot.title = element_text(color = "black"),
        plot.subtitle = element_text(color = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),  # Remover linhas de grade principais
        panel.grid.minor = element_blank()) +  # Remover linhas de grade secundárias
  scale_color_manual(values = custom_colors)  # Usar cores personalizadas

# Exibir o gráfico
print(p)

# Salvar o gráfico em PDF
ggsave("dino.pdf", plot = p, width = 5, height = 6)
