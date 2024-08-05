# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar bibliotecas necessárias
library(ggplot2)
library(dplyr)
library(forcats)

# Carregar e preparar os dados
dados <- read.csv2("alcool.csv")

# Bubble Plot com Cor Específica
p1 <- ggplot(dados, aes(x = fct_reorder(Pais, Litros), y = Litros, size = Litros)) +
  geom_point(shape = 21, alpha = 0.65, fill = "#657895", color = "black", show.legend = FALSE) +  # Cor personalizada com borda preta
  scale_size(range = c(6, 6)) +  # Ajustar o tamanho das bolhas para serem uniformes
  coord_flip() +  # Inverter os eixos para melhor visualização
  labs(x = "País", y = "Litros per capita", title = "Consumo de Álcool por Pessoa") +  # Adicionar rótulos e título
  theme_minimal(base_size = 14) +  # Aplicar tema minimalista
  theme(
    axis.text.x = element_text(size = 12, color = "black"),  # Ajustar texto do eixo x
    axis.text.y = element_text(size = 12, color = "black"),  # Ajustar texto do eixo y
    axis.title.x = element_text(size = 14, color = "black"),  # Ajustar título do eixo x
    axis.title.y = element_text(size = 14, color = "black"),  # Ajustar título do eixo y
    legend.position = "bottom"  # Posicionar legenda na parte inferior
  ) 

# Adicionar escala do eixo y
p1 <- p1 + scale_y_continuous(limits = c(0, 12))

# Exibir o gráfico
print(p1)

# Salvar o gráfico em formato PDF
ggsave("alcool.pdf", plot = p1, width = 9, height = 10)
