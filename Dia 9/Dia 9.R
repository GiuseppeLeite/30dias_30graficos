# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

#Carregar pacotes necessários
library(ggplot2)

# Criar um data frame com nomes, tamanhos e cores dos dragões de House of the Dragon
hotd_dragons <- data.frame(
  Name = c("Balerion", "Vhagar", "Vermithor", "Sheepstealer", "Silverwing",
           "Dreamfyre", "Caraxes", "Meleys", "Syrax", "Sunfyre", "Tessarion",
           "Seasmoke", "Vermax", "Arrax", "Tyraxes", "Stormcloud", "Morghul", "Moondancer"),
  Size = c(76, 65, 57, 50, 47, 45, 43, 42, 40, 34, 33, 32, 30, 28, 26, 22, 20, 16),
  Color = c("#000000", "#228B22", "#CD7F32", "#8B4513", "#C0C0C0", "#ADD8E6", 
            "#FF4500", "#FF0000", "#FFD700", "#FFD700", "#0000FF", "#D3D3D3", 
            "#32CD32", "#98FB98", "#006400", "#2F4F4F", "#8B4513", "#ADFF2F")
)

# Criar o gráfico de barras horizontais com uma única linha horizontal no eixo X
ggplot(hotd_dragons, aes(x = reorder(Name, Size), y = Size, fill = Name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = setNames(hotd_dragons$Color, hotd_dragons$Name)) +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remover legenda
    axis.text.y = element_text(color = "black", size = 12, hjust = 1),  # Texto do eixo y
    axis.text.x = element_text(color = "black", size = 12),  # Texto do eixo x
    axis.title.x = element_text(color = "black", size = 14),  # Título do eixo x
    axis.title.y = element_text(color = "black", size = 14),  # Título do eixo y
    plot.title = element_text(color = "black", size = 14, face = "bold"),  # Título do gráfico
    legend.title = element_text(color = "black", size = 12),  # Título da legenda
    legend.text = element_text(color = "black", size = 10),  # Texto da legenda
    panel.grid = element_blank(),  # Remover todas as linhas de grade
    axis.line.x = element_line(color = "black", size = 0.5)  # Manter a linha do eixo X
  ) +
  labs(
    title = "Size of Dragons in House of the Dragon",
    x = "Dragon Name",
    y = "Size in Meters",
    fill = "Dragon"
  )

# Salvar o gráfico em formato PDF
dev.copy2pdf(file = "casa_do.pdf", width = 10, height = 7)
