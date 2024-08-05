# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar as bibliotecas necessárias
library(ggplot2)
library(tidyr)
library(dplyr)

# Dados
dados <- data.frame(
  Pais = c("Estados Unidos", "Reino Unido", "França", "Espanha", "Alemanha", "Canadá", "Portugal", "Itália", "Holanda", "Austrália"),
  Iniciacao_cientifica = c(139, 71, 81, 83, 50, 42, 78, 21, 20, 19),
  Mestrado = c(411, 143, 152, 113, 103, 88, 148, 68, 34, 31),
  Doutorado = c(1620, 473, 440, 296, 308, 306, 193, 163, 151, 121),
  Pos_doutorado = c(764, 279, 196, 174, 172, 122, 66, 60, 65, 49),
  Total = c(2934, 966, 869, 666, 633, 558, 485, 312, 270, 220)
)

# Definir a ordem dos fatores na variável Pais
dados$Pais <- factor(dados$Pais, levels = c("Austrália", "Holanda", "Itália", "Portugal", "Canadá", "Alemanha", "Espanha", "França", "Reino Unido", "Estados Unidos"))

# Converter dados para formato longo
dados_long <- gather(dados, key = "Tipo", value = "Numero", -Pais, -Total)

# Calcular proporções
dados_long <- dados_long %>%
  group_by(Pais) %>%
  mutate(Proporcao = Numero / sum(Numero) * 100)

# Cores personalizadas
cores_personalizadas <- c("Iniciacao_cientifica" = "#004D40", "Mestrado" = "#FFC107", "Doutorado" = "#1E88E5", "Pos_doutorado" = "#D81B60")

# Criar o gráfico de barras empilhadas com porcentagens e rótulos
p <- ggplot(dados_long, aes(x = Pais, y = Proporcao, fill = Tipo)) +
  geom_bar(stat = "identity", alpha = 0.65) +
  geom_text(aes(label = paste0(round(Proporcao, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 5, color = "black") +
  scale_fill_manual(values = cores_personalizadas) +
  coord_flip() +
  labs(title = "Distribuição de Bolsas BEPE por Tipo e País (em %)",
       x = "País",
       y = "Proporção de Bolsas (%)",
       fill = "Tipo de Bolsa") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, hjust = 0.5),
    panel.grid.major = element_blank(),  # Remover linhas de grade principais
    panel.grid.minor = element_blank()   # Remover linhas de grade secundárias
  )

# Exibir o gráfico
print(p)

# Salvar o gráfico em PDF
ggsave("bepe.pdf", plot = p, width = 13, height = 6)
