# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar bibliotecas necessárias
library(ggplot2)
library(ggalt)
library(dplyr)

# Carregar os dados
dados <- read.delim("dados.txt")

# Criar a coluna 'change' para representar a mudança de 2009 para 2019
dados$change <- dados$X2019 - dados$X2009

# Criar a coluna 'color' para indicar aumento ou diminuição
dados$color <- ifelse(dados$change > 0, "increase", "decrease")

# Ordenar as capitais com base na mudança, colocando as mudanças positivas no topo
dados <- dados %>%
  arrange(desc(change))

# Criar o gráfico de barras com a mudança
ggplot(dados, aes(x = reorder(Capitais, change), y = change, fill = color)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = round(change, 1)), color = "black", size = 3.5, hjust = ifelse(dados$change > 0, -0.2, 1.2)) +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 14),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 16, hjust = 0.5)
  ) +
  scale_fill_manual(values = c("increase" = "red", "decrease" = "blue")) +
  labs(
    title = "Mudança no Percentual de Estudantes com Aulas de Educação Física (2009 vs 2019)", 
    x = "Capitais", 
    y = "Mudança na Porcentagem"
  )

# Salvar o gráfico em formato PDF
dev.copy2pdf(file = "escolaridade.pdf", width = 10, height = 7)
