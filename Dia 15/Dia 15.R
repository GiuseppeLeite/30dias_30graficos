# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar as bibliotecas necessárias
library(ggplot2)
library(gganimate)
library(tidyverse)

# Carregar o conjunto de dados
PubMed_Timeline_Results_by_Year <- read.csv("PubMed_Timeline_Results_by_Year.csv", comment.char = "#")

# Verificar a estrutura do conjunto de dados
str(PubMed_Timeline_Results_by_Year)

# Criar o gráfico lollipop com melhorias na visualização
p <- ggplot(PubMed_Timeline_Results_by_Year, aes(x = Year, y = Count)) +
  geom_segment(aes(xend = Year, yend = 0), color = "#9eb9e0") +
  geom_point(size = 3, color = "#0d50b2") +
  labs(title = "Publicações com o termo: infecção, no PubMed de 1944 a 2024", 
       x = "Ano", 
       y = "Número de artigos") +
  theme_bw() +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 300000, by = 50000)) +  # Melhorias nos rótulos do eixo y
  scale_x_continuous(limits = c(1944, 2024), breaks = seq(1944, 2024, by = 10)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black", size = 10),  # Rótulos do eixo x na horizontal
    axis.text.y = element_text(color = "black", size = 10),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 14, face = "bold", color = "black")
  )

# Exibir o gráfico
print(p)

# Salvar o gráfico em formato PDF
dev.copy2pdf(file = "PubMed_Publications_Lollipop.pdf", width = 10, height = 6)
