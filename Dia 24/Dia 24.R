# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar pacotes necessários
library(dplyr)
library(ggplot2)
library(tidyr)

# Carregar o dataset
lotr_characters <- read.csv("lotr_characters.csv")

# Definir as raças específicas e suas traduções para português
racas_selecionadas <- c("Men", "Hobbits", "Elves", "Dwarves", "Ainur", "Orcs", "Half-elven", "Dragons")
racas_portugues <- c("Homens", "Hobbits", "Elfos", "Anões", "Ainur", "Orcs", "Meio-elfos", "Dragões")

# Criar um dataframe de tradução
traducao_racas <- data.frame(race = racas_selecionadas, raca_pt = racas_portugues)

# Filtrar e contar personagens masculinos e femininos por raça
contagem_raca_genero <- lotr_characters %>%
  filter(race %in% racas_selecionadas, gender %in% c("Male", "Female")) %>%
  group_by(race, gender) %>%
  summarise(contagem = n(), .groups = 'drop') %>%
  left_join(traducao_racas, by = "race")

# Criar um dataframe completo com combinações de raças e gêneros, preenchendo com 0 onde necessário
contagem_completa <- expand.grid(raca_pt = racas_portugues, gender = c("Male", "Female")) %>%
  left_join(contagem_raca_genero %>% select(raca_pt, gender, contagem), by = c("raca_pt", "gender")) %>%
  mutate(contagem = replace_na(contagem, 0))

# Calcular o total de personagens por raça
total_contagens <- contagem_completa %>%
  group_by(raca_pt) %>%
  summarise(contagem_total = sum(contagem), .groups = 'drop')

# Ordenar as raças de acordo com o total de personagens
contagem_completa <- contagem_completa %>%
  mutate(raca_pt = factor(raca_pt, levels = total_contagens$raca_pt[order(-total_contagens$contagem_total)]))

# Criar o gráfico de barras agrupadas usando ggplot2
p <- ggplot(contagem_completa, aes(x = raca_pt, y = contagem, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black", size = 0.5) +
  scale_fill_manual(values = c("Male" = "#1f78b4", "Female" = "#e31a1c"), labels = c("Masculino", "Feminino")) +  # Cores mais suaves
  labs(title = "Número de Personagens por Gênero e Raça", 
       x = "Raça", 
       y = "Contagem", 
       fill = "Gênero") +
  theme_minimal(base_size = 15) +  # Ajustar o tamanho da fonte
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    plot.title = element_text(color = "black"),
    panel.grid.major = element_blank(),  # Remover grades principais
    panel.grid.minor = element_blank(),  # Remover grades menores
    panel.background = element_rect(fill = "white", color = NA),  # Fundo branco
    plot.background = element_rect(fill = "white", color = NA)  # Fundo branco
  ) +
  geom_text(aes(label = contagem), vjust = -0.5, position = position_dodge(0.9), size = 4.5, color = "black")  # Adicionar rótulos de dados com cor preta

# Exibir o gráfico
print(p)

# Salvar o gráfico em PDF
ggsave("LOTR.pdf", plot = p, width = 10, height = 8)
