# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar pacotes necessários
library(ggplot2)
library(dplyr)

# Carregar o dataset Titanic
titanic_data <- read.csv("https://web.stanford.edu/class/archive/cs/cs109/cs109.1166/stuff/titanic.csv")

# Mostrar a estrutura do dataset para entender suas variáveis
str(titanic_data)

# Definir os pontos de corte para cada grupo de idade
pontos_corte <- c(0, 12, 18, 60, Inf)

# Definir os rótulos para cada grupo de idade
rotulos <- c("Criança", "Adolescente", "Adulto", "Idoso")

# Criar a coluna grupo_idade utilizando cut() e mutate() do pacote dplyr
titanic_data <- titanic_data %>%
  mutate(grupo_idade = cut(Age, breaks = pontos_corte, labels = rotulos, right = FALSE))

# Definir cores para sobreviventes e não sobreviventes
cores_survived <- c("0" = "black", "1" = "white")

# Criar gráfico de barra empilhada - Taxa de sobrevivência por classe e gênero
grafico1 <- ggplot(titanic_data, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(position = "fill", color = "black") +  # Gráfico de barras empilhadas, normalizando as proporções
  facet_wrap(~ Sex) +  # Dividir o gráfico por gênero
  labs(y = "Proporção", fill = "Sobreviveu") +  # Rótulos dos eixos e legenda
  scale_fill_manual(values = cores_survived) +  # Definir cores manuais para a legenda
  theme_minimal() +  # Aplicar tema minimalista ao gráfico
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        text = element_text(size = 14)) +
  ggtitle("Taxa de Sobrevivência por Classe e Gênero")  # Título do gráfico
grafico1

# Definir uma paleta de cores para as classes
cores_classes <- c("1" = "#FFD700", "2" = "#C0C0C0", "3" = "#CD7F32")

# Calcular a porcentagem de sobrevivência por grupo de idade e classe
survival_by_age_class <- titanic_data %>%
  group_by(grupo_idade, Pclass) %>%
  summarize(Survived = mean(Survived) * 100)

# Criar gráfico de barras com facetas - Porcentagem de sobrevivência por grupo de idade e classe
grafico2 <- ggplot(survival_by_age_class, aes(x = Pclass, y = Survived, fill = as.factor(Pclass))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # Gráfico de barras lado a lado
  facet_wrap(~ grupo_idade) +  # Dividir o gráfico por grupos de idade
  labs(y = "Porcentagem de Sobrevivência", x = "Classe", fill = "Classe") +  # Rótulos dos eixos e legenda
  scale_fill_manual(values = cores_classes) +  # Definir cores manuais para a legenda
  theme_minimal() +  # Aplicar tema minimalista ao gráfico
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        text = element_text(size = 14)) +
  ggtitle("Porcentagem de Sobrevivência por Classe e Grupo de Idade")  # Título do gráfico
grafico2

# Salvar os gráficos em formato PDF
ggsave("grafico1.pdf", plot = grafico1, width = 5, height = 5)
ggsave("grafico2.pdf", plot = grafico2, width = 5, height = 4)
