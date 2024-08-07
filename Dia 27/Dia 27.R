# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar pacotes necessários
library(tidyverse)

# Dados por sexo, idade, escolaridade e renda
dados_outros <- data.frame(
  Categoria = c("Total", "Masculino", "Feminino", "16 a 24 anos", "25 a 44 anos", "45 a 59 anos", "60 anos ou +",
                "Fundamental", "Médio", "Superior", "Sem renda fixa", "Até 2 SM", "Entre 2 e 5 SM", "Entre 5 e 10 SM", "+ de 10 SM"),
  OtimoBom = c(45, 49, 42, 54, 33, 53, 58, 51, 43, 25, 48, 39, 50, 54, 44),
  Regular = c(41, 43, 40, 39, 54, 31, 26, 34, 45, 61, 41, 44, 45, 43, 31),
  RuimPessimo = c(9, 7, 10, 5, 6, 10, 15, 7, 9, 12, 9, 11, 4, 1, 13),
  NaoSabe = c(5, 1, 8, 2, 7, 6, 1, 7, 2, 2, 2, 5, 1, 1, 13)
)

# Verificar a soma dos percentuais e ajustar se necessário
dados_outros <- dados_outros %>%
  mutate(Total = OtimoBom + Regular + RuimPessimo + NaoSabe) %>%
  mutate(
    OtimoBom = OtimoBom / Total * 100,
    Regular = Regular / Total * 100,
    RuimPessimo = RuimPessimo / Total * 100,
    NaoSabe = NaoSabe / Total * 100
  ) %>%
  select(-Total)

# Transformar dados para formato longo
dados_outros_long <- dados_outros %>%
  pivot_longer(cols = -Categoria, names_to = "Resposta", values_to = "Percentual")

# Ajustar a ordem dos níveis do fator Resposta
dados_outros_long$Resposta <- factor(dados_outros_long$Resposta, levels = c("OtimoBom", "Regular", "RuimPessimo", "NaoSabe"))

# Adicionar grupos de categorias
dados_outros_long <- dados_outros_long %>%
  mutate(Grupo = case_when(
    Categoria %in% c("Total", "Masculino", "Feminino") ~ "Sexo",
    Categoria %in% c("16 a 24 anos", "25 a 44 anos", "45 a 59 anos", "60 anos ou +") ~ "Idade",
    Categoria %in% c("Fundamental", "Médio", "Superior") ~ "Escolaridade",
    Categoria %in% c("Sem renda fixa", "Até 2 SM", "Entre 2 e 5 SM", "Entre 5 e 10 SM", "+ de 10 SM") ~ "Renda",
    TRUE ~ "Outros"
  ))

# Ordenar as categorias conforme a imagem
ordem_categorias <- c("Total", "Masculino", "Feminino", "16 a 24 anos", "25 a 44 anos", "45 a 59 anos", "60 anos ou +",
                      "Fundamental", "Médio", "Superior", "Sem renda fixa", "Até 2 SM", "Entre 2 e 5 SM", "Entre 5 e 10 SM", "+ de 10 SM")

dados_outros_long$Categoria <- factor(dados_outros_long$Categoria, levels = ordem_categorias)

# Gráfico de barras empilhadas por sexo, idade, escolaridade e renda com layout ajustado
grafico_outros_empilhado <- ggplot(dados_outros_long, aes(y = Categoria, x = Percentual, fill = Resposta, label = round(Percentual, 1))) +
  geom_bar(stat = "identity", color = "black", alpha = 0.55) +
  geom_text(position = position_stack(vjust = 0.5), size = 4) +
  facet_grid(Grupo ~ ., scales = "free", space = "free") +
  scale_fill_manual(values = c("OtimoBom" = "#1d578f", "Regular" = "white", "RuimPessimo" = "#b20000", "NaoSabe" = "#a6a6a6")) +
  labs(title = "Avaliação do atendimento do SUS", x = "Percentual", y = "Categoria", fill = "Resposta") +
  scale_x_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 14),
    axis.title.y = element_text(color = "black", size = 14),
    strip.background = element_rect(fill = "#fdf9f5"),
    strip.text = element_text(color = "black", face = "bold"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#fdf9f5", color = NA),
    plot.background = element_rect(fill = "#fdf9f5", color = NA)
  )

# Exibir gráfico de barras empilhadas
print(grafico_outros_empilhado)

# Salvar o gráfico em PDF
ggsave("grafico_combined.pdf", plot = grafico_outros_empilhado, width = 10, height = 6)
