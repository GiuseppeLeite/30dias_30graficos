# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(ggstar)

# Ler dados
data <- read.csv("average-years-of-schooling-vs-gdp-per-capita.csv", sep = ";")

# Remover linhas com valores NA em variáveis de interesse
data <- data %>% filter(!is.na(average_years_of_education) & !is.na(GDP) & !is.na(Population))

# Calcular correlação de Spearman
spearman_cor <- cor.test(data$average_years_of_education, data$GDP, method = "spearman")
correlation_label <- paste("Spearman's ρ =", round(spearman_cor$estimate, 2))

# Identificar os top 3 maiores PIBs
top_3_gdp <- data %>% arrange(desc(GDP)) %>% head(3)

# Identificar os top 3 menores PIBs
bottom_3_gdp <- data %>% arrange(GDP) %>% head(3)

# Identificar o Brasil
brazil <- data %>% filter(Entity == "Brazil")

# Combinar todas as observações a serem rotuladas
to_label <- bind_rows(top_3_gdp, bottom_3_gdp, brazil)

# Vetor de tradução dos nomes dos países para português do Brasil
country_translation <- c(
  "Afghanistan" = "Afeganistão",
  "Albania" = "Albânia",
  "Argentina" = "Argentina",
  "Brazil" = "Brasil",
  "Canada" = "Canadá",
  "China" = "China",
  "Germany" = "Alemanha",
  "India" = "Índia",
  "Japan" = "Japão",
  "Luxembourg" = "Luxemburgo",
  "Mexico" = "México",
  "Pakistan" = "Paquistão",
  "Russia" = "Rússia",
  "Saudi Arabia" = "Arábia Saudita",
  "United States" = "Estados Unidos"
  # Adicione mais traduções conforme necessário
)

# Aplicar tradução aos países selecionados
to_label$Entity_pt <- country_translation[to_label$Entity]
to_label$Entity_pt[is.na(to_label$Entity_pt)] <- to_label$Entity[is.na(to_label$Entity_pt)]

# Criar gráfico
d <- ggplot(data, aes(x = GDP, y = average_years_of_education)) +
  ggstar::geom_star(aes(size = Population), starshape = "hexagon", fill = "#084d6e", color = "white", alpha = 0.80) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Adicionar linha de tendência
  geom_text(data = to_label, aes(label = Entity_pt), color = "black", size = 3, vjust = -0.5, check_overlap = TRUE) +
  annotate("text", x = 2000, y = 10, label = correlation_label, color = "red", size = 5, hjust = 0) +  # Adicionar correlação
  scale_size_continuous(name = "População", range = c(4, 10)) +  # Ajustar o tamanho mínimo e máximo
  scale_x_continuous(name = "PIB per Capita (US$)", trans = "log10", breaks = c(1000, 2000, 5000, 10000, 20000, 50000, 100000), labels = scales::dollar_format(prefix = "$", big.mark = ",")) +  # Definir os intervalos específicos
  scale_y_continuous(name = "Anos Médios de Educação", breaks = c(0, 2, 4, 6, 8, 10, 12)) +  # Definir os intervalos específicos
  labs(
    title = "Relação entre Anos Médios de Educação e PIB per Capita",
    y = "Anos Médios de Educação",
    caption = "Fonte: OurWorldInData.org"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),
    axis.title.x = element_text(size = 15, color = "black"),
    axis.title.y = element_text(size = 15, color = "black"),
    axis.text = element_text(size = 15, color = "black"),
    legend.position = "bottom",
    panel.grid.major = element_blank(),  # Remover as grades principais
    panel.grid.minor = element_blank()   # Remover as grades menores
  )

# Exibir gráfico
print(d)

# Salvar o gráfico em PDF
ggsave("educacao.pdf", plot = d, width = 10, height = 8)
