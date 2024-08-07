# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar bibliotecas necessárias
library(rvest)
library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)

# Definir a URL da página da Wikipedia que contém a lista de filmes em preto e branco produzidos desde 1966
url <- "https://en.wikipedia.org/wiki/List_of_black-and-white_films_produced_since_1966"

# Ler o conteúdo HTML da URL
page <- read_html(url)

# Extrair as tabelas da página
tables <- page %>% html_nodes("table.wikitable")

# Loop para extrair os dados de cada tabela
film_data <- lapply(tables, function(table) {
  data <- table %>% html_table(fill = TRUE) # Preencher valores ausentes na tabela
  return(data)
})

# Combinar os dados de todas as tabelas em um único data frame
film_list <- bind_rows(film_data)

# Renomear a coluna 'Film' para 'primaryTitle'
names(film_list)[names(film_list) == "Film"] <- "primaryTitle"

# Ler o arquivo title.basics.tsv
title.basics <- read.delim2("title.basics.tsv")

# Filtrar os dados onde titleType é "movie" ou "short"
title.basics <- title.basics[title.basics$titleType %in% c("movie", "short"), ]

# Mesclar os datasets film_list e title.basics pela coluna 'primaryTitle'
dados <- merge(film_list, title.basics, by = "primaryTitle", all = FALSE)

# Converter a coluna 'startYear' para numérica
dados$startYear <- as.numeric(dados$startYear)

# Filtrar os dados onde Year é igual a startYear
dados_filtrados <- subset(dados, Year == startYear)

# Remover duplicatas em 'primaryTitle'
dados_unicos <- dados_filtrados[!duplicated(dados_filtrados$primaryTitle), ]

# Ler o arquivo title.ratings.tsv
title.ratings <- read.delim("title.ratings.tsv")

# Mesclar os datasets title.ratings e dados_unicos pela coluna 'tconst'
dados_unicos <- merge(title.ratings, dados_unicos, by = "tconst", all = FALSE)

# Criar a coluna 'decada'
dados_unicos$decada <- paste0(floor(dados_unicos$Year / 10) * 10, "s")

# Encontrar o maior rating de cada década
max_rating <- dados_unicos %>%
  group_by(decada) %>%
  summarise(max_rating = max(averageRating),
            title = primaryTitle[which.max(averageRating)]) %>%
  ungroup()

# Mesclar os dados para destacar os maiores ratings
dados_unicos <- dados_unicos %>%
  left_join(max_rating, by = "decada") %>%
  mutate(highlight = ifelse(averageRating == max_rating, "highlight", "normal"))

# Criar o gráfico com jitter e fundo cinza
p <- ggplot(dados_unicos, aes(x = averageRating, y = decada)) +
  geom_jitter(aes(fill = highlight), size = 5, height = 0.2, shape = 21, color = "white") +
  geom_text_repel(data = subset(dados_unicos, highlight == "highlight"), 
                  aes(label = paste(title, "(", round(averageRating, 1), ")")),
                  nudge_x = 0.5, 
                  nudge_y = 0.5,
                  color = "black",
                  size = 4) +
  scale_fill_manual(values = c("highlight" = "#190000", "normal" = "#737373")) +
  labs(title = "Black & White Movies",
       subtitle = "Highest rated movie per decade highlighted in red",
       x = "Rating",
       y = "Decade") +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#f2f2f2"),
    panel.background = element_rect(fill = "#f2f2f2"),
    panel.grid.major = element_line(color = "black"),
    panel.grid.minor = element_line(color = "black"),
    text = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(size = 14),
    legend.background = element_rect(fill = "#f2f2f2"),
    legend.key = element_rect(fill = "#f2f2f2")
  )

# Exibir o gráfico
print(p)

# Salvar o gráfico em PDF
ggsave("preto e branco.pdf", plot = p, width = 10, height = 6)
