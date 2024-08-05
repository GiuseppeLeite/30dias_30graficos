library(tidyverse)
library(RColorBrewer)

# Carregar os dados
dados <- read.csv("renewable-share-energy.csv")

# Listar os países do G7 mais o Brasil e Holanda, e seus nomes em português
paises_g7_brazil_netherlands <- c("Brazil", "Canada", "France", "Germany", "Italy", "Japan", "United Kingdom", "United States", "Netherlands")
paises_portugues <- c("Brasil", "Canadá", "França", "Alemanha", "Itália", "Japão", "Reino Unido", "Estados Unidos", "Holanda")

# Criar um dataframe de mapeamento de nomes
mapa_paises <- data.frame(Entity = paises_g7_brazil_netherlands, NomePortugues = paises_portugues)

# Filtrar os dados dos países do G7 mais o Brasil e Holanda
dados_filtrados <- subset(dados, Entity %in% paises_g7_brazil_netherlands)

# Filtrar os dados a partir de 1990
dados_filtrados <- subset(dados_filtrados, Year >= 1993)

# Adicionar os nomes em português
dados_filtrados <- merge(dados_filtrados, mapa_paises, by = "Entity")

# Definir a paleta de cores Paired do RColorBrewer
cores <- brewer.pal(n = length(paises_portugues), name = "Paired")
names(cores) <- paises_portugues

# Criar o gráfico
ggplot(dados_filtrados, aes(x = Year, y = Renewables.....electricity, color = NomePortugues)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = seq(1993, max(dados_filtrados$Year), by = 10)) +
  scale_color_manual(values = cores) +
  theme_minimal(base_size = 15) +  # Aumenta a base do tamanho da fonte
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    text = element_text(color = "white"),
    axis.title = element_text(color = "white", size = 18),
    axis.text = element_text(color = "white", size = 15),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white", size = 15),
    legend.title = element_text(color = "white", size = 18),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16)
  ) +
  labs(title = "Energia Renovável ao Longo dos Anos (a partir de 1990)",
       subtitle = "Participação de Energia Renovável por País",
       x = "Ano",
       y = "Energia Renovável (Equivalente Primário)",
       color = "País")

# Salvar o gráfico
ggsave("energia_renovavel.pdf", width = 10, height = 8)

brewer.pal(12, "Paired")
"#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C" "#FDBF6F" "#FF7F00" "#CAB2D6" "#6A3D9A" "#FFFF99" "#B15928"