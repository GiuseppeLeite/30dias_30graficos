# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar as bibliotecas necessárias
library(ggplot2)
library(plotly)

# Ler o arquivo CSV com informações do Modern Family
modern_family_info <- read.csv("modern_family_info.csv")

# Converter a coluna Airdate para o formato de data
modern_family_info$Airdate <- as.Date(modern_family_info$Airdate)

# Crie um vetor com a ordem desejada para as temporadas
season_order <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")

# Defina os limites do eixo X
min_date <- min(modern_family_info$Airdate)
max_date <- max(modern_family_info$Airdate)

# Crie o gráfico de bolhas usando ggplot2
p <- ggplot(modern_family_info, aes(x = Airdate, 
                                    y = Rating, 
                                    size = Total.Votes, 
                                    fill = as.factor(gsub("S([0-9]+)-E[0-9]+", "\\1", Season.Episode)))) +
  # Adicionar os pontos (bolhas) ao gráfico
  geom_point(alpha = 0.85, shape = 21) +
  # Configurar a escala de tamanho das bolhas e inverter a ordem (maior para menor)
  scale_size_continuous(range = c(3, 20), name = "Total de Votos") +
  # Configurar a escala de preenchimento (cores) para as temporadas e ordenar corretamente
  scale_fill_manual(name = "Temporada", values = scales::hue_pal()(11), breaks = season_order) +
  # Definir os rótulos dos eixos e o título do gráfico
  labs(x = "Data de Exibição", y = "Classificação (IMDb)", title = "Classificações e Votos dos Episódios de Modern Family") +
  # Configurar a escala do eixo X para exibir as datas em intervalos de 1 ano e ajustar os limites
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(as.Date("2009-01-01"), as.Date("2020-12-31")), 
               expand = expansion(mult = c(0.01, 0.01))) +
  # Aplicar um tema minimalista ao gráfico
  theme_minimal() +
  # Personalizar a aparência dos elementos do gráfico
  theme(plot.title = element_text(size = rel(2)),
        panel.grid.major.x = element_line(color = "grey", linewidth = 0.3),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 20, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title.x = element_text(size = 20, color = "black"),
        axis.title.y = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"))

# Exibir o gráfico
print(p)

# Salvar o gráfico em formato PDF
dev.copy2pdf(file="familia.pdf", width = 14, height = 10)
