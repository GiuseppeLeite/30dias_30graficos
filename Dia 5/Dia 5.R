# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar pacotes necessários
library(readxl)
library(tidyverse)
library(ggplot2)

# Carregar os dados de casamentos e divórcios em 2022
dados <- read_excel("dados_casamento_divorcio_2022.xlsx")

# Mostrar a estrutura do dataset para entender suas variáveis
str(dados)

# Preparação dos dados para o gráfico divergente
dados_long <- dados %>%
  select(Estado, Casamentos_Total, Divórcios_Total, Razao) %>%  # Selecionar colunas relevantes
  pivot_longer(cols = c(Casamentos_Total, Divórcios_Total), names_to = "Tipo", values_to = "Total") %>%  # Transformar dados para formato longo
  mutate(Total = ifelse(Tipo == "Casamentos_Total", -Total, Total),  # Negativar valores de casamentos
         Tipo = factor(Tipo, levels = c("Casamentos_Total", "Divórcios_Total")))  # Converter Tipo para fator

# Manter a mesma ordem dos estados baseada na razão
dados_long <- dados_long %>%
  arrange(Razao, Estado) %>%  # Ordenar por razão e estado
  mutate(Estado = factor(Estado, levels = unique(Estado)))  # Definir ordem dos fatores para Estado

# Criar o gráfico divergente
p1 <- ggplot() +
  geom_bar(data = dados_long, aes(x = Total, y = Estado, fill = Tipo), stat = "identity", position = "stack") +  # Gráfico de barras empilhadas
  geom_text(data = dados, aes(x = 0, y = Estado, label = round(Razao, 2)), vjust = 0.4, size = 4, color = "black") +  # Adicionar texto das razões
  scale_x_continuous(labels = abs, breaks = seq(-250000, 250000, by = 50000), limits = c(-250000, 250000)) +  # Ajustar escala do eixo x
  scale_fill_manual(values = c("Casamentos_Total" = "#c03647ff", "Divórcios_Total" = "#368cc0ff")) +  # Definir cores manuais para a legenda
  labs(x = "Total de Casamentos/Divórcios", y = "Estado", fill = "Tipo") +  # Adicionar rótulos dos eixos e legenda
  theme_bw() +  # Usar o tema de fundo branco
  theme(
    axis.line = element_line(color = 'black', linewidth = 0.01),  # Cor e espessura das linhas dos eixos
    plot.background = element_blank(),  # Remover o fundo do gráfico
    axis.text.x = element_text(colour = "black", size = 11),  # Cor e tamanho do texto do eixo x
    axis.text.y = element_text(colour = "black", size = 13),  # Cor e tamanho do texto do eixo y
    panel.grid.minor = element_blank(),  # Remover grades menores
    panel.grid.major = element_blank(),  # Remover grades maiores
    axis.title.x = element_blank(),  # Remover o título do eixo x
    legend.position = "bottom"  # Posicionar a legenda na parte inferior
  )

# Exibir o gráfico
p1

# Salvar o gráfico em formato PDF
ggsave("divergente.pdf", plot = p1, width = 9, height = 6)
