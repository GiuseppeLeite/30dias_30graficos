# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar pacotes necessários
library(pheatmap)

# Carregar os dados
dados <- read.delim("dados.txt", row.names = 1)

# Verificar a estrutura dos dados
str(dados)

# Criar o primeiro heatmap com normalização por coluna
pheatmap(
  dados,
  color = colorRampPalette(c("#1111fd", "white", "red"))(50),
  cluster_rows = FALSE,   # Desabilitar o clustering das linhas
  cluster_cols = TRUE,    # Habilitar o clustering das colunas
  scale = "column",       # Normalizar os dados por coluna
  show_rownames = TRUE,   # Mostrar os nomes das linhas (genes)
  show_colnames = TRUE    # Mostrar os nomes das colunas (amostras)
)

# Salvar o primeiro heatmap em PDF
dev.copy2pdf(file = "heatmap_alive_HVs.pdf", width = 6, height = 6)

# Criar o segundo heatmap com normalização por linha
pheatmap(
  dados,
  color = colorRampPalette(c("#1111fd", "white", "red"))(50),
  cluster_rows = FALSE,   # Desabilitar o clustering das linhas
  cluster_cols = TRUE,    # Habilitar o clustering das colunas
  scale = "row",          # Normalizar os dados por linha
  show_rownames = TRUE,   # Mostrar os nomes das linhas (genes)
  show_colnames = TRUE    # Mostrar os nomes das colunas (amostras)
)

# Salvar o segundo heatmap em PDF
dev.copy2pdf(file = "heatmap_alive2_HVs.pdf", width = 6, height = 6)
