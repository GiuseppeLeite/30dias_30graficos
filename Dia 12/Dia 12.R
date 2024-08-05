# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar os pacotes necessários
library(ggplot2)
library(dplyr)
library(circlize)

# Carregar os dados
HPCharactersData <- read.csv("HPCharactersData.csv")

# Filtrar as colunas relevantes
HPCharactersFiltered <- HPCharactersData[, c("Name", "Gender", "Species.Race", "Blood", "School")]

# Dividir a coluna School em School e House
HPCharactersFiltered <- transform(HPCharactersFiltered, 
                                  House = ifelse(grepl(" - ", School), sub(".* - ", "", School), NA),
                                  School = ifelse(grepl(" - ", School), sub(" - .*", "", School), School))

# Filtrar para manter apenas as quatro grandes casas
HPCharactersFiltered <- HPCharactersFiltered[HPCharactersFiltered$House %in% c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin"), ]

# Criar dados para o Diagrama de Cordas
chord_data <- table(HPCharactersFiltered$House, HPCharactersFiltered$Blood)
chord_data <- as.data.frame(as.table(chord_data))

# Limpar qualquer configuração anterior do circos
circos.clear()

# Definir parâmetros do circos
circos.par(start.degree = 90, gap.degree = 4)

# Criar o Diagrama de Cordas
chordDiagram(chord_data, transparency = 0.5)
title("Conexões entre Casas e Tipos de Sangue")

# Salvar o gráfico em formato PDF
dev.copy2pdf(file="HP.pdf", width = 10, height = 10)
