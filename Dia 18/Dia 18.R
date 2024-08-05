# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar as bibliotecas necessárias
library(tidyverse)
library(RColorBrewer)


# Carregar os dados
data <- read.csv("the-worlds-number-of-vaccinated-one-year-olds.csv")

# Lista de países da América do Sul
south_american_countries <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", 
                              "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", 
                              "Uruguay", "Venezuela")

# Filtrar os países da América do Sul
south_america_data <- data %>% filter(Entity %in% south_american_countries)

# Filtrar anos de interesse
filtered_data <- south_america_data %>% 
  filter(Year %in% c(2015, 2016, 2017, 2018, 2019, 2020, 2021))

# Reorganizar os dados para formato longo
vaccines_data <- filtered_data %>%
  gather(key = "Vaccine_Type", value = "Vaccinated_Count", 
         Number.of.one.year.olds.vaccinated.with.BCG, 
         Number.of.one.year.olds.vaccinated.with.DTP.containing.vaccine..3rd.dose,
         Number.of.one.year.olds.vaccinated.with.HepB3, 
         Number.of.one.year.olds.vaccinated.with.polio..3rd.dose,
         Number.of.one.year.olds.vaccinated.with.measles.containing.vaccine..1st.dose,
         Number.of.one.year.olds.vaccinated.with.Hib3,
         Number.of.one.year.olds.vaccinated.with.rubella.containing.vaccine..1st.dose,
         Number.of.one.year.olds.vaccinated.with.rotavirus..last.dose)

# Renomear os tipos de vacinas para uma melhor legibilidade
vaccines_data$Vaccine_Type <- recode(vaccines_data$Vaccine_Type,
                                     `Number.of.one.year.olds.vaccinated.with.BCG` = "BCG",
                                     `Number.of.one.year.olds.vaccinated.with.DTP.containing.vaccine..3rd.dose` = "DTP",
                                     `Number.of.one.year.olds.vaccinated.with.HepB3` = "HepB3",
                                     `Number.of.one.year.olds.vaccinated.with.polio..3rd.dose` = "Polio",
                                     `Number.of.one.year.olds.vaccinated.with.measles.containing.vaccine..1st.dose` = "Measles",
                                     `Number.of.one.year.olds.vaccinated.with.Hib3` = "Hib3",
                                     `Number.of.one.year.olds.vaccinated.with.rubella.containing.vaccine..1st.dose` = "Rubella",
                                     `Number.of.one.year.olds.vaccinated.with.rotavirus..last.dose` = "Rotavirus")

# Agregar os dados por ano e tipo de vacina
aggregated_data <- vaccines_data %>%
  group_by(Year, Vaccine_Type) %>%
  summarise(Total_Vaccinated = sum(Vaccinated_Count, na.rm = TRUE))

# Definir uma paleta de cores
color_palette <- brewer.pal(8, "Greys")

# Criar gráfico de barras agrupadas com melhorias
p <- ggplot(aggregated_data, aes(x = factor(Year), y = Total_Vaccinated, fill = Vaccine_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.85), color = "black", size = 0.1) +
  scale_fill_manual(values = color_palette) +
  labs(title = "Número de Crianças de Um Ano Vacinadas por Tipo de Vacina na América do Sul",
       x = "Ano",
       y = "Número de Crianças Vacinadas",
       fill = "Tipo de Vacina") +
  theme_classic(base_size = 15) +
  theme(axis.line.y = element_line(color = "black"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 18)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, max(aggregated_data$Total_Vaccinated) * 1.1))

# Exibir o gráfico
print(p)

# Salvar o gráfico em PDF
ggsave("vacinas.pdf", plot = p, width = 10, height = 5)


