# Instalar e carregar os pacotes necessários
#install.packages("treemap")

library(treemap)

# Dados: Campeões do Campeonato Brasileiro com cores específicas
clubes <- data.frame(
  times = c("Palmeiras", "Flamengo", "Santos", "Corinthians", "São Paulo", "Cruzeiro", 
           "Fluminense", "Vasco", "Atlético-MG", "Internacional", "Bahia", 
           "Botafogo", "Grêmio", "Athl", "Coritiba", "Guarani", "Sport"),
  titulos = c(11, 9, 8, 7, 6, 4, 4, 4, 3, 3, 2, 2, 2, 1, 1, 1, 1),
Colors = c("#006437", "#C52613", "white", "black", "#FE0000", "#2F529E", 
             "#870A28", "black", "black", "#E5050F", "#006CB5", "black", 
             "#0D80BF", "#CE181E", "#00544D", "#006C51","red"))


TextColor = c("white", "black", "#C69F0F", "white", "white", "white", 
              "#00613C", "white", "white", "white", "white", "white", 
              "black", "black", "white", "white",  "#FFD900")

treemap(clubes,
        index = "times", vSize = "titulos", type = "color", 
        vColor = "Colors",
        fontsize.labels = c(12),
        border.col = "black",
        border.lwds = 0.8,
        title = "Campeões do Campeonato Brasileiro de Futebol",
        fontface.labels = c(2), # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels = c("transparent"), # Background color of labels
        #align.labels = "center",
        overlap.labels = 1, 
        inflate.labels = F)

dev.copy2pdf(file="titulos_BR.pdf", width = 6, height = 4)


