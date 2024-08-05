# Script para visualização de dados - 30 dias e 30 gráficos
# Prof. Giuseppe Leite
# email: ggfleite@gmail.com

# Carregar a biblioteca necessária
library(ggpubr)

# Carregar os dados
shark_attacks <- read.csv("shark_attacks.csv")

# Criar o gráfico de dispersão com ggpubr
p <- ggscatter(shark_attacks, x = "IceCreamSales", y = "SharkAttacks", 
               cor.coef = TRUE, cor.method = "spearman",
               xlab = "Vendas de Sorvete", ylab = "Ataques de Tubarão",
               title = "Correlação entre Vendas de Sorvete e Ataques de Tubarão") +
  stat_smooth(method = "lm", col = "#e50000", fill = "#ffcccc") +
  theme_bw() +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    plot.title = element_text(color = "black"),
    plot.subtitle = element_text(color = "black"),
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remover linhas de grade principais
    panel.grid.minor = element_blank()    # Remover linhas de grade secundárias
  )

# Exibir o gráfico
print(p)

# Salvar o gráfico em PDF
ggsave("shark_attacks.pdf", plot = p, width = 5, height = 5)
