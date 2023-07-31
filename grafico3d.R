# Instale e carregue a biblioteca plot3D
install.packages("plot3D")
library(plot3D)
cols <- colorRampPalette(c("#ccffcc", "black"))(10)                                 # 20 distinct colors

# Exemplo de dados
# Crie o gr?fico 3D com os eixos representando as vari?veis preditoras e a cor representando a vari?vel resposta
points3D(x = habitat_amount_simulations$matrix_permeability,
          y = habitat_amount_simulations$vision_angle,
          z = habitat_amount_simulations$perceptual_range,
          colvar = habitat_amount_simulations$assess_top_sections,
          col = cols,
          pch = 16,
          cex = 0.2,
          colkey = TRUE,
          theta = 45,
          phi = 20,
          bty = "g",
          xlim = c(min(habitat_amount_simulations$matrix_permeability), max(habitat_amount_simulations$matrix_permeability)),
          ylim = c(min(habitat_amount_simulations$vision_angle), max(habitat_amount_simulations$vision_angle)),
          zlim = c(min(habitat_amount_simulations$perceptual_range), max(habitat_amount_simulations$perceptual_range)),
          xlab = "matrix permeability",
          ylab = "vision angle",
          zlab = "perceptual range",
          clab = "Assess top sections",
          main = "Cenario 1")
par(mar = c(1, 1, 2, 3))
# Adicione uma legenda de cores ao gr?fico
add.xyz.labels(x = dados$preditor1,
               y = dados$preditor2,
               z = dados$preditor3,
               labs = dados$resposta,
               cex = 0.7)
