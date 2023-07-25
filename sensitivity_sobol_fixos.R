#
#
#
#### sensitivity cenarios fixos
#
#
# pacotes
library(sensitivity)
library(dplyr)
library(here)

# obter inputs do objeto netlogo para adicionar na analise de sensibilidade
# carregar o objeto netlogo - pasta results, arquivo sobol.rds
# inputs
random_sample_1 <- nl_crossings@simdesign@simobject[[1]][["X1"]]
random_sample_2 <- nl_crossings@simdesign@simobject[[1]][["X2"]]
experiment_design <- nl_crossings@simdesign@simobject[[1]][["X"]]

# outputs
outputs <- read.csv(here("results", "inputs_and_outputs.csv"))
assess_values <- outputs$assess_top_sections
crossings_values <- outputs$total_crossings

# analise sensibilidade 
# variavel assess_top_sections
sensitivity_assess <- sobol(model = NULL, X1 = random_sample_1, X2 = random_sample_2, order = 1, nboot = 1000) 
sensitivity_assess$X <- experiment_design
tell(sensitivity_assess, assess_values)
print(sensitivity_assess)
plot(sensitivity_assess)


# corrigida 
tell(sensitivity_assess, (assess_values-mean(assess_values))/sd(assess_values))
print(sensitivity_assess)
plot(sensitivity_assess)

# total crossings
sensitivity_crossings <- sobol(model = NULL, X1 = random_sample_1, X2 = random_sample_2, order = 2, nboot = 1000) 
sensitivity_crossings$X <- experiment_design
tell(sensitivity_crossings, crossings_values)
print(sensitivity_crossings)
plot(sensitivity_crossings)

# corrigida 
tell(sensitivity_crossings, (crossings_values-mean(crossings_values))/sd(crossings_values))
print(sensitivity_crossings)
plot(sensitivity_crossings)


# create bias-corrected plots of first-order and total sensitivity indices
# grimm et al 2014 - Facilitating Parameter Estimation and Sensitivity Analysis of Agent-Based Models: A Cookbook Using NetLogo and R
plotfun <- function(x, title) {
  x_corr <- x[["original"]] - x[["bias"]]
  plot(x_corr, ylim=c(0,1), main = title)
  # confidence intervals
  at <- 1 : nrow(x)
  if (("min. c.i." %in% colnames(x)) & "max. c.i." %in% colnames(x)) {
    for (j in at) {
      lines(c(at[j], at[j]), c(x[["min. c.i."]][j], x[["max. c.i."]][j]))
    }
  }
}
plotfun(sensitivity_assess$S, title = "assess_top_sections")
plotfun(sensitivity_crossings$S, title = "total_crossings")

# etapas que passei mas nao sei se vao precisar aparecer

# apenas para conferir se os dados do objeto netlogo e do que o sensitivity gera eram iguais
# teria que ser antes de designar o experiment_desing com $X - estava tudo igual
sensitivity_assess$X
nl_crossings@simdesign@simobject[[1]][["X"]]
all.equal(sensitivity_assess$X, nl_crossings@simdesign@simobject[[1]][["X"]])


# organizando outputs
# juntei o arquivo salvo pelo netlogo com o output do nlrx e salvei um arquivo csv para usar no sobol
salvo_netlogo <- read.csv(here("results", "sobol_teste.csv")) # preciso desse para poder ter os ids mas tenho que excluir a duplicacao
resultados_nlrx <- objeto_netlogo@simdesign@simoutput # cada combinacao foi rodada duas vezes para salvar os rasters

agrupados <- left_join(resultados_nlrx, salvo_netlogo) # agrupei os dois arquivos por colunas identicas
agrupados <- agrupados %>% filter(!row_number() %in% c(10911, 10914)) # por alguma razao as informacoes dessa linha estavam duplicadas e tive que excluir
agrupados <- agrupados %>% filter(`[run number]` == 2) # mantive apenas 1 resultado por combinacao
write.csv(agrupados, here("results", "inputs_and_outputs.csv"), row.names = F)

# aqui foi para conferir se a ordem das linhas no arquivo dos resultados esta igual a ordem dos inputs
ordem_salvo_netlogo <- agrupados %>% 
  select(proportion_of_habitat, matrix_permeability, perceptual_range, vision_angle)
ordem_resultados_nlrx <- sensitivity_assess$X
all.equal(ordem_salvo_netlogo$proportion_of_habitat, ordem_resultados_nlrx$proportion_of_habitat) # no geral da erro mas comparando as colunas sozinhas tao todas iguais

