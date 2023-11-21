# ibm-code

* pasta raiz:
  Código netlogo - crossings.nlogo
  Scripts
    - simulations: usado para fazer as simulacoes usando o pacote nlrx - precisa ter o netlogo versao 6.0.4 no computador
    - sensitivity_analysis: usado para fazer a analise e graficos de sensibilidade, com pacote sensitivity e metodo sobol2007, usando os resultados do script "simulations_sobol_design" 
    - grafico parametros: usado para os graficos da relacao dos inputs com outputs, com modelo ajustado
    - aomisc_model_fitting: usado para ajustar e comparar modelos para cada grafico dos parametros, com pacote aomisc/bbmle

* pasta results:
    - arquivos _simulations.rds = todo o arquivo das simulacoes feitas nos scripts "simulations
    - arquivos _output.csv = apenas a planilha final de resultados do arquivo .rds
    - arquiv model_fitting = resultados das competicoes dos modelos

* pasta imagens:
    figuras dos resultados dos parametros, das analises de sensibilidade, dos cenarios e dos passos do netlogo
