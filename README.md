  # Avaliar sensibilidade da métrica a partir p-valor e na sobreposição dos
  # quartis considerando o método de Barbour et al. (1996)
  #
  # a. Caso não haja sobreposição dos interquartis e p < 0.05, a métrica é
  #    considerada sensível ao impacto (score 3).
  # b. Caso haja sobreposição dos interquartis, mas a mediana de ambos os grupos 
  #    estiver fora do intervalo interquartil um do outro e p < 0.05, a métrica
  #    é considerada moderadamente sensível ao impacto (score 2).
  # c. Caso haja sobreposição dos interquartis e a mediana de pelo menos de um
  #    dos grupos estiver fora do intervalo interquartil do outro grupo e
  #    p < 0.05, a métrica é considerada pouco sensível ao impacto (score 1).
  # d. Caso haja sobreposição dos interquartis e as medianas de ambos os grupos
  #    estejam dentro do intervalo interquartil um do outro ou p >= 0.05, a
  #    métrica é considerada insensível ao impacto (score 0).