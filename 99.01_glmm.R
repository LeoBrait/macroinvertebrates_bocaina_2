library(tidyverse)
library(lme4)

dados <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv")



modelo_com_interacao <- glmer(
  riqueza_familias ~ method * rio * campanha + (1 | locality_name),
  data = dados,
  family = poisson
)

modelo_sem_interacao <- glmer(
  riqueza_familias ~ method + rio + campanha + (1 | locality_name),
  data = dados,
  family = poisson
)

summary(modelo_com_interacao)
summary(modelo_sem_interacao)
anova(modelo_sem_interacao, modelo_com_interacao, test = "Chisq")


drop1(modelo_sem_interacao, test = "Chisq")
drop1(modelo_com_interacao, test = "Chisq")


