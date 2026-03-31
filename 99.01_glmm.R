library(tidyverse)
library(lme4)

dados <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv")



modelo <- glmer(
    riqueza_familias ~ rio * method + (1 | locality_name),
    data = dados,
    family = poisson
)
summary(modelo)


m1 <- glmer(
  riqueza_familias ~ rio * method + (1 | locality_name),
  data = dados,
  family = poisson
)

m0 <- glmer(
  riqueza_familias ~ rio + (1 | locality_name),
  data = dados,
  family = poisson
)

anova(m0, m1, test = "Chisq")

m2 <- glmer(
  riqueza_familias ~ rio + method + (1 | locality_name),
  data = dados,
  family = poisson
)

anova(m2, m1, test = "Chisq")
