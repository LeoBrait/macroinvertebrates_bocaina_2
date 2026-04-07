library(tidyverse)
library(lme4)

dados_espaciais <- read_csv("dados/dados_satelite_bocaina.csv") %>%
  select(rio, locality_name, longitude, latitude)


dados <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv")


histograma <- ggplot(dados, aes(x = riqueza_familias)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histograma da Riqueza de Ordens",
       x = "Riqueza de Ordens",
       y = "Frequência")

modelo_com_interacao <- glmer(
  riqueza_familias ~ method * rio + campanha + (1 | locality_name),
  data = dados,
  family = poisson
)
summary(modelo_com_interacao)

modelo_sem_interacao <- glmer(
  riqueza_familias ~ method + rio + campanha + (1 | locality_name),
  data = dados,
  family = poisson
)
summary(modelo_sem_interacao)

anova(modelo_sem_interacao, modelo_com_interacao, test = "Chisq")


m0 <- glmer(
  riqueza_familias ~ 1 + (1 | locality_name),
  data = dados,
  family = poisson
)

m1 <- glmer(
  riqueza_familias ~ method + rio + (1 | locality_name),
  data = dados,
  family = poisson
)

m2 <- glmer(
  riqueza_familias ~ rio + campanha + (1 | locality_name),
  data = dados,
  family = poisson
)

m3 <- glmer(
  riqueza_familias ~ campanha + method + rio + (1 | locality_name),
  data = dados,
  family = poisson
)

m3_satelite <- glmer(
  riqueza_familias ~ campanha + method + rio + cobertura + (1 | locality_name),
  data = dados_espaciais,
  family = poisson
)

# Teste de autocorrelação espacial Moran sobre residouos do glmm
library(spdep)
# Obter os resíduos do modelo
residuos <- residuals(m3)

# Criar um objeto de vizinhança
coords <- dados %>% select(locality_name, longitude, latitude)
nb <- dnearneigh(coords, 0, 100) # 10 km de distância
listw <- nb2listw(nb)

# Calcular o índice de Moran
moran.test(residuos, listw)
