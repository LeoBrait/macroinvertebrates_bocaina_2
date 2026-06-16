library(tidyverse)
library(lme4)
library(MuMIn)
library(DHARMa)
library(glmmTMB)


options(na.action = "na.fail")  # obrigatório para dredge
pontos_impactado <- c("RiodaBarra-P2", "RioMutuca-P3")
metodos_de_amostragem <- c("D Net", "Pool Net", "Sieve")
rios <- c("rio_bebedouro", "rio_da_barra", "rio_mutuca")
################################### Dados ######################################
dados_espaciais <- read_csv("dados/dados_satelite_bocaina.csv")

# Com ou sem method no id_tempo_espaço?
dados <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv") %>%
  mutate(
    periodo = case_when(
      campanha %in% c("campanha_1", "campanha_2", "campanha_4") ~ "seco",
      campanha %in% c("campanha_3") ~ "chuvoso",
      TRUE ~ "desconecido"
    ),
    barramento = case_when(
      locality_name %in% pontos_impactado ~ "barrado",
      TRUE ~ "não_barrado"
    )
  ) %>%
  filter(method %in% metodos_de_amostragem) %>%
  filter(rio %in% rios)




################################### Riqueza ####################################

# Familias ---------------------------------------------------------------------
global <- glmmTMB(
  riqueza_familias ~
  campanha + method + rio + (1 | rio:locality_name),
  data = dados,
  family = nbinom2
)


ranking_modelos <- dredge(global)
m1_selecionado <- get.models(ranking_modelos, subset = 1)[[1]]
m2_selecionado <- get.models(ranking_modelos, subset = 3)[[1]]

# qualidade do ajuste do modelo 1
residuals_m1 <- simulateResiduals(m1_selecionado)
plot(residuals_m1)
testDispersion(residuals_m1)
testZeroInflation(residuals_m1)  # verificar excesso de zeros
testUniformity(residuals_m1)

# qualidade do ajuste do modelo 2
residuals_m2 <- simulateResiduals(m2_selecionado)
plot(residuals_m2)
testDispersion(residuals_m2)
testZeroInflation(residuals_m2)  # verificar excesso de zeros
testUniformity(residuals_m2)

summary(m1_selecionado)
summary(m2_selecionado)

# Ordens -----------------------------------------------------------------------
global <- glmer(
  riqueza_ordens ~
  campanha + method + rio + (1 | rio:locality_name),
  data = dados,
  family = poisson
)

ranking_modelos <- dredge(global)
m1_selecionado <- get.models(ranking_modelos, subset = 2)[[1]]
m2_selecionado <- get.models(ranking_modelos, subset = 4)[[1]]


# qualidade do ajuste do modelo 1
residuals_m1 <- simulateResiduals(m1_selecionado)
plot(residuals_m1)
testDispersion(residuals_m1)
testZeroInflation(residuals_m1)  # verificar excesso de zeros
testUniformity(residuals_m1)

# qualidade do ajuste do modelo 2
residuals_m2 <- simulateResiduals(m2_selecionado)
plot(residuals_m2)
testDispersion(residuals_m2)
testZeroInflation(residuals_m2)  # verificar excesso de zeros
testUniformity(residuals_m2)


summary(m1_selecionado)
summary(m2_selecionado)

anova(m1_selecionado, m2_selecionado, test = "Chisq")




################################## Grafico #####################################

 plot <- ggplot(
    dados,
    aes(x = method, y = total_macroinvertebrados)
  ) +
    theme_minimal() +
    geom_boxplot() +
    geom_point(
        aes(
        color = factor(rio),
        shape = factor(periodo)
        ),
        #fill = "gray70", 
        size = 10,
        alpha = 0.7,
        stroke = 2) +
    scale_shape_manual(values = c(4, 21))



######################### Autocorrelação espacial ##############################
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
