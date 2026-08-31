library(tidyverse)
library(lme4)
library(MuMIn)
library(DHARMa)
library(glmmTMB)
library(emmeans)


options(na.action = "na.fail")  # obrigatório para dredge
pontos_impactado <- c("RiodaBarra-P2", "RioMutuca-P3")
metodos_de_amostragem <- c("D Net", "Pool Net", "Sieve")
descarte <- c("RiodaBarra-P1")
rios <- c("rio_bebedouro", "rio_da_barra", "rio_mutuca")
################################### Dados ######################################

# Com ou sem method no id_tempo_espaço?
dados_completos <-
  read_csv("dados/processado/matriz_indices_imaturos_piata.csv") %>%
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
  filter(rio %in% rios) %>%
  filter(!locality_name %in% descarte)


dados <- dados_completos %>%
  group_by(campanha, rio, locality_name) %>%
  filter(all(metodos_de_amostragem %in% method)) %>%
  ungroup()


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
m2_selecionado <- get.models(ranking_modelos, subset = 2)[[1]]

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

# comparacoes rios
medias_rios <- emmeans(m2_selecionado, ~ rio)
contrastes_rios <- pairs(medias_rios)
contrastes_rios

# comparacoes redes
medias_redes <- emmeans(m2_selecionado, ~ method)
contrastes_redes <- pairs(medias_redes)
contrastes_redes



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




################################## Abundancia ##################################

global <- glmmTMB(
  total_macroinvertebrados ~
  campanha + method + rio + (1 | rio:locality_name),
  data = dados,
  family = nbinom2
)

ranking_modelos <- dredge(global)
m1_selecionado <- get.models(ranking_modelos, subset = 1)[[1]]
m2_selecionado <- get.models(ranking_modelos, subset = 2)[[1]]

# qualidade do ajuste do modelo 1
residuals_m1 <- simulateResiduals(m1_selecionado)
plot(residuals_m1)
testDispersion(residuals_m1)
testZeroInflation(residuals_m1)  # verificar excesso de zeros
testUniformity(residuals_m1)
summary(m1_selecionado)

# qualidade do ajuste do modelo 2
residuals_m2 <- simulateResiduals(m2_selecionado)
plot(residuals_m2)
testDispersion(residuals_m2)
testZeroInflation(residuals_m2)  # verificar excesso de zeros
testUniformity(residuals_m2)
summary(m2_selecionado)

############################ Indices de qualidade da água ######################

# bmwp -------------------------------------------------------------------------
global <- glmmTMB(
  bmwp ~
  campanha + method + rio + (1 | rio:locality_name),
  data = dados,
  family = gaussian
)

ranking_modelos <- dredge(global)
m1_selecionado <- get.models(ranking_modelos, subset = 1)[[1]]
m2_selecionado <- get.models(ranking_modelos, subset = 2)[[1]]

# qualidade do ajuste do modelo 1
residuals_m1 <- simulateResiduals(m1_selecionado)
plot(residuals_m1)
testDispersion(residuals_m1)
testZeroInflation(residuals_m1)  # verificar excesso de zeros
testUniformity(residuals_m1)
summary(m1_selecionado)

# comparacoes rios
medias_rios <- emmeans(m1_selecionado, ~ rio)
contrastes_rios <- pairs(medias_rios)

# comparacoes campanhas
medias_campanhas <- emmeans(m1_selecionado, ~ campanha)
contrastes_campanhas <- pairs(medias_campanhas)

# comparacoes metodos
medias_metodos <- emmeans(m1_selecionado, ~ method)
contrastes_metodos <- pairs(medias_metodos)

summary(m2_selecionado)
# IBVOL -----------------------------------------------------------------------

global <- glmmTMB(
  ibvol ~
  campanha + method + rio + (1 | rio:locality_name),
  data = dados,
  family = gaussian
)

ranking_modelos <- dredge(global)
m1_selecionado <- get.models(ranking_modelos, subset = 1)[[1]]



################################### Garbage ####################################
# criar uma tabela apenas com as linhas de dados_completos que nao estão em dados
# dados_nao_incluidos <- anti_join(dados_completos, dados, by = c("campanha", "rio", "locality_name", "method")) %>%
#   select(campanha, rio, locality_name, method, riqueza_familias)



# dados %>% 
#   group_by(method) %>% 
#   summarise(media_razao = mean(riqueza_familias, na.rm = TRUE))

# # Agora compare usando os 85 dados
# dados_completos %>% 
#   group_by(method) %>% 
#   summarise(media_razao = mean(riqueza_familias, na.rm = TRUE))
