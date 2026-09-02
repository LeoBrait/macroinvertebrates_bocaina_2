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


dados_completos <-
  read_csv("dados/processado/matriz_indices_imaturos_piata.csv") %>%
  # Coloquei essas distinções, mas terminei nao utilizando,
  # Só dificultam a convergência dos modelos
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
medias_rios <- emmeans(m2_selecionado, ~ rio, type = "response")
contrastes_rios <- pairs(medias_rios)
contrastes_rios

# comparacoes redes
medias_redes <- emmeans(m2_selecionado, ~ method, type = "response")
contrastes_redes <- pairs(medias_redes)
contrastes_redes

#******************************************************************************#
#                               PARECER DA ANALISE                             #
#******************************************************************************#
# Reposta obtida: Os métodos de coleta são marginalmente significativos 
# (p=0.06). Acho importente pontuar isso, porque essa diferneça se torna
# significativa quando eliminamos alguns passos de filtragem. E o modelo com
# método de Friedman para este parâmetro também dá como significativo.

# Quando fazemos os testes de comparações múltiplas, vemos que a diferença entre
# as redes D e Pool é significativa. (p = 0.02).

# Há uma diferença detectada entre as campanhas, mas esse resultado nao vale
# a pena porque campanha está confundida com rio. As campanhas 1, 2 e 3 foram
# feitas nos rios barra e bebedouro, enquanto que o rio mutuca só foi amostrado
# a partir da camapanha 2 e o único amostrado na campanha 4.

#******************************************************************************#

# Ordens -----------------------------------------------------------------------
global <- glmer(
  riqueza_ordens ~
    campanha + method + rio + (1 | rio:locality_name),
  data = dados,
  family = poisson
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

#******************************************************************************#
#                               PARECER DA ANALISE                             #
#******************************************************************************#
# Modelo nulo é o melhor modelo, o modelo 2 tem problemas na verificação       #
# de uniformidade dos resíduos.                                                #
#                                                                              #
#******************************************************************************#

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

#******************************************************************************#
#                               PARECER DA ANALISE                             #
#******************************************************************************#
# Ambos os modelos 1 e 2 apontam para o mesmo resultado. A rede de piscina     #
# coleta mais.                                                                 #
#                                                                              #
#******************************************************************************#

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
testZeroInflation(residuals_m1)
testUniformity(residuals_m1)
summary(m1_selecionado)

# qualidade do ajuste do modelo 2
residuals_m2 <- simulateResiduals(m2_selecionado)
plot(residuals_m2)
testDispersion(residuals_m2)
testZeroInflation(residuals_m2)
testUniformity(residuals_m2)
summary(m2_selecionado)

# comparacoes rios
medias_rios <- emmeans(m1_selecionado, ~ rio)
contrastes_rios <- pairs(medias_rios)

#******************************************************************************#
#                               PARECER DA ANALISE                             #
#******************************************************************************#
# Ambos os modelos 1 e 2 apontam para o mesmo resultado. O Rio Mutuca tem      #
# maior qualidade da agua que todos os outros rios                             #
# O modelo 2 adiciona a variável método, mas não é significativo.              #
#******************************************************************************#


# IBVOL -----------------------------------------------------------------------

global <- glmmTMB(
  ibvol ~
    campanha + method + rio + (1 | rio:locality_name),
  data = dados,
  family = gaussian
)

ranking_modelos <- dredge(global)
m1_selecionado <- get.models(ranking_modelos, subset = 2)[[1]]
summary(m1_selecionado)


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
