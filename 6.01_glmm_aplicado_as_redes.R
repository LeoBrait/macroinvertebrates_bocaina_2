library(tidyverse)
library(lme4)
library(MuMIn)


options(na.action = "na.fail")  # obrigatório para dredge
pontos_impactado <- c("RiodaBarra-P2", "RioMutuca-P3")
metodos_de_amostragem <- c("D Net", "Pool Net", "Sieve")
rios <- c("rio_bebedouro", "rio_da_barra", "rio_mutuca")
################################### Dados ######################################
dados_espaciais <- read_csv("dados/dados_satelite_bocaina.csv")

# Com ou sem method no id_tempo_espaço?
dados <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv") %>%
  mutate(
    id_tempo_espaço = paste0(campanha, rio, locality_name),
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


histograma <- ggplot(dados, aes(x = total_macroinvertebrados)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histograma da Riqueza de Ordens",
       x = "Riqueza de Ordens",
       y = "Frequência")




### Dredge
global <- glmer(
  total_macroinvertebrados ~
  periodo + method + rio + barramento + (1 | id_tempo_espaço),
  data = dados,
  family = poisson
)



ranking_modelos <- dredge(global)
modelo_selecionado <- get.models(ranking_modelos, subset = 5)[[1]]
summary(modelo_selecionado)



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
