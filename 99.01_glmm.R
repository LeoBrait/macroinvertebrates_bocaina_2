library(tidyverse)
library(lme4)
library(MuMIn)



####### LEMBRAR DE ADICIONAR BARRAMENTO.
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
  ) 


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

m00 <- glmer(
  riqueza_familias ~ 1 + (1 | id_tempo_espaço),
  data = dados,
  family = poisson
)

m0 <- glmer(
  riqueza_familias ~ 1 + (1 | locality_name),
  data = dados,
  family = poisson
)

m_comum <- glmer(riqueza_familias ~ method + rio + (1 | locality_name),
            data = dados, family = poisson)

m_overdispersion <- glmer(riqueza_familias ~ method + rio +
            (1 | locality_name) + (1 | id_tempo_espaço),
            data = dados, family = poisson)

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

m3_periodo <- glmer(
  riqueza_ordens ~ periodo + method + rio + (1 | id_tempo_espaço),
  data = dados,
  family = poisson
)

m3_campanha <- glmer(
  riqueza_ordens ~ campanha + method + rio + (1 | id_tempo_espaço),
  data = dados,
  family = poisson
)

m4_periodo <- glmer(
  riqueza_ordens ~ periodo + method + barramento + (1 | id_tempo_espaço),
  data = dados,
  family = poisson
)
summary(m4_periodo)
AIC(m3_periodo, m3_campanha)
summary(m3_campanha)
summary(m3_periodo)



m3_satelite <- glmer(
  riqueza_familias ~ campanha + method + rio + cobertura + (1 | locality_name),
  data = dados_espaciais,
  family = poisson
)


AIC(m00, m0)




### Dredge
global <- glmer(
  total_macroinvertebrados ~ periodo + method + rio + barramento + campanha + (1 | id_tempo_espaço),
  data = dados,
  family = poisson
)

options(na.action = "na.fail")  # obrigatório para dredge

resultado <- dredge(global)

resultado

get.models(resultado, subset = 1)
melhor <- get.models(resultado, subset = 1)[[1]]
summary(melhor)





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
