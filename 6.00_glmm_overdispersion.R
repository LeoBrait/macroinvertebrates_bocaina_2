library(tidyverse)
library(lme4)

dados_espaciais <- read_csv("dados/dados_satelite_bocaina.csv") 

# Com ou sem method no idunico?
dados_raw <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv") 

vetor_respostas <- c(
    "riqueza_familias",
    "riqueza_ordens",
    "bmwp",
    "total_macroinvertebrados"
)


dados <- dados_raw %>%
  mutate(
    id_tempo_espaço = paste0(campanha, rio, locality_name),
    idunico = paste0(campanha, rio, locality_name, method)
  )

for (i in vetor_respostas) {
  cat(
    "#######################################################################\n",
    "Analisando resposta:", i, "\n",
    "#######################################################################\n",
    sep = ""
  )
  cat(
    "\n",
    "-----------------------------------\n",
    "Modelo 1: Aleatório por espaço\n",
    "-----------------------------------\n",
    sep = ""
  )

  m1 <- glmer(
    as.formula(paste0(i, " ~ 1 + (1 | locality_name)")),
    data = dados,
    family = poisson
  )

  print(summary(m1))

  cat(
    "\n",
    "-----------------------------------\n",
    "Modelo 2: Aleatório por espaço e tempo\n",
    "-----------------------------------\n",
    sep = ""
  )

  m2 <- glmer(
    as.formula(paste0(i, " ~ 1 + (1 | id_tempo_espaço)")),
    data = dados,
    family = poisson
  )
  print(summary(m2))

  cat(
    "\n",
    "-----------------------------------\n",
    "Modelo 3: Aleatório por observação absoluta (tempo, espaço e método)\n",
    "-----------------------------------\n",
    sep = ""
  )

  m3 <- glmer(
    as.formula(paste0(i, " ~ 1 + (1 | idunico)")),
    data = dados,
    family = poisson
  )
  print(summary(m3))

  aics <- AIC(m1, m2, m3) %>%
    mutate(
        nome_modelo = c(
            "modelo espaço", 
            "modelo espaço-tempo", 
            "modelo espaço-tempo-método"
        )
    ) %>%
    select(nome_modelo, everything())
  print(aics)




}

