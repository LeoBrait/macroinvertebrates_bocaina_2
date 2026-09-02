library(tidyverse)
library(vegan)
library(ggpubr)


################################### Carregar dados #############################
rios <- c("rio_bebedouro", "rio_da_barra", "rio_mutuca")
metodos <- c("D Net", "Pool Net", "Sieve")
colunas <- c(
    "campanha", "rio", "locality_name", "method",
    "total_macroinvertebrados", "riqueza_ordens", "riqueza_familias"
)

amostras_descartadas_vetor <- c("RiodaBarra-P1")

# Pasta de figuras
pasta_figuras <- "figuras/cap1/comparacao_redes/"
if (!dir.exists(pasta_figuras)) {
  dir.create(pasta_figuras, recursive = TRUE)
}

pasta_tabelas <- "tabelas/cap1/comparacao_redes/simper/"
if (!dir.exists(pasta_tabelas)) {
  dir.create(pasta_tabelas, recursive = TRUE)
}


df_ordens <-
  read_csv(
    "dados/processado/matriz_comunidades_ordens_imaturos_largo.csv"
  ) %>%
  filter(rio %in% rios) %>%
  filter(method %in% metodos) %>%
  filter(!locality_name %in% amostras_descartadas_vetor) %>%
  group_by(campanha, rio, locality_name) %>%
  filter(all(metodos %in% method)) %>%
  ungroup() %>%
  mutate(
    campanha = case_when(
      campanha == "campanha_1" ~ "C1",
      campanha == "campanha_2" ~ "C2",
      campanha == "campanha_3" ~ "C3",
      campanha == "campanha_4" ~ "C4"
    ),
    rio = case_when(
      rio == "rio_bebedouro" ~ "Rio Bebedouro",
      rio == "rio_da_barra" ~ "Rio da Barra",
      rio == "rio_mutuca" ~ "Rio Mutuca",
      rio == "rio_da_serra" ~ "Rio da Serra",
      rio == "rio_carrapicho" ~ "Rio Carrapicho"
    ),
    method = case_when(
      method == "D Net" ~ "Rede D",
      method == "Pool Net" ~ "Rede de Piscina",
      method == "Sieve" ~ "Peneira"
    )
  )

df_familias <- read_csv(
  "dados/processado/matriz_comunidades_familias_imaturos_largo.csv") %>%
  filter(rio %in% rios) %>%
  filter(method %in% metodos) %>%
  filter(!locality_name %in% amostras_descartadas_vetor) %>%
  group_by(campanha, rio, locality_name) %>%
  filter(all(metodos %in% method)) %>%
  ungroup() %>%
  mutate(
    campanha = case_when(
      campanha == "campanha_1" ~ "C1",
      campanha == "campanha_2" ~ "C2",
      campanha == "campanha_3" ~ "C3",
      campanha == "campanha_4" ~ "C4"
    ),
    rio = case_when(
      rio == "rio_bebedouro" ~ "Rio Bebedouro",
      rio == "rio_da_barra" ~ "Rio da Barra",
      rio == "rio_mutuca" ~ "Rio Mutuca",
      rio == "rio_da_serra" ~ "Rio da Serra",
      rio == "rio_carrapicho" ~ "Rio Carrapicho"
    ),
    method = case_when(
      method == "D Net" ~ "Rede D",
      method == "Pool Net" ~ "Rede de Piscina",
      method == "Sieve" ~ "Peneira"
    )
  )

################################ Preparar dados ################################


comunidade_ordens <- df_ordens %>%
  select(-c(campanha, rio, locality_name, method))

comunidade_familias <- df_familias %>%
  select(-c(campanha, rio, locality_name, method))

grupos <- df_ordens$rio

simper_ordens <- simper(
  comunidade_ordens,
  grupos,
  permutations = 99999
)

simper_familias <- simper(
  comunidade_familias,
  grupos,
  permutations = 99999
)

summary_simper_ordens <- summary(simper_ordens)
summary_simper_familias <- summary(simper_familias)

# salvar os resultados do SIMPER em arquivos CSV
for (i in seq_along(summary_simper_ordens)) {
  nome_arquivo <- paste0(
    pasta_tabelas, "simper_ordens_", names(summary_simper_ordens)[i], ".csv"
  )

  # converter rownames em uma coluna antes de salvar
  summary_simper_ordens[[i]] <- summary_simper_ordens[[i]] %>%
    rownames_to_column(var = "ordem")
  
  # calcular contribuição individual
  summary_simper_ordens[[i]] <- summary_simper_ordens[[i]] %>%
    mutate(contribuicao_individual = cumsum - lag(cumsum, default = 0))


  write_csv(summary_simper_ordens[[i]], nome_arquivo)
}

for (i in seq_along(summary_simper_familias)) {

  nome_arquivo <- paste0(
    pasta_tabelas, "simper_familias_", names(summary_simper_familias)[i], ".csv"
  )

  # converter rownames em uma coluna antes de salvar
  summary_simper_familias[[i]] <- summary_simper_familias[[i]] %>%
    rownames_to_column(var = "familia")
  
  # calcular contribuição individual
  summary_simper_familias[[i]] <- summary_simper_familias[[i]] %>%
    mutate(contribuicao_individual = cumsum - lag(cumsum, default = 0))

  write_csv(summary_simper_familias[[i]], nome_arquivo)
}

