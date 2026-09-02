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
  dir.create(file.path(pasta_tabelas, "method"), recursive = TRUE)
  dir.create(file.path(pasta_tabelas, "rio"), recursive = TRUE)
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


# Analise dos rios -------------------------------------------------------------
grupos <- df_ordens$rio

simper_ordens_rio <- simper(
  comunidade_ordens,
  grupos,
  permutations = 99999
)

simper_familias_rio <- simper(
  comunidade_familias,
  grupos,
  permutations = 99999
)

summary_simper_ordens_rio <- summary(simper_ordens_rio)
summary_simper_familias_rio <- summary(simper_familias_rio)

for (i in seq_along(summary_simper_ordens_rio)) {


  nome_arquivo_ordens <- file.path(
    pasta_tabelas,
    "rio",
    paste0("simper_ordens_", names(summary_simper_ordens_rio)[i], ".csv")
  )

  nome_arquivo_familias <- file.path(
    pasta_tabelas,
    "rio",
    paste0("simper_familias_", names(summary_simper_familias_rio)[i], ".csv")
  )

  # converter rownames em uma coluna antes de salvar
  summary_simper_ordens_rio[[i]] <- summary_simper_ordens_rio[[i]] %>%
    rownames_to_column(var = "ordem")

  summary_simper_familias_rio[[i]] <- summary_simper_familias_rio[[i]] %>%
    rownames_to_column(var = "familia")

  # calcular contribuição individual
  summary_simper_ordens_rio[[i]] <- summary_simper_ordens_rio[[i]] %>%
    mutate(contribuicao_individual = cumsum - lag(cumsum, default = 0))

  summary_simper_familias_rio[[i]] <- summary_simper_familias_rio[[i]] %>%
    mutate(contribuicao_individual = cumsum - lag(cumsum, default = 0))

  # Salvar
  write_csv(summary_simper_ordens_rio[[i]], nome_arquivo_ordens)
  write_csv(summary_simper_familias_rio[[i]], nome_arquivo_familias)



}



# Analise dos metodos ----------------------------------------------------------
grupos <- df_ordens$method

simper_ordens_metodo <- simper(
  comunidade_ordens,
  grupos,
  permutations = 99999
)

simper_familias_metodo <- simper(
  comunidade_familias,
  grupos,
  permutations = 99999
)

summary_simper_ordens_metodo <- summary(simper_ordens_metodo)
summary_simper_familias_metodo <- summary(simper_familias_metodo)

for (i in seq_along(summary_simper_ordens_metodo)) {
  nome_arquivo_ordens <- file.path(
    pasta_tabelas,
    "method",
    paste0("simper_ordens_", names(summary_simper_ordens_metodo)[i], ".csv")
  )

  nome_arquivo_familias <- file.path(
    pasta_tabelas,
    "method",
    paste0("simper_familias_", names(summary_simper_familias_metodo)[i], ".csv")
  )

  # converter rownames em uma coluna antes de salvar
  summary_simper_ordens_metodo[[i]] <- summary_simper_ordens_metodo[[i]] %>%
    rownames_to_column(var = "ordem")

  summary_simper_familias_metodo[[i]] <- summary_simper_familias_metodo[[i]] %>%
    rownames_to_column(var = "familia")

  # calcular contribuição individual
  summary_simper_ordens_metodo[[i]] <- summary_simper_ordens_metodo[[i]] %>%
    mutate(contribuicao_individual = cumsum - lag(cumsum, default = 0))

  summary_simper_familias_metodo[[i]] <- summary_simper_familias_metodo[[i]] %>%
    mutate(contribuicao_individual = cumsum - lag(cumsum, default = 0))

  # Salvar
  write_csv(summary_simper_ordens_metodo[[i]], nome_arquivo_ordens)
  write_csv(summary_simper_familias_metodo[[i]], nome_arquivo_familias)
}








