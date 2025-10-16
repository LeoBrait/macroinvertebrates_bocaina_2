library(tidyverse)


################################# Carregar Dados ###############################
setwd(dir = "C:/Users/Giovana/Desktop/mestrado_giovana/")
dados_df <- read.csv(file = "dados/imaturos_piata _22_08_2025.csv", sep = ",")

################################# Vetores ######################################
campanha1_vetor <- c(
  "23.V.2024", "24.V.2024", "25.V.2024",
  "26.V.2024", "27.V.2024"
)

campanha2_vetor <- c(
  "26.VIII.2024", "27.VIII.2024", "28.VIII.2024"
) 

campanha3_vetor <- c(
  "12.II.2025", "13.II.2025", "15.II.2025",
  "19.II.2025", "20.II.2025"
)

campanha4_vetor <- c("09.VII.2025", "12.VII.2025", "08.VII.2025")

riobebedouro_vetor <- c (
  "RioBebedouro-P1", "RioBebedouro-P2", "RioBebedouro-P3"
)

rio_da_barra_vetor <- c (
  "RiodaBarra-P1/2", "RiodaBarra-P2", "RiodaBarra-P3"
)

rio_mutuca_vetor <- c (
  "RioMutuca-P1", "RioMutuca-P2", "RioMutuca-P3"
)

rio_carrapicho_vetor <- c (
  "RioCarrapicho-P1", "RioCarrapicho-P2", "RioCarrapicho-P3"
)

rio_serra_vetor <- c (
  "RiodaSerra-P2", "RiodaSerra-P3"
)  

amostras_descartadas_vetor <- c(
  "RiodaBarra-P1",
  "RioCarrapicho-P1", 
  "RioCarrapicho-P2", 
  "RioCarrapicho-P3",
  "RiodaSerra-P2", 
  "RiodaSerra-P3"
)

############################### pre-Processamento ##############################

# limpar
dados_df <- filter(
  dados_df,
  !(LocalityName %in% amostras_descartadas_vetor)
)

# Corrigir o nome "Rede Piscina" para "Rede de Piscina"
dados_df <- mutate(
  dados_df,
  Method = ifelse(Method == "RedePiscina", "Rede de Piscina", Method)
)

# Corrigir o nome "Rede Piscina" para "Rede de Piscina"
dados_df <- mutate(
  dados_df,
  Method = ifelse(Method == "RededePiscina", "Rede de Piscina", Method)
)
# Corrigir o nome "Rede Piscina" para "Rede de Piscina"
dados_df <- mutate(
  dados_df,
  Method = ifelse(Method == "Rede de Piscina", "Pool Net", Method)
)

# Corrigir o nome "Rede D" para "D net"
dados_df <- mutate(
  dados_df,
  Method = ifelse(Method == "RedeD", "D Net", Method)
)

# Corrigir o nome "Rede D" para "D net"
dados_df <- mutate(
  dados_df,
  Method = ifelse(Method == "Peneira", "Sieve", Method)
)

# criar coluna de campanha
dados_df <- mutate(                       # funcao que cria/mod colunas
  dados_df,                               # tabela
  campanha = case_when(                         # nova coluna
    Date %in% campanha1_vetor ~ "campanha_1",   # condiçao e resultado
    Date %in% campanha2_vetor ~ "campanha_2",   # condiçao e resultado
    Date %in% campanha3_vetor ~ "campanha_3",    # condiçao e resultado
    Date %in% campanha4_vetor ~ "campanha_4"
  )   
)

# criar coluna de rio
dados_df <- mutate(
  dados_df,
  rio = case_when(
    LocalityName %in% riobebedouro_vetor ~ "rio_bebedouro",
    LocalityName %in% rio_da_barra_vetor ~ "rio_da_barra",
    LocalityName %in% rio_mutuca_vetor ~ "rio_mutuca",
  )
)

# renomear e colocar em minusculo
dados_df <- dados_df %>%
  rename(
    class = Class,
    order = Order,
    family = Family,
    genus = Genus,
    abundance = Abundance,
    collected_by = CollectedBy,
    date = Date,
    method = Method,
    locality_name = LocalityName
  ) 



write_csv(
  dados_df,
  "dados/processado/imaturos_piata_limpo.csv"
)

################################## Processamento ###############################

# Classes ----------------------------------------------------------------------
dados_classes_df <- dados_df %>%
  select(campanha, rio, locality_name, method, class, abundance) %>%
  group_by(campanha, rio, locality_name, method, class) %>%
  summarise(abundance = sum(abundance))

dados_classes_df_largo <- dados_classes_df %>%
  pivot_wider(
    id_cols = c("campanha", "rio", "locality_name", "method"),
    names_from = class,
    values_from = abundance
  )

dados_classes_df_largo[is.na(dados_classes_df_largo)] <- 0.0

write_csv(
  dados_classes_df_largo,
  "dados/processado/matriz_comunidades_classes_imaturos_largo.csv"
)

# Ordens -----------------------------------------------------------------------
dados_ordens_df <- dados_df %>%
  select(campanha, rio, locality_name, method, order, abundance) %>%
  group_by(campanha, rio, locality_name, method, order) %>%
  summarise(abundance = sum(abundance))

dados_ordens_df_largo <- dados_ordens_df %>%
  pivot_wider(
    id_cols = c("campanha", "rio", "locality_name", "method"),
    names_from = order,
    values_from = abundance
  ) %>%
  rename(
    locality_name = locality_name
  )


dados_ordens_df_largo[is.na(dados_ordens_df_largo)] <- 0.0

write_csv(
  dados_ordens_df_largo,
  "dados/processado/matriz_comunidades_ordens_imaturos_largo.csv"
)

# Famílias-----------------------------------------------------------------------
dados_familia_df <- dados_df %>%
  select(campanha, rio, locality_name, method, family, abundance) %>%
  group_by(campanha, rio, locality_name, method, family) %>%
  summarise(abundance = sum(abundance))

dados_familia_df_largo <- dados_familia_df %>%
  pivot_wider(
    id_cols = c("campanha", "rio", "locality_name", "method"),
    names_from = family,
    values_from = abundance
  )

dados_familia_df_largo[is.na(dados_familia_df_largo)] <- 0.0

write_csv(
  dados_familia_df_largo,
  "dados/processado/matriz_comunidades_familias_imaturos_largo.csv"
)






