# 1. Qual a classe mais rica de famílias e mais abundante da base de dados;
# 2. Qual a ordem mais rica de famílias e mais abundante da base de dados;

############################### Ambiente #######################################
library(tidyverse)
setwd("C:/Users/Giovana/Desktop/mestrado_giovana/")
df <- read_csv("dados/processado/imaturos_piata_limpo.csv")

############################## Processamento ###################################

# Geral -----------------------------
n_distinct(df$class)
unique(df$class)

n_distinct(df$order)
unique(df$order)

# Classes --------------------------
sumario_classes_abundancia <- df %>% 
  group_by(class) %>%
  summarise(abundancia_classe = sum(abundance)) %>%
  ungroup() %>%
  mutate(abundancia_total = sum(abundancia_classe)) %>%
  mutate(abundancia_relativa = abundancia_classe / abundancia_total)

write_csv(
  sumario_classes_abundancia,
  "results/abundancias_por_classe.csv"
)

# abundancia por ordem -------------
sumario_ordem_abundancia <- df %>% 
  group_by(order) %>%
  summarise(abundancia_ordem = sum(abundance)) %>%
  ungroup() %>%
  mutate(abundancia_total = sum(abundancia_ordem)) %>%
  mutate(abundancia_relativa = abundancia_ordem / abundancia_total)


# riqueza por ordem ----------------
sumario_ordem_riqueza <- df %>%
  group_by(order) %>%
  summarise(riqueza = n_distinct(family)) %>%
  ungroup() %>%
  mutate(riqueza_total = sum(riqueza )) %>%
  mutate(riqueza_relativa = riqueza /riqueza_total)

# Juntar as tabelas ----------------
df_mesclado_ordens <- 
  merge(
    sumario_ordem_abundancia, 
    sumario_ordem_riqueza
  ) %>%
  select(-abundancia_total, -riqueza_total)

write_csv(
  df_mesclado_ordens,
  "results/abundancia_riqueza_de_familias_por_ordem.csv"
)



