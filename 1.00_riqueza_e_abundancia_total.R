# Resumo da base de dados. 
# Quantos insetos foram coletados? 
# Quantos táxons? Em cada Classe, Em cada ordem
# Quais táxons mais e menos representativos? 

############################### Ambiente #######################################
library(tidyverse)
# setwd("C:/Users/Giovana/Desktop/mestrado_giovana/")


# Pasta de tabelas
pasta_tabelas <- "tabelas/dados_descritivos/"
if (!dir.exists(pasta_tabelas)) {
  dir.create(pasta_tabelas, recursive = TRUE)
}

############################## Processamento ###################################

df <- read_csv("dados/processado/imaturos_piata_limpo.csv")

# Geral -----------------------------
n_distinct(df$class)
unique(df$class)

n_distinct(df$order)
unique(df$order)

# Classes --------------------------
sumario_classes <- df %>% 
  group_by(class) %>%
  summarise(
    abundancia_classe = sum(abundance), 
    riqueza_ordens = n_distinct(order),
    riqueza_familias = n_distinct(family)
    ) %>%
  ungroup() %>%
  mutate(abundancia_total = sum(abundancia_classe)) %>%
  mutate(abundancia_relativa = abundancia_classe / abundancia_total)

write_csv(
  sumario_classes,
  file.path(pasta_tabelas, "sumario_classes.csv")
)

# abundancia por ordem ---------------------------------------------------------
sumario_ordens <- df %>% 
  group_by(order) %>%
  summarise(
    abundancia_ordem = sum(abundance),
    riqueza_familias = n_distinct(family)
  ) %>%
  ungroup() %>%
  mutate(
    abundancia_total = sum(abundancia_ordem),
    riqueza_total = sum(riqueza_familias)
  ) %>%
  mutate(
    abundancia_relativa = abundancia_ordem / abundancia_total,
    riqueza_relativa = riqueza_familias /riqueza_total
  ) %>%
  select(-abundancia_total, -riqueza_total)


write_csv(
  sumario_ordens,
  file.path(pasta_tabelas, "sumario_ordens.csv")
)



