

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
sumario_classes_abundancia <- df %>% 
  group_by(class) %>%
  summarise(abundancia_classe = sum(abundance)) %>%
  ungroup() %>%
  mutate(abundancia_total = sum(abundancia_classe)) %>%
  mutate(abundancia_relativa = abundancia_classe / abundancia_total)

write_csv(
  sumario_classes_abundancia,
  file.path(pasta_tabelas, "abundancias_por_classe.csv")
)

# abundancia por ordem -------------
sumario_ordem_abundancia_riqueza <- df %>% 
  group_by(order) %>%
  summarise(
    abundancia_ordem = sum(abundance),
    riqueza = n_distinct(family)
  ) %>%
  ungroup() %>%
  mutate(
    abundancia_total = sum(abundancia_ordem),
    riqueza_total = sum(riqueza)
  ) %>%
  mutate(
    abundancia_relativa = abundancia_ordem / abundancia_total,
    riqueza_relativa = riqueza /riqueza_total
  ) #%>%
  select(-abundancia_total, -riqueza_total)

write_csv(
  sumario_ordem_abundancia_riqueza,
  file.path(pasta_tabelas, "sumario_ordem_abundancia_riqueza.csv")
)



