library(tidyverse)

metodos_de_amostragem <- c("D Net")
vetor_respostas <- c(
  "aspt", "bmwp", "razao_ept_chironomidae", "relativo_odonata", 
  "relativo_trichoptera", 
  "riqueza_familias_ephemeroptera", 
  "riqueza_familias_trichoptera")


sonda_df <- read_csv("dados/dado_sonda_bocaina.csv")
indices_df <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv") %>%
  filter(method %in% metodos_de_amostragem) %>%
  select(-method)

merged_df <- indices_df %>%
  inner_join(sonda_df, by = c("campanha", "rio", "locality_name"))
# spearman correlation ph e riqueza ephemeroptera

cor.test(
  sonda_df$ph,
  indices_df$riqueza_familias_ephemeroptera,
  method = "spearman"
)
