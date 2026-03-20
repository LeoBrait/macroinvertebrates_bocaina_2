library(tidyverse)



################################## Configurações ###############################
set.seed(1994)
metodos_de_amostragem <- c("D Net")
vetor_respostas <- c(
  "aspt", "bmwp", "razao_ept_chironomidae", "relativo_odonata", 
  "relativo_trichoptera", 
  "riqueza_familias_ephemeroptera", 
  "riqueza_familias_trichoptera",
  "giovanas_index")
vetor_variaveis_ambiente <- c(
  "floresta_densa_100m",
  "floresta_aberta_100m",
  "pastagem_100m",
  "agricultura_pastagem_100m",
  "floresta_densa_50m",
  "floresta_aberta_50m",
  "pastagem_50m",
  "agricultura_pastagem_50m",
  "floresta_densa_25m",
  "floresta_aberta_25m",
  "pastagem_25m",
  "agricultura_pastagem_25m"
)

################################## Carregar dados ##############################
satelite_df <- read_csv("dados/dados_satelite_bocaina.csv")


indices_df <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv") %>%
  filter(method %in% metodos_de_amostragem) %>%
  select(-method)
bocainas_indexes <- read_csv("tabelas/indice_multimetrico/indice_bocaina_imaturos_piata.csv") 


# Unir dados de índices com dados ambientais
merged_df <- indices_df %>%
  left_join(satelite_df, by = c("rio", "locality_name")) %>%
  inner_join(bocainas_indexes, by = c("campanha", "rio", "locality_name"))



################################ Análise de correlação #########################
response_df <- data.frame()

for (ambiental in vetor_variaveis_ambiente) {
    for (resposta in vetor_respostas) {
      cor_test <- cor.test(
        merged_df[[ambiental]],
        merged_df[[resposta]],
        method = "pearson"
      )
      
      response_df <- rbind(
        response_df,
        data.frame(
          variavel_ambiental = ambiental,
          resposta = resposta,
          correlacao = cor_test$estimate,
          p_value = cor_test$p.value
        )
      )
    }
}

write_csv(
  response_df,
  file.path("tabelas/correlacao_indices_vs_satelite.csv")
)
