library(tidyverse)



################################## Configurações ###############################
set.seed(1994)
metodos_de_amostragem <- c("D Net")
vetor_respostas <- c(
 "aspt", 
  "bmwp", 
  #"filtrador_proporcao",
  #"raspadores_proporcao",
  #"razao_ept_chironomidae",
  "relativo_odonata",
  "relativo_trichoptera",
  "shannon_familias")
vetor_variaveis_ambiente <- c(
"ocupacao_margens",
"largura_mata_ciliar",
"estabilidade_margens",
"cobertura_vegetal_leito",
"odor",
"oleosidade_espumas",
"presenca_animais_domesticos",
"plantas_aquaticas",
"conectividade_rio",
"represamento_rio",
"deposicao_sedimentos_fluxo_rio",
"disponibilidade_substratos_microhabitats",
"pontuacao_total"
)

# Pastas de figuras
pasta_figuras <- "figuras/cap2/indice_multimetrico/indice_bocaina/"
if (!dir.exists(pasta_figuras)) {
  dir.create(pasta_figuras, recursive = TRUE)
}

# Pasta de tabelas
pasta_tabelas <- "tabelas/cap2/indice_multimetrico/indice_bocaina/"
if (!dir.exists(pasta_tabelas)) {
  dir.create(pasta_tabelas, recursive = TRUE)
}

################################## Carregar dados ##############################
analise_entorno_df <- read_csv("dados/dados_analise_entorno_bocaina.csv")


bocainas_indexes <- read_csv("tabelas/cap2/indice_multimetrico/indice_bocaina/indice_bocaina_imaturos_piata.csv")




# Unir dados de índices com dados ambientais
merged_df <- bocainas_indexes %>%
  left_join(analise_entorno_df, by = c("rio", "locality_name"))


################################## Remover SD = 0 ##############################
sd_zero <- sapply(merged_df, function(x) {
  is.numeric(x) && sd(x, na.rm = TRUE) == 0
})

vetor_variaveis_ambiente <- vetor_variaveis_ambiente[
  !sd_zero[vetor_variaveis_ambiente]
]

variaveis_removidas <- names(sd_zero)[sd_zero]
variaveis_removidas

################################ Análise de correlação #########################
response_df <- data.frame()

for (ambiental in vetor_variaveis_ambiente) {
    for (resposta in vetor_respostas) {
    print(paste("Analisando correlação entre", ambiental, "e", resposta))
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
          r_quadrado = cor_test$estimate^2,
          p_value = cor_test$p.value
        )
      )
    }
}

write_csv(
  response_df,
  file.path(pasta_tabelas, "correlacao_indices_vs_formulario.csv")
)
