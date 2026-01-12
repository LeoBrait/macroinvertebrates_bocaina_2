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
  "ph", "tds", "condutividade", "o2_total", "o2_porcentagem", "temperatura"
)

################################## Carregar dados ##############################
sonda_df <- read_csv("dados/dados_sonda_bocaina.csv") %>%
  rename(
    temperatura = temp,
    o2_total = "o2 total",
    o2_porcentagem = "o2 porcentagem"
  )



indices_df <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv") %>%
  filter(method %in% metodos_de_amostragem) %>%
  select(-method)
bocainas_indexes <- read_csv("tabelas/indice_multimetrico/indice_bocaina_imaturos_piata.csv") 


# Unir dados de índices com dados ambientais
merged_df <- indices_df %>%
  inner_join(sonda_df, by = c("campanha", "rio", "locality_name")) %>%
  inner_join(bocainas_indexes, by = c("campanha", "rio", "locality_name"))
# spearman correlation ph e riqueza ephemeroptera


################################# Checar outliers ##############################

# boxplot de ph por rio
lista_de_plots <- list()
for (variavel in vetor_variaveis_ambiente) {
  
    ggplot(merged_df, aes_string(x = "rio", y = variavel)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, alpha = 0.5) +
      labs(
        title = paste("Boxplot de", variavel, "por localidade"),
        x = "Localidade",
        y = variavel
      ) +
      theme_minimal()
  ggsave(filename = paste0("figuras/sonda/boxplot_", variavel, ".png"), width = 8, height = 6)
}

# remover outliers
merged_df <- merged_df %>%
  filter(ph >= 5.7) %>%
  filter(temperatura < 24) %>%
  filter(condutividade < 250) %>%
  filter(tds < 125)






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
