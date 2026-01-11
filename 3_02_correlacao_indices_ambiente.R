library(tidyverse)

metodos_de_amostragem <- c("D Net")
vetor_respostas <- c(
  "aspt", "bmwp", "razao_ept_chironomidae", "relativo_odonata", 
  "relativo_trichoptera", 
  "riqueza_familias_ephemeroptera", 
  "riqueza_familias_trichoptera")


sonda_df <- read_csv("dados/dados_sonda_bocaina.csv")
indices_df <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv") %>%
  filter(method %in% metodos_de_amostragem) %>%
  select(-method)

merged_df <- indices_df %>%
  inner_join(sonda_df, by = c("campanha", "rio", "locality_name"))
# spearman correlation ph e riqueza ephemeroptera


for (resposta in vetor_respostas) {
  cat("Correlacao Spearman entre pH e", resposta, ":\n")
  print(
    cor.test(
      merged_df$ph,
      merged_df[[resposta]],
      method = "spearman"
    )
  )
  cat("\n")
}

for (resposta in vetor_respostas) {
  cat("Correlacao Spearman entre tds e", resposta, ":\n")
  print(
    cor.test(
      merged_df$tds,
      merged_df[[resposta]],
      method = "spearman"
    )
  )
  cat("\n")
}

for (resposta in vetor_respostas) {
  cat("Correlacao Spearman entre condutividade e", resposta, ":\n")
  print(
    cor.test(
      merged_df$condutividade,
      merged_df[[resposta]],
      method = "spearman"
    )
  )
  cat("\n")
}

for (resposta in vetor_respostas) {
  cat("Correlacao Spearman entre o2 e", resposta, ":\n")
  print(
    cor.test(
      merged_df[["o2 total"]],
      merged_df[[resposta]],
      method = "spearman"
    )
  )
  cat("\n")
}
