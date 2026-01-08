library(tidyverse)



################################## Configurações ###############################
set.seed(1994)
pontos_referencia <- c("RiodaBarra-P1/2", "RioMutuca-P1")
pontos_impactado <- c("RiodaBarra-P2", "RioMutuca-P3")

metodos_de_amostragem <- c("D Net")
vetor_respostas <- c(
  "aspt", "bmwp", "razao_ept_chironomidae", "relativo_odonata", 
  "relativo_trichoptera", 
  "riqueza_familias_ephemeroptera", 
  "riqueza_familias_trichoptera")

nota_maxima <- length(vetor_respostas) * 5

# Pastas de figuras
pasta_figuras <- "figuras/indice_multimetrico"
if (!dir.exists(pasta_figuras)) {
  dir.create(pasta_figuras, recursive = TRUE)
}

# Pasta de tabelas
pasta_tabelas <- "tabelas/indice_multimetrico"
if (!dir.exists(pasta_tabelas)) {
  dir.create(pasta_tabelas, recursive = TRUE)
}

################################## Carregar dados ##############################
indices_df <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv")


############################## Pre-processamento ###############################
# Adicionar coluna de impacto
indices_df <- indices_df %>%
  mutate(
    impacto = case_when(
      locality_name %in% pontos_referencia ~ "referencia",
      locality_name %in% pontos_impactado ~ "impactado",
      TRUE ~ "desconsiderado"
    )
  ) 

dados_filtrados <- indices_df %>%
  filter(impacto != "desconsiderado") %>%
  filter(method %in% metodos_de_amostragem) %>%
  mutate(
    impacto = as.factor(impacto),
    method = as.factor(method)
  )



############################### Detectando limiares ############################
estatisticas_df <- data.frame()


for (i in vetor_respostas) {
  print(paste("Analisando índice:", i))
  
  estatisticas <- dados_filtrados %>%
    filter(impacto == "referencia") %>%
    mutate(
      minimo = min(.data[[i]]),
      quartil_25 = quantile(.data[[i]], 0.25, na.rm = TRUE),
      quartil_50 = quantile(.data[[i]], 0.50, na.rm = TRUE),
      quartil_75 = quantile(.data[[i]], 0.75, na.rm = TRUE),
      maximo = max(.data[[i]]),
      metade = quartil_25 / 2
      ) %>%
    mutate(metrica = i) %>%
    select(
      metrica, quartil_25, quartil_50, quartil_75, maximo, minimo, metade
    ) %>%
    group_by(metrica) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)

  estatisticas_df <- rbind(estatisticas_df, estatisticas)
}

write_csv(
  estatisticas_df,
  file.path(pasta_tabelas, "limiares_indices_imaturos_piata.csv")
)
############################### Aplicando a taela completa #####################
statisticas_df_limpa <- estatisticas_df %>%
  select(metrica, quartil_25, metade)

indices_rebalanceados_df <- indices_df %>%
  filter(method %in% metodos_de_amostragem) %>%
  select(campanha, rio, locality_name, vetor_respostas) %>%
  pivot_longer(
    cols = all_of(vetor_respostas),
    names_to = "metrica",
    values_to = "valor"
  ) %>%
  left_join(statisticas_df_limpa, by = "metrica") %>%
  mutate(
    nota = case_when(
      valor >= quartil_25 ~ 5,
      valor >= metade & valor < quartil_25 ~ 3,
      valor < metade ~ 1,
      TRUE ~ NA_real_
    )
  ) 



giovanas_index <- indices_rebalanceados_df %>%
  group_by(campanha, rio, locality_name) %>%
  summarise(giovanas_index = sum(nota)) %>%
  ungroup() %>%
  mutate(giovanas_index_relative = giovanas_index / nota_maxima * 100)



str(giovanas_index)

# Atribuir notas


giovanas_index %>% 
  ggplot(aes(x = campanha, y = giovanas_index_relative, fill = campanha)) +
  geom_line(aes(group = 1)) +
  facet_wrap(~ interaction(rio, locality_name), scales = "free_x") +
  labs(
    x = "Localidade",
    y = "Índice Bocaina",
    fill = "Rio"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylim(0, NA)


ggsave(
  filename = file.path(
    pasta_figuras,
    "indice_giovana_imaturos_piata.png"
  ),
  width = 10,
  height = 6
)
