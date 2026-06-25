library(tidyverse)
library(patchwork)
################################## Configurações ###############################

set.seed(1994)

pontos_referencia <- c("RiodaBarra-P1/2", "RioMutuca-P1")
pontos_impactado <- c("RiodaBarra-P2", "RioMutuca-P3")

metodos_de_amostragem <- c("D Net")

vetor_respostas <- c(
  "aspt",
  "bmwp",
  "razao_ept_chironomidae",
  "relativo_odonata", 
  "relativo_trichoptera", 
  "riqueza_familias_ephemeroptera", 
  "riqueza_familias_trichoptera"
)

nomes_metricas <- c(
  aspt = "ASPT",
  bmwp = "BMWP'",
  razao_ept_chironomidae = "EPT/Chironomidae",
  relativo_odonata = "% Odonata",
  relativo_trichoptera = "% Trichoptera",
  riqueza_familias_ephemeroptera = "Riqueza Ephemeroptera",
  riqueza_familias_trichoptera = "Riqueza Trichoptera"
)

nota_maxima <- length(vetor_respostas) * 5
nota_minima <- length(vetor_respostas)

pasta_figuras <- "figuras/indice_multimetrico"
if (!dir.exists(pasta_figuras)) {
  dir.create(pasta_figuras, recursive = TRUE)
}

pasta_tabelas <- "tabelas/indice_multimetrico"
if (!dir.exists(pasta_tabelas)) {
  dir.create(pasta_tabelas, recursive = TRUE)
}

################################## Carregar dados ##############################


indices_df <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv")

############################## Pré-processamento ###############################

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
    impacto = factor(impacto),
    method = factor(method)
  )

############################### Detectando limiares ############################

estatisticas_df <- data.frame()

for (i in vetor_respostas) {
  
  print(paste("Analisando métrica:", i))
  
  estatisticas <- dados_filtrados %>%
    filter(impacto == "referencia") %>%
    summarise(
      metrica = i,
      minimo = min(.data[[i]], na.rm = TRUE),
      quartil_25 = quantile(.data[[i]], 0.25, na.rm = TRUE),
      quartil_50 = quantile(.data[[i]], 0.50, na.rm = TRUE),
      quartil_75 = quantile(.data[[i]], 0.75, na.rm = TRUE),
      maximo = max(.data[[i]], na.rm = TRUE),
      metade = quartil_25 / 2
    )
  
  estatisticas_df <- bind_rows(estatisticas_df, estatisticas)
}

write_csv(
  estatisticas_df,
  file.path(pasta_tabelas, "limiares_indices_imaturos_piata.csv")
)

############################### Aplicando pontuação ############################

estatisticas_df_limpa <- estatisticas_df %>%
  select(metrica, quartil_25, metade)

indices_rebalanceados_df <- indices_df %>%
  filter(method %in% metodos_de_amostragem) %>%
  select(campanha, rio, locality_name, all_of(vetor_respostas)) %>%
  pivot_longer(
    cols = all_of(vetor_respostas),
    names_to = "metrica",
    values_to = "valor"
  ) %>%
  left_join(estatisticas_df_limpa, by = "metrica") %>%
  mutate(
    nota = case_when(
      valor >= quartil_25 ~ 5,
      valor >= metade & valor < quartil_25 ~ 3,
      valor < metade ~ 1,
      TRUE ~ NA_real_
    )
  )

############################### Cálculo do IMM completo ########################
IMM_min <- 7
IMM_max <- 35

giovanas_index <- indices_rebalanceados_df %>%
  group_by(campanha, rio, locality_name) %>%
  summarise(
    IMM = sum(nota, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    IMM_percentual = (IMM - nota_minima) / (nota_maxima - nota_minima) * 100
  )

write_csv(
  giovanas_index,
  file.path(pasta_tabelas, "indice_bocaina_imaturos_piata.csv")
)

############################### Dados para regressão ###########################

dados_regressao <- indices_df %>%
  filter(method %in% metodos_de_amostragem) %>%
  select(campanha, rio, locality_name, method, all_of(vetor_respostas)) %>%
  left_join(
    giovanas_index %>%
      select(campanha, rio, locality_name, IMM, IMM_percentual),
    by = c("campanha", "rio", "locality_name")
  ) %>%
  drop_na(IMM_percentual)

############################### Regressões lineares ############################

resultados_regressao <- data.frame()

for (i in vetor_respostas) {
  
  modelo <- lm(
    as.formula(paste("IMM ~", i)),
    data = dados_regressao
  )
  
  resumo <- summary(modelo)
  
  resultados_regressao <- bind_rows(
    resultados_regressao,
    data.frame(
      metrica = i,
      nome_metrica = nomes_metricas[i],
      beta = coef(modelo)[2],
      intercepto = coef(modelo)[1],
      r2 = resumo$r.squared,
      r2_ajustado = resumo$adj.r.squared,
      p_valor = coef(summary(modelo))[2, 4]
    )
  )
}

resultados_regressao <- resultados_regressao %>%
  mutate(
    significancia = case_when(
      p_valor < 0.001 ~ "***",
      p_valor < 0.01 ~ "**",
      p_valor < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  arrange(desc(r2))

write_csv(
  resultados_regressao,
  file.path(pasta_tabelas, "regressao_metricas_IMM.csv")
)

resultados_regressao

############################### Gráfico de R² #################################

grafico_r2 <- resultados_regressao %>%
  ggplot(
    aes(
      x = reorder(nome_metrica, r2),
      y = r2
    )
  ) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Métrica",
    y = expression(R^2),
    title = "Proporção da variação do IMM explicada por cada métrica"
  ) +
  theme_bw()

grafico_r2

ggsave(
  filename = file.path(pasta_figuras, "regressao_R2_metricas_IMM.png"),
  plot = grafico_r2,
  width = 9,
  height = 6,
  dpi = 300
)

############################### Painel de regressões ###########################

dados_regressao_long <- dados_regressao %>%
  select(campanha, rio, locality_name, IMM_percentual, all_of(vetor_respostas)) %>%
  pivot_longer(
    cols = all_of(vetor_respostas),
    names_to = "metrica",
    values_to = "valor_metrica"
  ) %>%
  mutate(
    nome_metrica = nomes_metricas[metrica]
  ) %>%
  left_join(
    resultados_regressao %>%
      select(metrica, r2, p_valor, beta),
    by = "metrica"
  ) %>%
  mutate(
    etiqueta = paste0(
      "R² = ", round(r2, 2),
      "\np = ", signif(p_valor, 2)
    )
  )

painel_regressoes <- ggplot(
  dados_regressao_long,
  aes(
    x = valor_metrica,
    y = IMM_percentual
  )
) +
  geom_point(
    size = 2.5,
    alpha = 0.8
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  geom_text(
    data = dados_regressao_long %>%
      group_by(nome_metrica) %>%
      slice(1) %>%
      ungroup(),
    aes(
      x = Inf,
      y = Inf,
      label = etiqueta
    ),
    inherit.aes = FALSE,
    hjust = 1.1,
    vjust = 1.3,
    size = 3.5
  ) +
  facet_wrap(
    ~ nome_metrica,
    scales = "free_x",
    ncol = 3
  ) +
  labs(
    x = "Valor da métrica",
    y = "IMM (%)",
    title = "Relação entre as métricas biológicas e o IMM"
  ) +
  theme_bw()

painel_regressoes

ggsave(
  filename = file.path(pasta_figuras, "painel_regressao_metricas_IMM.png"),
  plot = painel_regressoes,
  width = 12,
  height = 8,
  dpi = 300
)