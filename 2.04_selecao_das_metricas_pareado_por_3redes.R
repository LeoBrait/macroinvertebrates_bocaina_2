library(tidyverse)

################################## Configurações ###############################
set.seed(1994)
pontos_referencia <- c("RiodaBarra-P1/2")
pontos_impactado <- c("RiodaBarra-P2")
metodos_de_amostragem <- c("D Net", "Pool Net", "Sieve")

# Pastas de figuras
pasta_figuras <- "figuras/indice_multimetrico/pareado_por_3_redes"
if (!dir.exists(pasta_figuras)) {
  dir.create(pasta_figuras, recursive = TRUE)
}

# Pasta de tabelas
pasta_tabelas <- "tabelas/redes/"
if (!dir.exists(pasta_tabelas)) {
  dir.create(pasta_tabelas, recursive = TRUE)
}

################################## Carregar dados ##############################
indices_df <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv")

############################## Pre-processamento ###############################
# Remover índice de Pielou por ser muito problemático.
vetor_respostas <- indices_df %>%
  select(-pielou_classes, -pielou_ordens, -pielou_familias) %>%
  select(-(campanha:method)) %>%
  names()

# Adicionar coluna de impacto
dados_filtrados <- indices_df %>%
  mutate(
    impacto = case_when(
      locality_name == pontos_referencia ~ "referencia",
      locality_name == pontos_impactado ~ "impactado",
      TRUE ~ "desconsiderado"
    )
  )

# Filtrar dados para análise baseado em rede-D e rede de piscina.
dados_filtrados <- dados_filtrados %>%
  filter(impacto != "desconsiderado") %>%
  filter(method %in% metodos_de_amostragem) #%>%
  #mutate(
  #  impacto = as.factor(impacto),
  #  method = as.factor(method)
  #)

dados_filtrados <- dados_filtrados %>%
  mutate("coluna_id" = paste0(campanha, "_", method))

str(indices_df)


################################################################################
i <- "bmwp"
for (i in vetor_respostas) {
  print(paste("Analisando impacto (pareado por redes) para índice:", i))
  
  dados_friedman <- dados_filtrados %>%
  filter(method %in% c("D Net", "Pool Net", "Sieve")) %>%
  select(campanha, method, all_of(i)) %>%
  drop_na()

  dados_friedman <- dados_friedman %>%
  group_by(campanha) %>%
  filter(n_distinct(method) == 3) %>%
  ungroup()

 friedman_res <- friedman.test(
  formula = as.formula(paste(i, "~ method | campanha")),
  data = dados_friedman
  )

p_value <- friedman_res$p.value

  # Teste de Wilcoxon pareado
  p_value <- wilcox.test(
    pares_validos[["referencia"]],
    pares_validos[["impactado"]],
    paired = TRUE
  )$p.value
  
pairwise.wilcox.test(
  x = dados_friedman[[i]],
  g = dados_friedman$method,
  paired = TRUE,
  p.adjust.method = "holm"
)

  # Criação do gráfico (continua igual, formato longo)
  plot <- ggplot(dados_friedman, aes(x = method, y = .data[[i]])) +
  geom_boxplot() +
  geom_point(aes(group = campanha), alpha = 0.7) +
  geom_line(aes(group = campanha), color = "gray", alpha = 0.5) +
  theme_minimal() +
  labs(
    title = i,
    subtitle = paste0("Friedman: p = ", round(p_value, 4))
  )

  # Salvar o gráfico
  ggsave(
    plot,
    filename = paste0(pasta_figuras, "/boxplot_", i, ".png"),
    width = 8,
    height = 6
  )
}
