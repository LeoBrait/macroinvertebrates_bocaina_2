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

for (i in vetor_respostas) {
  print(paste("Analisando impacto (pareado por redes) para índice:", i))
  
  # Pivotar dados para formato largo — uma coluna para cada método
  pares <- dados_filtrados %>%
    select(all_of(c("campanha", "impacto", "method", i))) %>%
    pivot_wider(names_from = impacto, values_from = all_of(i)) %>%
    mutate(coluna_id = paste0(campanha, "_", method))
  
  # Garantir que temos pares válidos (sem NA)
  pares_validos <- pares %>%
    filter(!is.na("D Net") & !is.na("Pool Net") & !is.na("Sieve"))
  

  # Teste de Wilcoxon pareado
  p_value <- wilcox.test(
    pares_validos[["referencia"]],
    pares_validos[["impactado"]],
    paired = TRUE
  )$p.value
  
  pares_validos_long <- pares_validos %>%
  pivot_longer(
    cols = c("referencia", "impactado"),
    names_to = "impacto",
    values_to = i
  )

  # Criação do gráfico (continua igual, formato longo)
  plot <- ggplot(
    pares_validos_long,
    aes_string(x = "impacto", y = i, fill = "impacto")
  ) +
    theme_minimal() +
    geom_boxplot() +
    geom_point(aes(shape = method),alpha = 0.7) +
    labs(
      title = i,
      subtitle = paste0("Teste de Wilcoxon pareado: p = ", round(p_value, 4))
    ) +
    geom_line(aes_string(group = "coluna_id"), color = "gray", alpha = 0.5)
  
  # Salvar o gráfico
  ggsave(
    plot,
    filename = paste0(pasta_figuras, "/boxplot_", i, ".png"),
    width = 8,
    height = 6
  )
}