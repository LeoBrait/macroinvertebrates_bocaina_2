library(tidyverse)

################################## Configurações ###############################

metodos_de_amostragem <- c("D Net", "Pool Net")

# Pastas de figuras
pasta_figuras <- "figuras/redes/"
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

dados_filtrados <- indices_df %>%
  filter(method %in% metodos_de_amostragem) %>%
  mutate("coluna_id" = paste0(rio, "_", locality_name, "_", campanha))

str(indices_df)

################################################################################



for (i in vetor_respostas) {
  print(paste("Analisando redes (pareado) para índice:", i))

  # Pivotar dados para formato largo — uma coluna para cada método
  pares <- dados_filtrados %>%
    select(all_of(c("coluna_id", "method", i))) %>%
    pivot_wider(names_from = method, values_from = all_of(i))

  # Garantir que temos pares válidos (sem NA)
  pares_validos <- pares %>%
    filter(!is.na("D Net") & !is.na("Pool Net"))

  ids_validos <- pares_validos$coluna_id
  dados_filtrados <- dados_filtrados %>%
    filter(coluna_id %in% ids_validos)

  # Teste de Wilcoxon pareado
  p_value <- wilcox.test(
    pares_validos[["D Net"]],
    pares_validos[["Pool Net"]],
    paired = TRUE
  )$p.value

  # Criação do gráfico (continua igual, formato longo)
  plot <- ggplot(
    dados_filtrados,
    aes_string(
      x = "method",
      y = i,
      fill = "method"
    )
  ) +
    theme_minimal() +
    geom_boxplot() +
    geom_point(alpha = 0.7) +
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
