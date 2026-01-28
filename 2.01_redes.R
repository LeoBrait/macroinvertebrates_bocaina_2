library(tidyverse)

################################## Configurações ###############################
set.seed(1994)
metodos_de_amostragem <- c("D Net", "Pool Net", "Sieve")

# Pastas de figuras
pasta_figuras <- "figuras/redes/comparacao_3_redes/"
if (!dir.exists(pasta_figuras)) {
  dir.create(pasta_figuras, recursive = TRUE)
}

# Pasta de tabelas
pasta_tabelas <- "tabelas/redes/"
if (!dir.exists(pasta_tabelas)) {
  dir.create(pasta_tabelas, recursive = TRUE)
}

################################## Carregar dados ##############################
indices_df_raw <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv")

############################## Pre-processamento ###############################
vetor_respostas <- indices_df_raw %>%
  select(-(campanha:method)) %>%
  names()

indices_df <- indices_df_raw %>%
  filter(method %in% metodos_de_amostragem) %>%
  mutate("coluna_id" = paste0(rio, "_", locality_name, "_", campanha)) %>%
  filter(!coletor_recolhedor_proporcao > 0.4)

################################ Processamento #################################
list_of_plots <- list()
for (i in vetor_respostas) {
  print(paste("Analisando redes para índice:", i))

  dados_filtrados <- indices_df %>%
    select(coluna_id, method, i) %>%
    pivot_wider(names_from = method, values_from = i) %>%
    filter(`D Net` >= 0 & `Pool Net` >= 0 & `Sieve` >= 0)
  
  dados_filtrados_long <- dados_filtrados %>%
    pivot_longer(
      cols = c("D Net", "Pool Net", "Sieve"),
      names_to = "method",
      values_to = i
    )

  # Friedman test
  friedman_test_result <- friedman.test(
    as.formula(paste(i, "~ method | coluna_id")),
    data = dados_filtrados_long
  )


  plot <- ggplot(
    dados_filtrados_long,
    aes(x = method, y = !!sym(i), fill = method)
  ) +
    theme_minimal() +
    geom_boxplot() +
    geom_point(alpha = 0.7) +
    geom_line(aes(group = coluna_id), color = "gray", alpha = 0.5) +
    labs(
      title = i,
      subtitle = paste0(
        "Friedman test p-value: ", round(friedman_test_result$p.value, 4)
      )
    ) 
  
  ggsave(
    filename = paste0(pasta_figuras, i, ".png"),
    plot = plot,
    width = 8,
    height = 6
  )

}

################################### Post roc ###################################

vetor_post_roc <- c("total_macroinvertebrados", "aspt", "riqueza_familias")

for (i in vetor_post_roc) {
  print(paste("Analisando redes (post-hoc) para índice:", i))
  
  dados_filtrados <- indices_df %>%
    select(coluna_id, method, i) %>%
    pivot_wider(names_from = method, values_from = i) %>%
    filter(`D Net` >= 0 & `Pool Net` >= 0 & `Sieve` >= 0)
  
  dados_filtrados_long <- dados_filtrados %>%
    pivot_longer(
      cols = c("D Net", "Pool Net", "Sieve"),
      names_to = "method",
      values_to = i
    )
  
  pairwise.wilcox.test(
    dados_filtrados_long[[i]],
    dados_filtrados_long$method,
    p.adjust.method = "bonferroni",
    paired = TRUE
  ) %>%
    print()
}
