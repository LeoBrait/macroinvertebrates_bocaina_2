library(tidyverse)

################################## Configurações ###############################
set.seed(1994)
metodos_de_amostragem <- c("D Net", "Pool Net", "Sieve")
pontos_impactado <- c("RiodaBarra-P2", "RioMutuca-P3")


if (length(metodos_de_amostragem) == 3) {
  pasta_figuras <- "figuras/redes/glmm_promessa/"
  pasta_tabelas <- "tabelas/redes/glmm_promessa/"
} 


# Pastas de figuras
if (!dir.exists(pasta_figuras)) {
  dir.create(pasta_figuras, recursive = TRUE)
}

# Pasta de tabelas
if (!dir.exists(pasta_tabelas)) {
  dir.create(pasta_tabelas, recursive = TRUE)
}

################################## Carregar dados ##############################
indices_df_raw <- 
  read_csv("dados/processado/matriz_indices_imaturos_piata.csv") %>%
  mutate()

############################## Pre-processamento ###############################
vetor_respostas <- indices_df_raw %>%
  select(-(campanha:method)) %>%
  names()

indices_df <- indices_df_raw %>%
  filter(method %in% metodos_de_amostragem) %>%
  mutate("coluna_id" = paste0(rio, "_", locality_name, "_", campanha)) %>%
    mutate(
    id_tempo_espaço = paste0(campanha, rio, locality_name),
    periodo = case_when(
      campanha %in% c("campanha_1", "campanha_2", "campanha_4") ~ "seco",
      campanha %in% c("campanha_3") ~ "chuvoso",
      TRUE ~ "desconecido"
    ),
    barramento = case_when(
        locality_name %in% pontos_impactado ~ "barrado",
        TRUE ~ "não_barrado"
    )
  ) 



categorias <- indices_df %>%
  select(coluna_id, periodo, rio, locality_name) %>%
  distinct()

################################ Processamento #################################
list_of_plots <- list()
i <- "riqueza_ordens"
for (i in vetor_respostas) {
  print(paste("Analisando redes para índice:", i))

  
  dados_filtrados <- indices_df %>%
    select(coluna_id, method, i) %>%
    pivot_wider(names_from = method, values_from = i) %>%
    filter(if_all(c(`D Net`, `Pool Net`, `Sieve`), is.finite))
  
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
  
  dados_filtrados_long_categorias <- dados_filtrados_long %>%
    left_join(categorias, by = "coluna_id")


  plot <- ggplot(
    dados_filtrados_long_categorias,
    aes(x = method, y = !!sym(i))
  ) +
    theme_minimal() +
    geom_boxplot() +
    geom_point(
        aes(
        color = factor(rio),
        shape = factor(periodo)
        ),
        #fill = "gray70", 
        size = 10,
        alpha = 0.7,
        stroke = 2) +
    scale_shape_manual(values = c(4, 21)) +
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

for (i in vetor_respostas) {
  print(paste("Analisando redes (post-hoc) para índice:", i))

  if (i == "coletor_recolhedor_proporcao") {
    indices_df <- indices_df %>%
      filter(!coletor_recolhedor_proporcao > 0.4)
  }
  
  dados_filtrados <- indices_df %>%
    select(coluna_id, method, i) %>%
    pivot_wider(names_from = method, values_from = i)
  
  dados_filtrados_long <- dados_filtrados %>%
    pivot_longer(
      cols = c("D Net", "Pool Net", "Sieve"),
      names_to = "method",
      values_to = i
    )
  
  tabela_post_hoc <- pairwise.wilcox.test(
    dados_filtrados_long[[i]],
    dados_filtrados_long$method,
    p.adjust.method = "bonferroni",
    paired = TRUE
  )
  write_csv(
    as.data.frame(tabela_post_hoc$p.value) %>%
      rownames_to_column(var = "method1"),
    paste0(pasta_tabelas,  "wilxox_matrix_", i, ".csv")
  )
}
