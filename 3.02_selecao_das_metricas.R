library(tidyverse)

################################## Configurações ###############################
pontos_referencia <- c("RiodaBarra-P1/2")
pontos_impactado <- c("RiodaBarra-P2")

metodos_de_amostragem <- c("D Net", "Pool Net")

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
# Remover índice de Pielou por ser muito problemático.
vetor_respostas <- indices_df %>%
  select(-pielou_classes, -pielou_ordens, -pielou_familias) %>%
  select(-(campanha:method)) %>%
  names()

# Adicionar coluna de impacto
indices_df <- indices_df %>%
  mutate(
    impacto = case_when(
      locality_name == pontos_referencia ~ "referencia",
      locality_name == pontos_impactado ~ "impactado",
      TRUE ~ "desconsiderado"
    )
  ) 

# Filtrar dados para análise baseado em rede-D e rede de piscina.
dados_filtrados <- indices_df %>%
  filter(impacto != "desconsiderado") %>%
  filter(method %in% metodos_de_amostragem)

################################### Scores #####################################
lista_p_values <- list()


for (i in vetor_respostas) {

  # Teste de Mann-Whitney
  p_value <- wilcox.test(
    formula = as.formula(paste(i, "~ impacto")),
    data = dados_filtrados
  )$p.value
    
  # Criação do gráfico
  plot <- ggplot(
    dados_filtrados,
    aes_string(
      x = "impacto",
      y = i,
      fill = "impacto"
    )
  ) +
    geom_boxplot() +
    geom_jitter(aes(shape = method), width = 0.2, alpha = 0.6) +
    scale_shape_manual(values = c(1, 2)) +
    theme_minimal() +
    labs(title = i, subtitle = paste0("p = ", round(p_value, 4)))

  # Salvar o gráfico
  ggsave(
    plot,
    filename = paste0(pasta_figuras, "/boxplot_", i, ".png"),
    width = 8,
    height = 6
  )

  lista_p_values[[i]] <- p_value
}



############################## spearman correlations ###########################


correlations <- cor(
  indices_df %>%
    filter(method %in% metodos_de_amostragem) %>%
    select(all_of(vetor_respostas)),
  method = "spearman"
)

# salvar tabela de correlações (apenas a parte inferior da matriz)
correlations_lower <- correlations
correlations_lower[upper.tri(correlations_lower)] <- ""
write_csv(
  as.data.frame(correlations_lower) %>%
    rownames_to_column(var = "indice_vs_indice"),
  file.path(pasta_tabelas, "correlacoes_spearman_indices_imaturos_piata.csv")
)

################################################################################
