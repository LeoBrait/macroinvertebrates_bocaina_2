library(tidyverse)


indices_df <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv")

vetor_respostas <- indices_df %>%
  select(-pielou_classes, -pielou_ordens, -pielou_familias) %>%
  select(-(campanha:method)) %>%
  names()

indices_df <- indices_df %>%
  mutate(
    impacto = case_when(
      locality_name == "RiodaBarra-P1/2" ~ "referencia",
      locality_name == "RiodaBarra-P2" ~ "impactado",
      TRUE ~ "desconsiderado"
    )
  ) 
  
dados_filtrados <- indices_df %>%
  filter(impacto != "desconsiderado") %>%
  filter(method %in% c("D Net", "Pool Net"))

################################### Scores #####################################
lista_plots <- list()


for (i in vetor_respostas) {

  # filtra dados válidos
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
  
  # salva o gráfico na lista
  lista_plots[[i]] <- plot
}

# salvar todos os gráficos
for (i in vetor_respostas) {
  ggsave(
    filename = paste0("figuras/indice_multimetrico_2_rede/boxplot_", i, ".png"),
    plot = lista_plots[[i]],
    width = 8,
    height = 6
  )
}


############################## spearman correlations ###########################


correlations <- cor(
  indices_df %>%
    filter(method == "D Net" | method == "Pool Net") %>%
    select(all_of(vetor_respostas)),
  method = "spearman"
) 

# salvar tabela de correlações (apenas a parte inferior da matriz)
correlations_lower <- correlations
correlations_lower[upper.tri(correlations_lower)] <- ""
write_csv(
  as.data.frame(correlations_lower) %>%
    rownames_to_column(var = "indice_vs_indice"),
  "correlacoes_spearman_indices_imaturos_piata.csv"
)

################################################################################
