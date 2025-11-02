library(tidyverse)


indices_df <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv")

vetor_respostas <- indices_df %>%
  select(-pielou_classes, -pielou_ordens, -pielou_familias) %>%
  select(-(campanha:method)) %>%
  names()

indices_df <- indices_df %>%
  mutate(
    impacto = case_when(
      locality_name == "RiodaBarra-P2" ~ "impacted",
      TRUE ~ "non-impacted"
    )
  )

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



################################### Scores #####################################











#list to store plots
lista_plotas <- list()

for (i in vetor_respostas) {
  p <- ggplot(
    indices_df %>% filter(method == "D Net" | method == "Pool Net"),
    aes_string(
      x = "impacto",
      y = i,
      fill = "impacto"
    )) +
    labs(title = i) +
    geom_boxplot() +
    theme_minimal() +
    geom_jitter()
  
  lista_plotas[[i]] <- p
}

# save all plots
for (i in vetor_respostas) {
  ggsave(
    filename = paste0("figuras/indice_multimetrico/boxplot_", i, ".png"),
    plot = lista_plotas[[i]],
    width = 8,
    height = 6
  )
}




################################################################################
