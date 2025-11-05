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
    filename = paste0("figuras/indice_multimetrico_2_rede/boxplot_", i, ".png"),
    width = 8,
    height = 6
  )

  # Avaliar sensibilidade da métrica a partir p-valor e na sobreposição dos
  # quartis considerando o método de Barbour et al. (1996)
  #
  # a. Caso não haja sobreposição dos interquartis e p < 0.05, a métrica é
  #    considerada sensível ao impacto (score 3).
  # b. Caso haja sobreposição dos interquartis, mas a mediana de ambos os grupos 
  #    estiver fora do intervalo interquartil um do outro e p < 0.05, a métrica
  #    é considerada moderadamente sensível ao impacto (score 2).
  # c. Caso haja sobreposição dos interquartis e a mediana de pelo menos de um
  #    dos grupos estiver fora do intervalo interquartil do outro grupo e
  #    p < 0.05, a métrica é considerada pouco sensível ao impacto (score 1).
  # d. Caso haja sobreposição dos interquartis e as medianas de ambos os grupos
  #    estejam dentro do intervalo interquartil um do outro ou p >= 0.05, a
  #    métrica é considerada insensível ao impacto (score 0).
  dados_resumo <- dados_filtrados %>%
    group_by(impacto) %>%
    summarise(
      Q1 = quantile(get(i), 0.25),
      Q3 = quantile(get(i), 0.75),
      mediana = median(get(i))
    )
    

  lista_p_values[[i]] <- p_value
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
