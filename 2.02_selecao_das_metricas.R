
library(tidyverse)

################################## Configurações ###############################
set.seed(1994)
pontos_referencia <- c("RiodaBarra-P1/2", "RioMutuca-P1")
pontos_impactado <- c("RiodaBarra-P2", "RioMutuca-P3")

metodos_de_amostragem <- c("D Net")

vetor_estetica_indice <- c(
  "razao_baetidae_ephemeroptera" = "Razão Baetidae/Ephemeroptera",
  "razao_ept_chironomidae" = "Razão EPT/Chironomidae",
  "relativo_chol" = "(%) CHOL",
  "relativo_chironomidae" = "(%) Chironomidae",
  "relativo_ephemeroptera" = "(%) Ephemeroptera",
  "relativo_plecoptera" = "(%) Plecoptera",
  "relativo_trichoptera" = "(%) Trichoptera",
  "relativo_coleoptera" = "(%) Coleoptera",
  "relativo_diptera" = "(%) Diptera",
  "relativo_odonata" = "(%) Odonata",
  "relativo_insecta" = "(%) Insecta",
  "relativo_oligochaeta" = "(%) Oligochaeta",
  "relativo_gastropoda" = "(%) Gastropoda",
  "relativo_crustacea" = "(%) Crustacea",
  "riqueza_classes" = "Classes Taxa",
  "riqueza_ordens" = "Ordens Taxa",
  "riqueza_familias" = "Famílias Taxa",
  "shannon_classes" = "Classes Taxa",
  "shannon_ordens" = "Shannon de Ordens",
  "shannon_familias" = "Shannon de Famílias",
  "pielou_classes" = "Pielou de Classes",
  "pielou_ordens" = "Pielou de Ordens",
  "pielou_familias" = "Pielou de Famílias",
  "riqueza_familias_ephemeroptera" = "Ephemeroptera Taxa",
  "riqueza_familias_trichoptera" = "Trichoptera Taxa",
  "riqueza_familias_plecoptera" = "Plecoptera Taxa",
  "riqueza_familias_coleoptera" = "Coleoptera Taxa",
  "ept_riqueza" = "EPT Taxa",
  "bmwp" = "BMWP",
  "aspt" = "ASPT",
  "coletor_recolhedor_proporcao" = "(%) Coletor-Recolhedor",
  "filtrador_proporcao" = "(%) Filtradores",
  "predador_proporcao" = "(%) Predadores",
  "perfuradores_proporcao" = "(%) Perfuradores",
  "fragmentadores_proporcao" = "(%) Fragmentadores",
  "raspadores_proporcao" = "(%) Raspadores",
  "detritivoros_proporcao" = "(%) Detritívoros",
  "herbivoros_proporcao" = "(%) Herbívoros"
)

vetor_estetica_impacto <- c(
  "referencia" = "Referência",
  "impactado" = "Impactado"
)

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
vetor_respostas <- indices_df %>%
  select(-(campanha:method)) %>%
  names()

# Adicionar coluna de impacto
indices_df <- indices_df %>%
  mutate(
    impacto = case_when(
      locality_name %in% pontos_referencia ~ "referencia",
      locality_name %in% pontos_impactado ~ "impactado",
      TRUE ~ "desconsiderado"
    )
  ) 

# Filtrar dados para análise baseado em rede-D.
dados_filtrados <- indices_df %>%
  filter(impacto != "desconsiderado") %>%
  filter(method %in% metodos_de_amostragem) %>%
  mutate(impacto = as.factor(impacto), method = as.factor(method))

str(dados_filtrados)
################################### Scores #####################################

for (i in vetor_respostas) {
  print(paste("Analisando índice:", i))
  
  test <- dados_filtrados %>%
    select(c("impacto", i))


  # Teste de Mann-Whitney
  teste <- wilcox.test(
    formula = as.formula(paste(i, "~ impacto")),
    data = dados_filtrados
  )
 
  p_value = teste$p.value
  w <- teste$statistic

  # Tamanhos das amostras
  n1 <- dados_filtrados %>%
    filter(impacto == levels(dados_filtrados$impacto)[1]) %>% 
    nrow()
  n2 <- dados_filtrados %>% 
    filter(impacto == levels(dados_filtrados$impacto)[2]) %>% 
    nrow()

  # Cálculo do U
  u <- w - n1 * (n1 + 1) / 2
  
  
  # Criação do gráfico
  nome_indice <- vetor_estetica_indice[[i]]

  plot <- ggplot(
    dados_filtrados,
    aes_string(
      x = "impacto",
      y = i
    )
  ) +
    theme_minimal() +
    geom_boxplot() +
    geom_jitter(width = 0.2, height = 0, alpha = 1) +
    scale_x_discrete(labels = vetor_estetica_impacto) +
    theme(
      text = element_text(size = unit(25, "pt")),
      axis.title = element_text(size = unit(25, "pt"), face = "bold"),
      plot.title = element_text(size = unit(25, "pt"), face = "bold"),
      plot.subtitle = element_text(size = unit(25, "pt"), face = "bold")
    ) +
  # Adicionar títulos e legendas
    labs(
      x = "Impacto",
      y = nome_indice,
      subtitle = paste0(
        "p-value = ", round(p_value, 4),
        ", U-test = ", round(u, 4)
      )
    ) 

  # Salvar o gráfico
  ggsave(
    plot,
    filename = paste0(pasta_figuras, "/boxplot_", i, ".png"),
    width = 8,
    height = 8
  )

    ggsave(
    plot,
    filename = paste0(pasta_figuras, "/boxplot_", i, ".svg"),
    width = 8,
    height = 8
  )
}



############################## spearman correlations ###########################
dados_filtrados_numerico <- dados_filtrados %>%
  select(all_of(vetor_respostas))

# Função para criar matriz de p-valores
pval_spearman <- function(df) {
  vars <- names(df)
  n <- length(vars)
  p_mat <- matrix(NA, n, n)
  colnames(p_mat) <- vars
  rownames(p_mat) <- vars

  # Loop pelos pares de variáveis
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        p_mat[i, j] <- 0
      } else {
        teste <- cor.test(df[[i]], df[[j]], method = "spearman")
        p_mat[i, j] <- teste$p.value
      }
    }
  }
  return(p_mat)
}

# Cria as matrizes
correlations <- cor(dados_filtrados_numerico, method = "spearman")
p_values <- pval_spearman(dados_filtrados_numerico)


# salvar tabela de correlações (apenas a parte inferior da matriz)
correlations_lower <- correlations
correlations_lower[upper.tri(correlations_lower)] <- ""
write_csv(
  as.data.frame(correlations_lower) %>%
    rownames_to_column(var = "indice_vs_indice"),
  file.path(pasta_tabelas, "correlacoes_spearman_indices_imaturos_piata.csv")
)


# salvar tabela de p-values (apenas a parte inferior da matriz)
p_values_lower <- p_values
p_values_lower[upper.tri(p_values_lower)] <- ""
write_csv(
  as.data.frame(p_values_lower) %>%
    rownames_to_column(var = "indice_vs_indice"),
  file.path(pasta_tabelas, "p_values_spearman_indices_imaturos_piata.csv")
)
################################################################################
