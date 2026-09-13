library(tidyverse)
library(ggpubr)
library(corrplot)

################################## Configurações ###############################
set.seed(1994)
pontos_referencia <- c("RiodaBarra-P1/2", "RioMutuca-P1")
pontos_impactado <- c("RiodaBarra-P2", "RioMutuca-P3")
rios <- c("rio_bebedouro", "rio_da_barra", "rio_mutuca")

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
  "herbivoros_proporcao" = "(%) Herbívoros",
  "total_macroinvertebrados" = "Abundância Total",
  "ibvol" = "IBVOL"
)

vetor_estetica_impacto <- c(
  "referencia" = "Referência",
  "impactado" = "Impactado"
)


pasta_figuras <- "figuras/cap2/indice_multimetrico/sensibilidade/"
pasta_tabelas <- "tabelas/cap2/indice_multimetrico/sensibilidade/"

if (!dir.exists(pasta_figuras)) {
  dir.create(pasta_figuras, recursive = TRUE)
}

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
dados_filtrados_wilcox <- indices_df %>%
  filter(impacto != "desconsiderado") %>%
  filter(method %in% metodos_de_amostragem) %>%
  mutate(impacto = as.factor(impacto), method = as.factor(method))

str(dados_filtrados_wilcox)

############################# Testes de Mann-Whitney ###########################
plot_list <- list()
for (i in vetor_respostas) {
  print(paste("Analisando índice:", i))
  
  test <- dados_filtrados_wilcox %>%
    select(c("impacto", i))


  # Teste de Mann-Whitney
  teste <- wilcox.test(
    formula = as.formula(paste(i, "~ impacto")),
    data = dados_filtrados_wilcox
  )
 
  p_value = teste$p.value
  w <- teste$statistic

  # Tamanhos das amostras
  n1 <- dados_filtrados_wilcox %>%
    filter(impacto == levels(dados_filtrados_wilcox$impacto)[1]) %>% 
    nrow()
  n2 <- dados_filtrados_wilcox %>% 
    filter(impacto == levels(dados_filtrados_wilcox$impacto)[2]) %>% 
    nrow()

  # Cálculo do U
  u <- w - n1 * (n1 + 1) / 2
  
  
  # Criação do gráfico
  nome_indice <- vetor_estetica_indice[[i]]

  plot <- ggplot(
    dados_filtrados_wilcox,
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


}

#################### Correlações de Spearman (Total) ###########################
variaveis_selecionadas <- c(
  "aspt", "bmwp", "ept_riqueza", "filtrador_proporcao",
  "raspadores_proporcao", "razao_ept_chironomidae", 
  "relativo_odonata", "relativo_trichoptera", "riqueza_familias_ephemeroptera",
  "riqueza_familias_trichoptera", "shannon_familias"
)


# Tratar dados numéricos, removendo valores infinitos, NA e dados com baixa ----
# Variabilidade ----------------------------------------------------------------
analise_redundancia_df <- indices_df %>%
  filter(method %in% metodos_de_amostragem) %>%
  filter(rio %in% rios) %>%
  select(all_of(variaveis_selecionadas))


# colocar NA para valores infinitos
analise_redundancia_df_finitos <- analise_redundancia_df %>%
  mutate(across(everything(), ~ ifelse(is.finite(.x), .x, NA)))

# Filtrar variáveis com baixa variação
colunas_sd_zero <- sapply(analise_redundancia_df_finitos, function(x) {
  sd(x, na.rm = TRUE) == 0
})

nomes_colunas_problema <- names(colunas_sd_zero[colunas_sd_zero])
print(nomes_colunas_problema)

analise_redundancia_df_limpo <- 
  analise_redundancia_df_finitos[, !colunas_sd_zero]


# Função para calcular p-valores. ----------------------------------------------
pval_spearman_clean <- function(df) {
  vars <- names(df)
  n <- length(vars)
  p_mat <- matrix(NA, n, n)
  colnames(p_mat) <- vars
  rownames(p_mat) <- vars

  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        p_mat[i, j] <- 0
      } else {
        # Apenas observações com valores finitos em ambas as variáveis
        valid <- is.finite(df[[i]]) & is.finite(df[[j]])
        if (sum(valid) < 3) {
          # Com menos de 3 pares não é possível calcular Spearman
          p_mat[i, j] <- NA
        } else {
          teste <- cor.test(df[[i]][valid], df[[j]][valid], method = "spearman")
          p_mat[i, j] <- teste$p.value
        }
      }
    }
  }
  return(p_mat)
}
# Executar spearman e p-valores ------------------------------------------------

correlations <- cor(
  analise_redundancia_df_limpo, 
  method = "spearman", 
  use = "pairwise.complete.obs"
)
correlations_squared <- correlations^2
p_values <- pval_spearman_clean(analise_redundancia_df_limpo)


# Visualizar a matriz de correlação
png(file.path(pasta_figuras, "correlacao_indices_imaturos_piata.png"), 
    width = 800, height = 800)  # Aumentei a resolução geral

corrplot(
  correlations_squared, 
  method = "color", 
  type = "lower", 
  tl.col = "black", 
  p.mat = p_values,
  sig.level = 0.05,  # Adiciona um nível de significância
  insig = "blank",  # Mostra apenas os valores significativos         
  tl.cex = 0.9,        # Reduz tamanho dos labels
  tl.srt = 45,         # Rotação de 45 graus
  tl.offset = 0.5,     # Ajusta distância dos labels
  addCoef.col = "black", 
  number.cex = 0.8,    # Diminui tamanho dos coeficientes dentro das células
  cl.cex = 0.6,        # Tamanho do texto da legenda
  diag = FALSE)
dev.off()

# salvar tabelas de correlação e p-valores
correlations_lower <- correlations_squared
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

# Plotar as regressoes entre as variaveis --------------------------------------
pares_significativos <- 
  which(p_values < 0.05 & lower.tri(p_values), arr.ind = TRUE)



# Criar gráficos de dispersão para pares significativos
if(nrow(pares_significativos) > 0) {
  # Criar um arquivo PDF com todos os gráficos
  pdf(file.path(pasta_figuras, "regressoes_pares_significativos_piata.pdf"),
      width = 10, height = 8)
  
  for(i in 1:nrow(pares_significativos)) {
    var1 <- colnames(analise_redundancia_df_limpo)[pares_significativos[i,1]]
    var2 <- colnames(analise_redundancia_df_limpo)[pares_significativos[i,2]]
    
    # Extrair valores
    x <- analise_redundancia_df_limpo[[var1]]
    y <- analise_redundancia_df_limpo[[var2]]
    
    # Remover NAs
    validos <- complete.cases(x, y)
    x_clean <- x[validos]
    y_clean <- y[validos]
    
    # Calcular estatísticas
    r <- correlations[var1, var2]
    r2 <- correlations_squared[var1, var2]
    p_val <- p_values[var1, var2]
    
    # Ajustar modelo de regressão linear
    modelo_linear <- lm(y_clean ~ x_clean)
    
    # Extrair coeficientes da regressão
    intercepto <- coef(modelo_linear)[1]
    inclinacao <- coef(modelo_linear)[2]
    
    # Criar o gráfico
    par(mar = c(5, 5, 4, 2))
    plot(x_clean, y_clean,
         xlab = var1,
         ylab = var2,
         main = paste("Regressão Linear\n", var1, "vs", var2),
         pch = 19,
         col = rgb(0, 0, 0, 0.5),
         cex.main = 0.9)
    
    # Adicionar linha de regressão linear
    abline(modelo_linear, col = "red", lwd = 2)
    
    # Adicionar intervalo de confiança da regressão (opcional)
    # Ordenar os pontos para o intervalo de confiança
    x_ordenado <- seq(min(x_clean), max(x_clean), length.out = 100)
    intervalo_conf <- predict(modelo_linear, 
                             newdata = data.frame(x_clean = x_ordenado), 
                             interval = "confidence")
    
    # Adicionar banda de confiança
    polygon(c(x_ordenado, rev(x_ordenado)),
            c(intervalo_conf[,2], rev(intervalo_conf[,3])),
            col = rgb(1, 0, 0, 0.1), border = NA)
    
    # Adicionar equação da reta e estatísticas
    equacao <- paste0("y = ", round(intercepto, 3), 
                     ifelse(inclinacao >= 0, " + ", " - "), 
                     round(abs(inclinacao), 3), "x")
    
    legend("topright",
           legend = c(
             equacao,
             paste("r =", round(r, 3)),
             paste("r² =", round(r2, 3)),
             paste("p =", format(p_val, scientific = TRUE, digits = 3))
           ),
           bty = "n",
           cex = 0.8,
           text.col = c("red", "blue", "blue", "blue"))
    
    # Adicionar grid
    grid()
  }
  
  dev.off()
  
  cat("Gráficos de regressão linear salvos em:", 
      file.path(pasta_figuras, "regressoes_pares_significativos_piata.pdf"))
}



################################### Painel #####################################


painel_vetor <- c(
 "aspt", 
  "bmwp", 
  #"filtrador_proporcao",
  #"raspadores_proporcao",
  "razao_ept_chironomidae",
  "relativo_odonata",
  "relativo_trichoptera",
  "shannon_familias"
)

painel_plots <- list()
for (i in painel_vetor) {
  print(paste("Analisando índice:", i))
  
  test <- dados_filtrados_wilcox %>%
    select(c("impacto", i))

  # Teste de Mann-Whitney
  teste <- wilcox.test(
    formula = as.formula(paste(i, "~ impacto")),
    data = dados_filtrados_wilcox
  )
 
  p_value = teste$p.value
  w <- teste$statistic

  # Tamanhos das amostras
  n1 <- dados_filtrados_wilcox %>%
    filter(impacto == levels(dados_filtrados_wilcox$impacto)[1]) %>% 
    nrow()
  n2 <- dados_filtrados_wilcox %>% 
    filter(impacto == levels(dados_filtrados_wilcox$impacto)[2]) %>% 
    nrow()

  # Cálculo do U
  u <- w - n1 * (n1 + 1) / 2
  
  
  # Criação do gráfico
  nome_indice <- vetor_estetica_indice[[i]]

  plot <- ggplot(
    dados_filtrados_wilcox,
    aes_string(
      x = "impacto",
      y = i,
      fill = "impacto"
    )
  ) +
    theme_pubr() +
    geom_boxplot() +
    geom_jitter(width = 0.2, height = 0, alpha = 1) +
    scale_x_discrete(labels = vetor_estetica_impacto) +
    theme(
      text = element_text(size = unit(18, "pt"), face = "bold"),
      #axis.title = element_blank(),
      plot.title = element_blank(),
      axis.text.x = element_blank(),
      plot.subtitle = element_text(size = unit(18, "pt"), face = "bold", hjust = 0.5)
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
  painel_plots[[i]] <- plot

}


painel <- ggarrange(
  plotlist = painel_plots,
  ncol = 3,
  nrow = 2,
  labels = "AUTO",
  common.legend = TRUE,
  legend = "bottom"
)


formats <- c("png", "svg")

for (fmt in formats) {
  ggsave(
    filename = file.path(
      pasta_figuras,
      paste0("painel_indices_imaturos_piata.", fmt)
    ),
    plot = painel,
    width = 15,
    height = 18
  )
}
