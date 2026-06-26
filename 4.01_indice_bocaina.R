library(tidyverse)

################################## Configurações ###############################
set.seed(1994)
pontos_referencia <- c("RiodaBarra-P1/2", "RioMutuca-P1")
pontos_impactado <- c("RiodaBarra-P2", "RioMutuca-P3")
rios <- c("rio_bebedouro", "rio_da_barra", "rio_mutuca")
amostras_descartadas <- c("RiodaBarra-P1")
metodos_de_amostragem <- c("D Net")
vetor_respostas <- c(
  "aspt", 
  "bmwp", 
  #"filtrador_proporcao",
  #"raspadores_proporcao",
  "razao_ept_chironomidae",
  "relativo_odonata",
  "relativo_trichoptera",
  "shannon_familias")

nota_maxima <- length(vetor_respostas) * 5
nota_minima <- length(vetor_respostas)


# Pastas de figuras
pasta_figuras <- "figuras/indice_multimetrico/rede_unica"
if (!dir.exists(pasta_figuras)) {
  dir.create(pasta_figuras, recursive = TRUE)
}

# Pasta de tabelas
pasta_tabelas <- "tabelas/indice_multimetrico/rede_unica"
if (!dir.exists(pasta_tabelas)) {
  dir.create(pasta_tabelas, recursive = TRUE)
}

################################## Carregar dados ##############################
indices_df <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv") 


############################## Pre-processamento ###############################
# Adicionar coluna de impacto
indices_df <- indices_df %>%
  mutate(
    impacto = case_when(
      locality_name %in% pontos_referencia ~ "referencia",
      locality_name %in% pontos_impactado ~ "impactado",
      TRUE ~ "desconsiderado"
    )
  ) %>%
  filter(rio %in% rios) %>%
  filter(!locality_name %in% amostras_descartadas)

dados_filtrados <- indices_df %>%
  filter(impacto != "desconsiderado") %>%
  filter(method %in% metodos_de_amostragem) %>%
  mutate(
    impacto = as.factor(impacto),
    method = as.factor(method)
  ) 

############################### Detectando limiares ############################
estatisticas_df <- data.frame()


for (i in vetor_respostas) {
  print(paste("Analisando índice:", i))
  
  estatisticas <- dados_filtrados %>%
    filter(impacto == "referencia") %>%
    mutate(
      minimo = min(.data[[i]]),
      quartil_25 = quantile(.data[[i]], 0.25, na.rm = TRUE),
      quartil_50 = quantile(.data[[i]], 0.50, na.rm = TRUE),
      quartil_75 = quantile(.data[[i]], 0.75, na.rm = TRUE),
      maximo = max(.data[[i]]),
      metade = quartil_25 / 2
      ) %>%
    mutate(metrica = i) %>%
    select(
      metrica, quartil_25, quartil_50, quartil_75, maximo, minimo, metade
    ) %>%
    group_by(metrica) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)

  estatisticas_df <- rbind(estatisticas_df, estatisticas)
}

write_csv(
  estatisticas_df,
  file.path(pasta_tabelas, "limiares_indices_imaturos_piata.csv")
)
############################### Aplicando na bacia #############################

statisticas_df_limpa <- estatisticas_df %>%
  select(metrica, quartil_25, metade)

indices_rebalanceados_df <- indices_df %>%
  filter(method %in% metodos_de_amostragem) %>%
  select(campanha, rio, locality_name, vetor_respostas) %>%
  pivot_longer(
    cols = all_of(vetor_respostas),
    names_to = "metrica",
    values_to = "valor"
  ) %>%
  left_join(statisticas_df_limpa, by = "metrica") %>%
  mutate(
    nota = case_when(
      valor >= quartil_25 ~ 5,
      valor >= metade & valor < quartil_25 ~ 3,
      valor < metade ~ 1,
      TRUE ~ NA_real_
    )
  ) 

  



giovanas_index <- indices_rebalanceados_df %>%
  group_by(campanha, rio, locality_name) %>%
  summarise(giovanas_index = sum(nota)) %>%
  ungroup() %>%
  mutate(
    giovanas_index_relative = (giovanas_index - nota_minima) 
                              / (nota_maxima - nota_minima) * 100
  )


tabela_IMM <- giovanas_index %>%
  select(campanha, rio, locality_name, giovanas_index, giovanas_index_relative)

tabela_indices <- indices_df %>%
  select(campanha, rio, locality_name, all_of(vetor_respostas)) %>%
  left_join(tabela_IMM, by = c("campanha", "rio", "locality_name"))

write_csv(
  tabela_indices,
  file.path(pasta_tabelas, "indice_bocaina_imaturos_piata.csv")
)


################## Plot indice multimetrico ####################################
metricas_contribuicao <- indices_rebalanceados_df %>%
  mutate(
    metrica_label = case_when(
      metrica == "aspt" ~ "ASPT",
      metrica == "bmwp" ~ "BMWP",
      metrica == "razao_ept_chironomidae" ~ "EPT/Chironomidae",
      metrica == "relativo_odonata" ~ "% Odonata",
      metrica == "relativo_trichoptera" ~ "% Trichoptera",
      metrica == "riqueza_familias_ephemeroptera" ~ "Presença Ephemeroptera",
      metrica == "shannon_familias" ~ "Shannon Famílias",
      TRUE ~ metrica
    )
  )

plot <- ggplot() +
  # Barras empilhadas com os valores brutos das notas
  geom_col(
    data = metricas_contribuicao,
    aes(x = campanha, y = nota, fill = metrica_label),
    position = "stack",
    alpha = 0.8,
    color = "black",
    size = 0.2
  ) +
  # Adicionar texto com os valores das notas dentro das barras
  geom_text(
    data = metricas_contribuicao,
    aes(x = campanha, y = nota, fill = metrica_label, label = nota),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "black",
    fontface = "bold"
  ) +
  # Linha do índice total
  geom_line(
    data = giovanas_index,
    aes(x = campanha, y = giovanas_index_relative / 100 * nota_maxima, group = 1),
    color = "black",
    size = 1.2
  ) +
  # Pontos do índice total
  geom_point(
    data = giovanas_index,
    aes(x = campanha, y = giovanas_index_relative / 100 * nota_maxima),
    color = "black",
    size = 3,
    shape = 21,
    fill = "white",
    stroke = 1.5
  ) +
  facet_wrap(~ interaction(rio, locality_name), scales = "free_x", ncol = 3) +
  labs(
    x = "Campanha",
    y = "Nota (Soma das Métricas)",
    fill = "Métricas",
    title = "Contribuição das Métricas para o Índice Bocaina"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
    limits = c(0, nota_maxima + 5),
    breaks = seq(0, nota_maxima, by = 5),
    expand = c(0, 0)
  )

ggsave(
  filename = file.path(pasta_figuras, "indice_bocaina_imaturos_piata.png"),
  plot = plot,
  width = 15,
  height = 15,
  dpi = 300
)


############################# plotar correlação entre métricas e IMM ###########
# Identificar colunas numéricas (excluindo giovanas_index_relative e giovanas_index)
colunas_numericas <- names(tabela_indices)[sapply(tabela_indices, is.numeric)]
colunas_para_plotar <- setdiff(colunas_numericas, c("giovanas_index_relative", "giovanas_index"))

# Criar gráficos de dispersão para giovanas_index_relative vs outras variáveis numéricas
if(length(colunas_para_plotar) > 0) {
  # Criar um arquivo PDF com todos os gráficos
  pdf(file.path(pasta_figuras, "graficos_giovanas_index_relative.pdf"),
      width = 10, height = 8)
  
  for(var in colunas_para_plotar) {
    # Extrair valores
    x <- tabela_indices[["giovanas_index_relative"]]
    y <- tabela_indices[[var]]
    
    # Remover NAs
    validos <- complete.cases(x, y)
    x_clean <- x[validos]
    y_clean <- y[validos]
    
    # Pular se não houver dados suficientes
    if(length(x_clean) < 3) next
    
    # Calcular correlação
    r <- cor(x_clean, y_clean, method = "pearson")
    r2 <- r^2
    
    # Teste de correlação
    cor_test <- cor.test(x_clean, y_clean, method = "pearson")
    p_val <- cor_test$p.value
    
    # Ajustar modelo de regressão linear
    modelo_linear <- lm(y_clean ~ x_clean)
    
    # Extrair coeficientes da regressão
    intercepto <- coef(modelo_linear)[1]
    inclinacao <- coef(modelo_linear)[2]
    
    # Criar o gráfico
    par(mar = c(5, 5, 4, 2))
    plot(x_clean, y_clean,
         xlab = "Giovana's Index Relative",
         ylab = var,
         main = paste("Regressão Linear\nGiovana's Index Relative vs", var),
         pch = 19,
         col = rgb(0, 0, 0, 0.5),
         cex.main = 0.9)
    
    # Adicionar linha de regressão linear
    abline(modelo_linear, col = "red", lwd = 2)
    
    # Adicionar intervalo de confiança da regressão
    if(length(x_clean) > 10) {  # Só adiciona intervalo se houver dados suficientes
      x_ordenado <- seq(min(x_clean), max(x_clean), length.out = 100)
      intervalo_conf <- predict(modelo_linear, 
                               newdata = data.frame(x_clean = x_ordenado), 
                               interval = "confidence")
      
      # Adicionar banda de confiança
      polygon(c(x_ordenado, rev(x_ordenado)),
              c(intervalo_conf[,2], rev(intervalo_conf[,3])),
              col = rgb(1, 0, 0, 0.1), border = NA)
    }
    
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
  
  cat("Gráficos de regressão linear salvos em: graficos_giovanas_index_relative.pdf\n")
  
  # Também criar uma tabela resumo das correlações
  resultado_correlacoes <- data.frame(
    Variavel = colunas_para_plotar,
    Correlacao_r = NA,
    R_quadrado = NA,
    P_valor = NA,
    Significativo_005 = NA
  )
  
  for(i in 1:length(colunas_para_plotar)) {
    var <- colunas_para_plotar[i]
    validos <- complete.cases(tabela_indices[["giovanas_index_relative"]], 
                             tabela_indices[[var]])
    
    if(sum(validos) >= 3) {
      cor_test <- cor.test(tabela_indices[["giovanas_index_relative"]][validos], 
                          tabela_indices[[var]][validos])
      resultado_correlacoes$Correlacao_r[i] <- round(cor_test$estimate, 3)
      resultado_correlacoes$R_quadrado[i] <- round(cor_test$estimate^2, 3)
      resultado_correlacoes$P_valor[i] <- format(cor_test$p.value, scientific = TRUE, digits = 3)
      resultado_correlacoes$Significativo_005[i] <- cor_test$p.value < 0.05
    }
  }
  
  # Mostrar e salvar resultados
  print(resultado_correlacoes)
  write.csv(resultado_correlacoes, file.path(pasta_tabelas, "correlacoes_giovanas_index_relative.csv"), row.names = FALSE)
  
}

############################## INACABADO INACABADO #############################
############################### Análise Leave-One-Out ##########################
############################## INACABADO INACABADO #############################

# Função para calcular o IMM com um conjunto específico de métricas
calcular_imm <- function(vetor_metricas, dados, estatisticas_df) {
  
  # Ajustar notas máximas e mínimas para o número de métricas
  nota_max_parcial <- length(vetor_metricas) * 5
  nota_min_parcial <- length(vetor_metricas)
  
  # Filtrar estatísticas para as métricas selecionadas
  estatisticas_parciais <- estatisticas_df %>%
    filter(metrica %in% vetor_metricas) %>%
    select(metrica, quartil_25, metade)
  
  # Calcular IMM parcial
  imm_parcial <- dados %>%
    select(campanha, rio, locality_name, all_of(vetor_metricas)) %>%
    pivot_longer(
      cols = all_of(vetor_metricas),
      names_to = "metrica",
      values_to = "valor"
    ) %>%
    left_join(estatisticas_parciais, by = "metrica") %>%
    mutate(
      nota = case_when(
        valor >= quartil_25 ~ 5,
        valor >= metade & valor < quartil_25 ~ 3,
        valor < metade ~ 1,
        TRUE ~ NA_real_
      )
    ) %>%
    group_by(campanha, rio, locality_name) %>%
    summarise(
      imm_score = sum(nota),
      n_metricas = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      imm_relative = (imm_score - nota_min_parcial) / 
                     (nota_max_parcial - nota_min_parcial) * 100
    )
  
  return(imm_parcial)
}

# Calcular IMM completo (referência)
imm_completo <- calcular_imm(vetor_respostas, indices_df, estatisticas_df)

# Inicializar dataframe para resultados
resultados_loo <- data.frame()

# Loop Leave-One-Out
for (metrica_removida in vetor_respostas) {
  
  cat("\n========================================\n")
  cat("Removendo métrica:", metrica_removida, "\n")
  
  # Criar vetor sem a métrica atual
  metricas_reduzidas <- vetor_respostas[vetor_respostas != metrica_removida]
  
  # Calcular IMM sem essa métrica
  imm_parcial <- calcular_imm(metricas_reduzidas, indices_df, estatisticas_df)
  
  # Comparar com o IMM completo
  comparacao <- imm_completo %>%
    select(campanha, rio, locality_name, imm_completo = imm_relative) %>%
    left_join(
      imm_parcial %>% 
        select(campanha, rio, locality_name, imm_parcial = imm_relative),
      by = c("campanha", "rio", "locality_name")
    ) %>%
    mutate(
      metrica_removida = metrica_removida,
      diferenca = imm_parcial - imm_completo,
      diferenca_absoluta = abs(diferenca)
    )
  
  resultados_loo <- bind_rows(resultados_loo, comparacao)
  
  # Resumo estatístico
  cat("Diferença média:", mean(comparacao$diferenca, na.rm = TRUE), "\n")
  cat("Diferença absoluta média:", mean(comparacao$diferenca_absoluta, na.rm = TRUE), "\n")
  cat("Correlação com IMM completo:", cor(comparacao$imm_completo, comparacao$imm_parcial, use = "complete.obs"), "\n")
}

# Salvar resultados completos
write_csv(
  resultados_loo,
  file.path(pasta_tabelas, "resultados_leave_one_out.csv")
)

############################### Análise de estabilidade ########################

# Resumo por métrica removida
resumo_loo <- resultados_loo %>%
  group_by(metrica_removida) %>%
  summarise(
    correlacao = cor(imm_completo, imm_parcial, use = "complete.obs"),
    dif_media = mean(diferenca, na.rm = TRUE),
    dif_abs_media = mean(diferenca_absoluta, na.rm = TRUE),
    dif_dp = sd(diferenca, na.rm = TRUE),
    dif_max = max(diferenca_absoluta, na.rm = TRUE),
    n_amostras = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(correlacao))

write_csv(
  resumo_loo,
  file.path(pasta_tabelas, "resumo_leave_one_out.csv")
)

############################### Visualizações ##################################

# 1. Boxplot das diferenças por métrica removida
p1 <- ggplot(resultados_loo, aes(x = reorder(metrica_removida, diferenca_absoluta), 
                                  y = diferenca)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Impacto da remoção de cada métrica no IMM",
    subtitle = "Análise Leave-One-Out",
    x = "Métrica removida",
    y = "Diferença no IMM relativo"
  ) +
  theme_minimal()

ggsave(
  file.path(pasta_figuras, "leave_one_out_boxplot.png"),
  p1,
  width = 12,
  height = 8
)

# 2. Correlação entre IMM completo e parcial para cada métrica
p2 <- ggplot(resumo_loo, aes(x = reorder(metrica_removida, correlacao), 
                              y = correlacao)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(correlacao, 3)), hjust = -0.1) +
  coord_flip() +
  labs(
    title = "Correlação entre IMM completo e IMM sem cada métrica",
    x = "Métrica removida",
    y = "Correlação de Pearson"
  ) +
  ylim(0.5, 1) +
  theme_minimal()

ggsave(
  file.path(pasta_figuras, "leave_one_out_correlacao.png"),
  p2,
  width = 12,
  height = 8
)

# 3. Scatterplot para a métrica mais influente
metrica_mais_influente <- resumo_loo$metrica_removida[which.min(resumo_loo$correlacao)]

p3 <- resultados_loo %>%
  filter(metrica_removida == metrica_mais_influente) %>%
  ggplot(aes(x = imm_completo, y = imm_parcial)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = paste("IMM completo vs IMM sem", metrica_mais_influente),
    x = "IMM completo (%)",
    y = paste("IMM sem", metrica_mais_influente, "(%)")
  ) +
  theme_minimal()

ggsave(
  file.path(pasta_figuras, "leave_one_out_scatter_mais_influente.png"),
  p3,
  width = 10,
  height = 8
)

# 4. Heatmap da diferença por ponto amostral
p4 <- resultados_loo %>%
  ggplot(aes(x = locality_name, y = metrica_removida, fill = diferenca)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 0, name = "Diferença") +
  labs(
    title = "Impacto da remoção de métricas por ponto amostral",
    x = "Ponto amostral",
    y = "Métrica removida"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path(pasta_figuras, "leave_one_out_heatmap.png"),
  p4,
  width = 14,
  height = 10
)

# 5. Gráfico de importância das métricas
p5 <- ggplot(resumo_loo, aes(x = reorder(metrica_removida, dif_abs_media), 
                              y = dif_abs_media)) +
  geom_col(fill = "darkred", alpha = 0.8) +
  geom_errorbar(aes(ymin = dif_abs_media - dif_dp, 
                    ymax = dif_abs_media + dif_dp),
                width = 0.2) +
  coord_flip() +
  labs(
    title = "Importância relativa das métricas no IMM",
    subtitle = "Baseado na diferença absoluta média ao remover cada métrica",
    x = "Métrica",
    y = "Diferença absoluta média (%)"
  ) +
  theme_minimal()

ggsave(
  file.path(pasta_figuras, "leave_one_out_importancia.png"),
  p5,
  width = 12,
  height = 8
)

############################### Análise de redundância #########################


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



# Matriz de correlação entre métricas
matriz_cor <- dados_filtrados %>%
  select(all_of(vetor_respostas)) %>%
  cor(method = "spearman")

matriz_pval <- pval_spearman(dados_filtrados %>% select(all_of(vetor_respostas)))

# Salvar matriz
write_csv(
  as.data.frame(matriz_cor) %>% rownames_to_column("metrica"),
  file.path(pasta_tabelas, "correlacao_metricas.csv")
)

# Visualizar matriz de correlação
install.packages("corrplot")  # Instalar o pacote corrplot se ainda não estiver instalado

png(file.path(pasta_figuras, "correlacao_metricas.png"), 
    width = 800, height = 800)
corrplot(matriz_cor, 
         method = "color", 
         type = "upper", 
         addCoef.col = "black",
         tl.col = "black", 
         tl.srt = 45,
         title = "Correlação entre métricas do IMM",
         mar = c(0,0,2,0))
dev.off()

# visualizar matriz de p-valores
png(file.path(pasta_figuras, "pval_metricas.png"), 
    width = 800, height = 800)
corrplot(matriz_pval, 
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         title = "P-valores da correlação entre métricas do IMM",
         mar = c(0,0,2,0))
dev.off()

############################### Resumo final ###################################
cat("\n\n========================================\n")
cat("RESUMO DA ANÁLISE LEAVE-ONE-OUT\n")
cat("========================================\n\n")

cat("Métricas mais influentes (maior diferença):\n")
resumo_loo %>%
  top_n(3, dif_abs_media) %>%
  select(metrica_removida, dif_abs_media, correlacao) %>%
  print()

cat("\nMétricas menos influentes (menor diferença):\n")
resumo_loo %>%
  top_n(3, desc(dif_abs_media)) %>%
  select(metrica_removida, dif_abs_media, correlacao) %>%
  print()

cat("\nCorrelação média entre IMM completo e parcial:", 
    mean(resumo_loo$correlacao), "\n")
cat("Desvio padrão das correlações:", 
    sd(resumo_loo$correlacao), "\n")


