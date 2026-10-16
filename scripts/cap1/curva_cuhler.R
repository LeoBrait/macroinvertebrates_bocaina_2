###############################################################################
# CURVAS DE ACUMULAÇÃO + MODELO DE CLENCH
# Família e Ordem lado a lado
###############################################################################
library(tidyverse)
library(vegan)
library(ggplot2)
library(patchwork)

###############################################################################
# CONFIGURAÇÕES
###############################################################################

rios <- c(
  "rio_bebedouro",
  "rio_da_barra",
  "rio_mutuca"
)

amostras_descartadas <- c(
  "RiodaBarra-P1"
)

metodos <- c(
  "D Net",
  "Pool Net",
  "Sieve"
)

###############################################################################
# IMPORTAÇÃO
###############################################################################

df_cru <- read_csv(
  "dados/processado/imaturos_piata_limpo.csv"
)



###############################################################################
# FILTRAGEM GERAL
###############################################################################

df_base <- df_cru %>%
  filter(rio %in% rios) %>%
  filter(!locality_name %in% amostras_descartadas) %>%
  filter(method %in% metodos) %>%
  group_by(
    campanha,
    rio,
    locality_name
  ) %>%
  filter(
    all(metodos %in% method)
  ) %>%
  ungroup() %>%
  mutate(
    amostra = paste(
      campanha,
      rio,
      locality_name,
      sep = "_"
    )
  )

###############################################################################
# CORES
###############################################################################

cores <- c(
  "D Net" = "#2ec24e",
  "Pool Net" = "#6ca2ff",
  "Sieve" = "#f8776e"
)

###############################################################################
# NOMES DOS MÉTODOS EM PORTUGUÊS
###############################################################################

nomes_metodos <- c(
  "D Net" = "Rede D",
  "Pool Net" = "Rede de piscina",
  "Sieve" = "Peneira"
)

###############################################################################
# FUNÇÃO PARA RODAR A ANÁLISE
###############################################################################

rodar_clench <- function(df_base, taxon, titulo_y){
  
  ###########################################################################
  # REMOVER REGISTROS SEM IDENTIFICAÇÃO
  ###########################################################################
  
  df <- df_base %>%
    filter(
      !is.na(.data[[taxon]]),
      .data[[taxon]] != ""
    )
  
  ###########################################################################
  # MATRIZES
  ###########################################################################
  
  matrizes <- list()
  
  for(m in metodos){
    
    mat <- df %>%
      filter(method == m) %>%
      group_by(
        amostra,
        !!sym(taxon)
      ) %>%
      summarise(
        Abundancia = n(),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = !!sym(taxon),
        values_from = Abundancia,
        values_fill = 0
      )
    
    rownames(mat) <- mat$amostra
    
    mat <- mat[, -1]
    
    matrizes[[m]] <- as.matrix(mat)
    
  }
  
  ###########################################################################
  # CURVA DE ACUMULAÇÃO
  ###########################################################################
  
  set.seed(123)
  
  acum <- lapply(
    matrizes,
    function(x){
      
      specaccum(
        x,
        method = "random",
        permutations = 999
      )
      
    }
  )
  
  ###########################################################################
  # ORGANIZAR DADOS
  ###########################################################################
  
  dados_acum <- bind_rows(
    
    lapply(
      metodos,
      function(m){
        
        data.frame(
          Metodo = m,
          Amostras = acum[[m]]$sites,
          Riqueza = acum[[m]]$richness,
          SD = acum[[m]]$sd
        )
        
      }
    )
    
  )
  
  ###########################################################################
  # AJUSTE DE CLENCH
  ###########################################################################
  
  modelos_clench <- list()
  
  resultados_clench <- data.frame()
  
  for(m in metodos){
    
    dados <- dados_acum %>%
      filter(Metodo == m)
    
    a_inicial <-
      max(dados$Riqueza) /
      max(dados$Amostras)
    
    b_inicial <- 0.01
    
    modelo <- nls(
      
      Riqueza ~
        (a * Amostras) /
        (1 + b * Amostras),
      
      data = dados,
      
      start = list(
        a = a_inicial,
        b = b_inicial
      ),
      
      algorithm = "port",
      
      lower = c(
        a = 0,
        b = 0
      )
      
    )
    
    modelos_clench[[m]] <- modelo
    
    coeficientes <- coef(modelo)
    
    a <- unname(
      coeficientes["a"]
    )
    
    b <- unname(
      coeficientes["b"]
    )
    
    Smax <- a / b
    
    Sobs <- max(
      dados$Riqueza
    )
    
    completude <-
      (Sobs / Smax) * 100
    
    n_final <- max(
      dados$Amostras
    )
    
    inclinacao_final <-
      a /
      ((1 + b * n_final)^2)
    
    pred <- predict(modelo)
    
    SSE <- sum(
      (dados$Riqueza - pred)^2
    )
    
    SST <- sum(
      (dados$Riqueza -
         mean(dados$Riqueza))^2
    )
    
    R2 <- 1 - SSE / SST
    
    resultados_clench <- bind_rows(
      
      resultados_clench,
      
      data.frame(
        Nivel = taxon,
        Metodo = m,
        Numero_amostras = n_final,
        Riqueza_observada = Sobs,
        a = a,
        b = b,
        Riqueza_assintotica = Smax,
        Completude_percentual = completude,
        Inclinacao_final = inclinacao_final,
        R2 = R2
      )
      
    )
    
  }
  
  ###########################################################################
  # PREDIÇÕES
  ###########################################################################
  
  predicoes <- list()
  
  for(m in metodos){
    
    n_max <- max(
      acum[[m]]$sites
    )
    
    novo_x <- seq(
      from = 0,
      to = n_max * 2,
      length.out = 300
    )
    
    predicoes[[m]] <- data.frame(
      Metodo = m,
      Amostras = novo_x,
      Riqueza = predict(
        modelos_clench[[m]],
        newdata = data.frame(
          Amostras = novo_x
        )
      )
    )
    
  }
  
  predicoes_df <- bind_rows(
    predicoes
  )
  
  ###########################################################################
  # GRÁFICO
  ###########################################################################
  
  grafico <- ggplot() +
    #Buffer das curvas - intervalo de confiança
    geom_ribbon(
      data = dados_acum,
      aes(
        x = Amostras,
        ymin = pmax(Riqueza - SD, 0),
        ymax = Riqueza + SD,
        fill = Metodo
      ),
      alpha = 0.18
    ) +
    
    #Cria as curvas em linha reta de cada método
    geom_line(
     data = dados_acum,
      aes(
        x = Amostras,
        y = Riqueza,
        color = Metodo
      ),
      linewidth = 1
    ) +
    #Coloca pontos em cada número de amostras
    geom_point(
      data = dados_acum,
      aes(
       x = Amostras,
        y = Riqueza,
        color = Metodo
      ),
      size = 1.8
    ) +
    
  #Define até onde vai o intervalo de confiança, o buffer
   geom_line(
      data = predicoes_df,
      aes(
       x = Amostras,
        y = Riqueza,
        color = Metodo
      ),
      linewidth = 1.1,
      linetype = "dashed"
    ) +
    
    #Mexe  na cor das legendas
    scale_color_manual(
      values = cores,
      labels = nomes_metodos
    ) +
    # Mexe em qual lado as legendas
    scale_fill_manual(
      values = cores,
      labels = nomes_metodos
    ) +
    #Mexe nas etiquetas/legendas
    labs(
     x = "Nº de amostras",
      y = titulo_y,
      color = "Método",
      fill = "Método"
    ) +
    #Mexe no tamanho da fonte
    theme_classic(
      base_size = 12
    ) +
    
    
    
    theme(
      
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.title = element_text(size = 12, family = "bold"),
      axis.text = element_text(size = 12)
    )
  
  ###########################################################################
  # RETORNAR
  ###########################################################################
  
  return(
    list(
      grafico = grafico,
      resultados = resultados_clench
    )
  )
  
}

###############################################################################
# RODAR PARA FAMÍLIA
###############################################################################


resultado_familia <- rodar_clench(
  df_base = df_base,
  taxon = "family",
  titulo_y = "Riqueza acumulada de família"
)

###############################################################################
# RODAR PARA ORDEM
###############################################################################

resultado_ordem <- rodar_clench(
  df_base = df_base,
  taxon = "order",
  titulo_y = "Riqueza acumulada de ordem"
)

###############################################################################
# EXTRAIR OS GRÁFICOS
###############################################################################
resultado_familia[1]
resultado_familia[2]


grafico_familia <- resultado_familia$grafico

grafico_ordem <- resultado_ordem$grafico

###############################################################################
# JUNTAR LADO A LADO
###############################################################################

grafico_final <-
  
  grafico_ordem +
  
  grafico_familia +
  
  plot_layout(
    ncol = 2,
    guides = "collect"
  ) &
  
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    
  )
  
###############################################################################
# AJUSTAR LIMITES DOS EIXOS Y
###############################################################################

# Ordens: eixo Y de 0 a 20
grafico_ordem <- resultado_ordem$grafico +
  scale_y_continuous(
    limits = c(0, 20),
    breaks = seq(0, 20, by = 5),
    expand = expansion(mult = c(0, 0.02))
  )

# Famílias: eixo Y de 0 a 50
grafico_familia <- resultado_familia$grafico +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, by = 10),
    expand = expansion(mult = c(0, 0.02))
  )

###############################################################################
# MOSTRAR FIGURA
###############################################################################

grafico_final

###############################################################################
# CRIAR PASTAS
###############################################################################

dir.create(
  "figuras",
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  "resultados",
  showWarnings = FALSE,
  recursive = TRUE
)

###############################################################################
# SALVAR FIGURA
###############################################################################

ggsave(
  filename =
    "figuras/Curvas_Clench_ordem_familia_lado_a_lado.png",
  plot =
    grafico_final,
  width =
    30,
  height =
    13,
  units =
    "cm",
  dpi =
    300
)

###############################################################################
# JUNTAR RESULTADOS NUMÉRICOS
###############################################################################

resultados_finais <- bind_rows(
  resultado_ordem$resultados,
  resultado_familia$resultados
)

###############################################################################
# TRADUZIR NOMES DOS MÉTODOS
###############################################################################

resultados_finais <- resultados_finais %>%
  
  mutate(
    
    Nivel = recode(
      Nivel,
      "order" = "Ordem",
      "family" = "Família"
    ),
    
    Metodo = recode(
      Metodo,
      "D Net" = "Rede D",
      "Pool Net" = "Rede de piscina",
      "Sieve" = "Peneira"
    )
    
  ) %>%
  
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  )

###############################################################################
# VISUALIZAR TABELA FINAL
###############################################################################

print(
  resultados_finais
)

View(
  resultados_finais
)

###############################################################################
# SALVAR TABELA
###############################################################################

write_csv(
  resultados_finais,
  "resultados/resultados_Clench_ordem_familia.csv"
)

###############################################################################
# FIM
###############################################################################
###############################################################################
# EXTRAIR E AJUSTAR OS GRÁFICOS
###############################################################################

# ORDENS
# Eixo Y termina em 20
# Eixo X termina em 50

grafico_ordem <- resultado_ordem$grafico +
  
  scale_y_continuous(
    limits = c(0, 20),
    breaks = seq(0, 20, by = 5),
    expand = expansion(mult = c(0, 0))
  ) +
  
  scale_x_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, by = 10),
    expand = expansion(mult = c(0, 0))
  )


# FAMÍLIAS
# Eixo Y termina em 50
# Eixo X termina em 50

grafico_familia <- resultado_familia$grafico +
  
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, by = 10),
    expand = expansion(mult = c(0, 0))
  ) +
  
  scale_x_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, by = 10),
    expand = expansion(mult = c(0, 0))
  )


###############################################################################
# JUNTAR OS GRÁFICOS LADO A LADO
###############################################################################

grafico_final <-
  
  grafico_ordem +
  
  grafico_familia +
  
  plot_layout(
    ncol = 2,
    guides = "collect"
  ) &
  
  theme(
    
    # Legenda
    legend.position = "bottom",
    legend.title = element_blank(),
    
    # Títulos dos eixos em negrito
    axis.title.x = element_text(
      face = "bold",
      size = 13
    ),
    
    axis.title.y = element_text(
      face = "bold",
      size = 13
    )
    
  )


###############################################################################
# MOSTRAR FIGURA
###############################################################################

grafico_final
