###############################################################################
# CURVA DE ACUMULAÇÃO DE ESPÉCIES
# Comparação entre métodos de coleta
# Delineamento: 3 campanhas × 3 rios × 3 métodos
###############################################################################

library(tidyverse)
library(vegan)
library(ggplot2)
###############################################################################
# CONFIGURAÇÕES
###############################################################################

rios <- c("rio_bebedouro",
          "rio_da_barra",
          "rio_mutuca")

amostras_descartadas <- c("RiodaBarra-P1")

metodos <- c("D Net",
             "Pool Net",
             "Sieve")

taxon <- "family"      # usar "order" para ordens

###############################################################################
# IMPORTAÇÃO
###############################################################################

df_cru <- read_csv("dados/processado/imaturos_piata_limpo.csv")

###############################################################################
# FILTRAGEM
###############################################################################

df <- df_cru %>%
  filter(rio %in% rios) %>%
  filter(!locality_name %in% amostras_descartadas) %>%
  filter(method %in% metodos) %>%
  group_by(campanha,
           rio,
           locality_name) %>%
  filter(all(metodos %in% method)) %>%
  ungroup()

###############################################################################
# CRIA UMA UNIDADE AMOSTRAL
###############################################################################

df <- df %>%
  mutate(amostra = paste(campanha,
                         rio,
                         locality_name,
                         sep = "_"))

###############################################################################
# MATRIZES PARA CADA MÉTODO
###############################################################################

matrizes <- list()

for(m in metodos){

  mat <- df %>%
    filter(method == m) %>%
    group_by(amostra,
             !!sym(taxon)) %>%
    summarise(Abundancia = n(),
              .groups="drop") %>%
    pivot_wider(names_from = !!sym(taxon),
                values_from = Abundancia,
                values_fill = 0)

  rownames(mat) <- mat$amostra
  mat <- mat[,-1]

  matrizes[[m]] <- as.matrix(mat)

}

###############################################################################
# CURVA DE RAREFAÇÃO
###############################################################################

library(vegan)

# Somar todas as amostras de cada método
matriz_total <- lapply(matrizes, colSums)

# Número máximo de indivíduos
n.max <- min(sapply(matriz_total, sum))

# Sequência de indivíduos
amostras <- seq(10, n.max, by = 10)

# Calcular rarefação
rare <- lapply(matriz_total, function(x){

  data.frame(
    Individuos = amostras,
    Riqueza = sapply(amostras,
                     function(n)
                       rarefy(x, sample = n))
  )

})

###############################################################################
# FIGURA
###############################################################################

cores <- c("#1b9e77",
           "#d95f02",
           "#7570b3")

png("figuras/Curva_rarefacao_metodos.png",
    width = 2200,
    height = 1800,
    res = 300)

plot(NULL,
     xlim = c(0, n.max),
     ylim = c(0, max(sapply(rare, function(x) max(x$Riqueza))) + 2),
     xlab = "Número de indivíduos",
     ylab = "Riqueza esperada")

for(i in 1:length(metodos)){

  lines(rare[[i]]$Individuos,
        rare[[i]]$Riqueza,
        col = cores[i],
        lwd = 3)

}

legend("bottomright",
       legend = metodos,
       col = cores,
       lwd = 3,
       bty = "n")

dev.off()



###############################################################################
# CURVA DE RAREFAÇÃO COM INTERVALO DE CONFIANÇA
###############################################################################

library(vegan)

# Somar todas as amostras de cada método
matriz_total <- lapply(matrizes, colSums)

# Número máximo de indivíduos
n.max <- min(sapply(matriz_total, sum))

# Sequência de indivíduos
amostras <- seq(10, n.max, by = 10)

# Rarefação + erro padrão
rare <- lapply(matriz_total, function(x){

  res <- sapply(amostras, function(n){

    r <- rarefy(x, sample = n, se = TRUE)

    c(
      riqueza = r[1],
      se = r[2]
    )

  })

  res <- as.data.frame(t(res))

  res$Individuos <- amostras
  res$IC.inf <- res$riqueza - 1.96 * res$se
  res$IC.sup <- res$riqueza + 1.96 * res$se

  res

})



###############################################################################
# FIGURA
###############################################################################

cores <- c("#1b9e77",
           "#d95f02",
           "#7570b3")

png("figuras/Curva_rarefacao_metodos_IC.png",
    width = 2200,
    height = 1800,
    res = 300)

ylim.max <- max(sapply(rare, function(x) max(x$IC.sup)))

plot(NULL,
     xlim = c(0, n.max),
     ylim = c(0, ylim.max + 2),
     xlab = "Número de indivíduos",
     ylab = "Riqueza esperada")

for(i in seq_along(metodos)){

  # faixa do intervalo de confiança
  polygon(
    c(rare[[i]]$Individuos,
      rev(rare[[i]]$Individuos)),
    c(rare[[i]]$IC.inf,
      rev(rare[[i]]$IC.sup)),
    col = adjustcolor(cores[i], alpha.f = 0.25),
    border = NA
  )

  # curva
  lines(rare[[i]]$Individuos,
        rare[[i]]$riqueza,
        col = cores[i],
        lwd = 3)

}

legend("bottomright",
       legend = metodos,
       col = cores,
       lwd = 3,
       bty = "n")

dev.off()

##########Curva de acumulação de espécies########################################
matrizes[[m]] <- as.matrix(mat)
###############################################################################
# CURVA DE ACUMULAÇÃO
###############################################################################

library(vegan)

acum <- lapply(matrizes, function(x){

  specaccum(x,
            method = "random",
            permutations = 1000)

})
matrizes[[m]] <- as.matrix(mat)
permutations = 1000
cores <- c("#1b9e77",
           "#d95f02",
           "#7570b3")

png("figuras/Curva_acumulacao_metodos.png",
    width=2200,
    height=1800,
    res=300)

ylim.max <- max(sapply(acum, function(x) max(x$richness + x$sd)))

plot(NULL,
     xlim=c(1,max(sapply(acum,function(x) max(x$sites)))),
     ylim=c(0,ylim.max+2),
     xlab="Número de amostras",
     ylab="Riqueza acumulada")

for(i in seq_along(metodos)){

    polygon(
      c(acum[[i]]$sites,
        rev(acum[[i]]$sites)),
      c(acum[[i]]$richness-acum[[i]]$sd,
        rev(acum[[i]]$richness+acum[[i]]$sd)),
      col=adjustcolor(cores[i],0.25),
      border=NA
    )

    lines(acum[[i]]$sites,
          acum[[i]]$richness,
          lwd=3,
          col=cores[i])

}

legend("bottomright",
       legend=metodos,
       col=cores,
       lwd=3,
       bty="n")

dev.off()
