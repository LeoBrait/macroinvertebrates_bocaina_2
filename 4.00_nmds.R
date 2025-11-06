library(tidyverse)
library(vegan)

################################### Carregar dados #############################
df_ordens <- read_csv(
  "dados/processado/matriz_comunidades_ordens_imaturos_largo.csv") %>%
  filter(method %in% c("D Net", "Pool Net"))

df_familias <- read_csv(
  "dados/processado/matriz_comunidades_familias_imaturos_largo.csv") %>%
  filter(method %in% c("D Net", "Pool Net"))

################################### Pre-processamento ##########################
matriz_relativa_ordens <- df_ordens %>%
    select(-(campanha:method)) %>%
    as.data.frame() %>%
    mutate(sample_id = row_number()) %>%
    column_to_rownames(var = "sample_id") %>%
    as.matrix() %>%
    decostand(method = "total", na.rm = TRUE)

matriz_relativa_familias <- df_familias %>%
    select(-(campanha:method)) %>%  
    as.data.frame() %>%
    mutate(sample_id = row_number()) %>%
    column_to_rownames(var = "sample_id") %>%
    as.matrix() %>%
    decostand(method = "total", na.rm = TRUE) %>%
    vegdist(method = "canberra")

matriz_presenca_ordens <- df_ordens %>%
    select(-(campanha:method)) %>%
    as.data.frame() %>%
    mutate(sample_id = row_number()) %>%
    column_to_rownames(var = "sample_id") %>%
    as.matrix() %>%
    decostand(method = "pa", na.rm = TRUE)

matriz_presenca_familias <- df_familias %>%
    select(-(campanha:method)) %>%
    as.data.frame() %>%
    mutate(sample_id = row_number()) %>%
    column_to_rownames(var = "sample_id") %>%
    as.matrix() %>%
    decostand(method = "pa", na.rm = TRUE)

matriz_bray_ordens <- 
    vegdist(matriz_relativa_ordens, method = "bray")
matriz_bray_familias <- 
    vegdist(matriz_relativa_familias, method = "bray")

matriz_canberra_ordens <- 
    vegdist(matriz_relativa_ordens, method = "canberra")
matriz_canberra_familias <- 
    vegdist(matriz_relativa_familias, method = "canberra")

matriz_jaccard_ordens <- 
    vegdist(matriz_presenca_ordens, method = "jaccard")
matriz_jaccard_familias <- 
    vegdist(matriz_presenca_familias, method = "jaccard")

#################################### NMDS ######################################


nmds_ordens_bray <- 
    metaMDS(
        matriz_bray_ordens, 
        k = 2, 
        try = 4999,
        trymax = 4999, 
        parallel = 4
    )

nmds_ordens_canberra <- 
    metaMDS(
        matriz_canberra_ordens, 
        k = 2, 
        try = 4999,
        trymax = 4999, 
        parallel = 4
    )

nmds_familias_bray <- 
    metaMDS(
        matriz_bray_familias, 
        k = 2, 
        try = 4999,
        trymax = 4999,
        parallel = 4
    )

nmds_familias_canberra <-
    metaMDS(
        matriz_canberra_familias, 
        k = 2, 
        try = 4999,
        trymax = 4999,
        parallel = 4
    )

nmds_familias_jaccard <- 
    metaMDS(
        matriz_jaccard_familias, 
        k = 2, 
        try = 4999,
        trymax = 4999,
        parallel = 4
    )

nmds_ordens_jaccard <-
    metaMDS(
        matriz_jaccard_ordens, 
        k = 2, 
        try = 4999,
        trymax = 4999,
        parallel = 4
    )
################################### Extract points ############################

# ordens
pontos_ordens_bray <- 
    as.data.frame(nmds_ordens_bray$points) %>%
    rename(MDS1 = MDS1, MDS2 = MDS2)
stress_ordens_bray <- nmds_ordens_bray$stress

pontos_ordens_canberra <- 
    as.data.frame(nmds_ordens_canberra$points) %>%
    rename(MDS1 = MDS1, MDS2 = MDS2)
stress_ordens_canberra <- nmds_ordens_canberra$stress

pontos_ordens_jaccard <- 
    as.data.frame(nmds_ordens_jaccard$points) %>%
    rename(MDS1 = MDS1, MDS2 = MDS2)
stress_ordens_jaccard <- nmds_ordens_jaccard$stress

# famílias
pontos_familias_bray <- 
    as.data.frame(nmds_familias_bray$points) %>%
    rename(MDS1 = MDS1, MDS2 = MDS2)
stress_familias_bray <- nmds_familias_bray$stress

pontos_familias_canberra <- 
    as.data.frame(nmds_familias_canberra$points) %>%
    rename(MDS1 = MDS1, MDS2 = MDS2)
stress_familias_canberra <- nmds_familias_canberra$stress

pontos_familias_jaccard <- 
    as.data.frame(nmds_familias_jaccard$points) %>%
    rename(MDS1 = MDS1, MDS2 = MDS2)
strwess_familias_jaccard <- nmds_familias_jaccard$stress


#################################### Plots #####################################
plot_ordens_bray <- ggplot(
    pontos_ordens_bray, 
        aes(
            x = MDS1,  y = MDS2, 
            color = df_ordens$rio, 
            shape = df_ordens$method
        )    
    ) +
    geom_point(size = 3) +
    theme_minimal() +
    labs(
        title = "NMDS Bray-Curtis - Ordens ",
        subtitle = paste("Stress:", round(stress_ordens_bray, 3)),
        x = "MDS1",
        y = "MDS2",
        color = "Rio",
        shape = "Método"
    ) +
    theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "right"
    )

plot_ordens_canberra <- ggplot(
    pontos_ordens_canberra, 
        aes(
            x = MDS1,  y = MDS2, 
            color = df_ordens$rio, 
            shape = df_ordens$method
        )    
    ) +
    geom_point(size = 3) +
    theme_minimal() +
    labs(
        title = "NMDS Canberra - Ordens ",
        subtitle = paste("Stress:", round(stress_ordens_canberra, 3)),
        x = "MDS1",
        y = "MDS2",
        color = "Rio",
        shape = "Método"
    ) +
    theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "right"
    )

plot_familias_bray <- ggplot(
    pontos_familias_bray, 
        aes(
            x = MDS1,  y = MDS2, 
            color = df_familias$rio, 
            shape = df_familias$method
        )    
    ) + 
    geom_point(size = 3) +
    theme_minimal() +
    labs(
        title = "NMDS Bray-Curtis - Famílias ",
        subtitle = paste("Stress:", round(stress_familias_bray, 3)),
        x = "MDS1",
        y = "MDS2",
        color = "Rio",
        shape = "Método"
    ) +
    theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "right"
    )

plot_familias_canberra <- ggplot(
    pontos_familias_canberra,
        aes(
            x = MDS1,  y = MDS2, 
            color = df_familias$rio, 
            shape = df_familias$method
        )    
    ) +
    geom_point(size = 3) +
    theme_minimal() +
    labs(
        title = "NMDS Canberra - Famílias ",
        subtitle = paste("Stress:", round(stress_familias_canberra, 3)),
        x = "MDS1",
        y = "MDS2",
        color = "Rio",
        shape = "Método"
    ) +
    theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "right"
    )

plot_ordens_jaccard <- ggplot(
    pontos_ordens_jaccard,
        aes(
            x = MDS1,  y = MDS2, 
            color = df_ordens$rio, 
            shape = df_ordens$method
        )    
    ) +
    geom_point(size = 3) +
    theme_minimal() +
    labs(
        title = "NMDS Jaccard - Ordens ",
        subtitle = paste("Stress:", round(stress_ordens_jaccard, 3)),
        x = "MDS1",
        y = "MDS2",
        color = "Rio",
        shape = "Método"
    ) +
    theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "right"
    )

plot_familias_jaccard <- ggplot(
    pontos_familias_jaccard,
        aes(
            x = MDS1,  y = MDS2, 
            color = df_familias$rio, 
            shape = df_familias$method
        )    
    ) +
    geom_point(size = 3) +
    theme_minimal() +
    labs(
        title = "NMDS Jaccard - Famílias ",
        subtitle = paste("Stress:", round(stress_familias_jaccard, 3)),
        x = "MDS1",
        y = "MDS2",
        color = "Rio",
        shape = "Método"
    ) +
    theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "right"
    )
#################################### Salvar ####################################
# Salvar 
ggsave(
    plot_ordens_bray,
    filename = "figuras/nmds/nmds_ordens_bray.png",
    width = 8,
    height = 6
)
ggsave(
    plot_ordens_canberra,
    filename = "figuras/nmds/nmds_ordens_canberra.png",
    width = 8,
    height = 6
)
ggsave(
    plot_familias_bray,
    filename = "figuras/nmds/nmds_familias_bray.png",
    width = 8,
    height = 6
)
ggsave(
    plot_familias_canberra,
    filename = "figuras/nmds/nmds_familias_canberra.png",
    width = 8,
    height = 6
)

ggsave(
    plot_ordens_jaccard,
    filename = "figuras/nmds/nmds_ordens_jaccard.png",
    width = 8,
    height = 6
)
ggsave(
    plot_familias_jaccard,
    filename = "figuras/nmds/nmds_familias_jaccard.png",
    width = 8,
    height = 6
)
