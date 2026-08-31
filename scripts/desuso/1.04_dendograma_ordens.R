library(vegan)
library(tidyverse)
library(dendextend)

# Pasta de figuras
pasta_figuras <- "figuras/dendogramas/"
if (!dir.exists(pasta_figuras)) {
  dir.create(pasta_figuras, recursive = TRUE)
}


################################### Dados ######################################
df <- read.csv("dados/processado/matriz_comunidades_ordens_imaturos_largo.csv")

############################## Preprocessamento ################################


# Nome dos táxons
colnames(df[5:length(df)])
taxa <- colnames(df)[-(1:4)]

# Ajustes do dado
df_limpo <- df %>%
  mutate(
    campanha = case_when(
      campanha == "campanha_1" ~ "C1",
      campanha == "campanha_2" ~ "C2",
      campanha == "campanha_3" ~ "C3",
      campanha == "campanha_4" ~ "C4"
    ),
    rio = case_when(
      rio == "rio_bebedouro" ~ "Bebedouro",
      rio == "rio_da_barra" ~ "Barra",
      rio == "rio_mutuca" ~ "Mutuca",
      rio == "rio_da_serra" ~ "Serra",
      rio == "rio_carrapicho" ~ "Carrapicho"
    ),
    locality_name = case_when(
      locality_name == "RioBebedouro-P1" ~ "P1",
      locality_name == "RioBebedouro-P2" ~ "P2",
      locality_name == "RioBebedouro-P3" ~ "P3",
      locality_name == "RiodaBarra-P1/2" ~ "P1",
      locality_name == "RiodaBarra-P1" ~ "P4",
      locality_name == "RiodaBarra-P2" ~ "P2",
      locality_name == "RiodaBarra-P3" ~ "P3",
      locality_name == "RioMutuca-P1" ~ "P1",
      locality_name == "RioMutuca-P2" ~ "P2",
      locality_name == "RioMutuca-P3" ~ "P3",
      locality_name == "RiodaSerra-P1" ~ "P1",
      locality_name == "RiodaSerra-P2" ~ "P2",
      locality_name == "RiodaSerra-P3" ~ "P3",
      locality_name == "RioCarrapicho-P1" ~ "P1",
      locality_name ==  "RioCarrapicho-P2" ~ "P2",
      locality_name ==  "RioCarrapicho-P3" ~ "P3"
    ),
    method = case_when(
      method == "D Net" ~ " ■",
      method == "Pool Net" ~ " ▲",
      method == "Sieve" ~ " ●"
    ),
    etiqueta = paste(rio, locality_name, campanha, method,  sep = "")
  ) %>%
  ungroup()

rios <- df_limpo$rio
names(rios) <- df_limpo$etiqueta
############################## Matrizes de distância ###########################
df_numeric <- df[5:length(df)]

# distância e clustering de estrutura
dist_bray <- vegdist(df_numeric, method = "bray")
clust_bray <- hclust(dist_bray, method = "average")

# distância e clustering de composição
dist_jaccard <- vegdist(df_numeric, method = "jaccard")
clust_jaccard <- hclust(dist_jaccard, method = "average")


################################## Plot ########################################


#--------------------------------- Jaccard -------------------------------------
dendograma_jaccard <- as.dendrogram(clust_jaccard)
labels(dendograma_jaccard) <- df_limpo$etiqueta
ordem_labels <- labels(dendograma_jaccard)
rios_ord <- rios[ordem_labels]

# paleta de cores para rios
cores_rios <- c(
  "Bebedouro" = "#1b9e77",
  "Barra" = "#d95f02",
  "Mutuca" = "#7570b3",
  "Serra" = "#e7298a",
  "Carrapicho" = "#66a61e"
)

# aplicar cores nas labels
dendograma_jaccard <- set(dendograma_jaccard, "labels_col", cores_rios[rios_ord])
dendograma_jaccard <- set(dendograma_jaccard, "labels_cex", 0.7)

png(
  paste0(pasta_figuras, "dendrograma_composicao_ordens.png"),
  width = 1800, height = 1600
)
plot(
  dendograma_jaccard,
  ylab = "Distância Entre Ordens Jaccard (Composição)",
  horiz = TRUE
)
legend(
  "topright",
  legend = c("D Net", "Pool Net", "Sieve"),
  pch = c(15, 17, 16),  # quadrado, triângulo, círculo
  bty = "n",
  pt.cex = 1.5
)
dev.off()

# ----------------------------------- Estrutura --------------------------------
dendograma_bray <- as.dendrogram(clust_bray)
labels(dendograma_bray) <- df_limpo$etiqueta
ordem_labels_bray <- labels(dendograma_bray)
rios_ord_bray <- rios[ordem_labels_bray]
# aplicar cores nas labels
dendograma_bray <- set(dendograma_bray, "labels_col", cores_rios[rios_ord_bray])
dendograma_bray <- set(dendograma_bray, "labels_cex", 0.7)
png(
  paste0(pasta_figuras, "dendrograma_estrutura_ordens.png"),
  width = 1800, height = 1600
)
plot(
  dendograma_bray,
  ylab = "Distância Entre Ordens Bray-Curtis (Estrutura)",
  horiz = TRUE
)
legend(
  "topright",
  legend = c("D Net", "Pool Net", "Sieve"),
  pch = c(15, 17, 16),  # quadrado, triângulo, círculo
  bty = "n",
  pt.cex = 1.5
)
dev.off()
































# criar um vetor com 

# distância e clustering





plot(
  clust,
  labels = df_limpo$etiqueta,
  xlab = "Amostras",
  ylab = "Distância",
  sub = ""
) 

