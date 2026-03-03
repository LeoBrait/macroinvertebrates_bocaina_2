library(vegan)
library(tidyverse)
library(dendextend)

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

df_numeric <- df[5:length(df)]

dist_bc <- vegdist(df_numeric, method = "jaccard")
clust <- hclust(dist_bc, method = "average")



# converter para dendrograma
dend <- as.dendrogram(clust)

# garantir que os labels do dendrograma sejam suas etiquetas
labels(dend) <- df_limpo$etiqueta

# vetor com rio de cada amostra
rios <- df_limpo$rio
names(rios) <- df_limpo$etiqueta

# ordem real das folhas no dendrograma
ordem_labels <- labels(dend)

# rios na mesma ordem do dendrograma
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
dend <- set(dend, "labels_col", cores_rios[rios_ord])
dend <- set(dend, "labels_cex", 0.7)

# plot

png("dendrograma_composicao_ordens.png", width = 1800, height = 1600)
plot(dend, ylab = "Distância Jaccard (Composição)", horiz = TRUE) 
dev.off()






legend(
  "topright",
  legend = names(cores_rios),
  fill = cores_rios,
  border = NA,
  cex = 0.8
)





































# criar um vetor com 

# distância e clustering





plot(
  clust,
  labels = df_limpo$etiqueta,
  xlab = "Amostras",
  ylab = "Distância",
  sub = ""
) 

