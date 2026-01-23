# Próximos passos
## 1. Fazer gráfico de abundância relativa para família
## 3. Renomear as campanhas e o nome dos pontos 
##### Ex.Rio Bebedouro - RBE - Campanha 1 - C1 


library(tidyverse)
install.packages("colorspace")
library(colorspace)


cores_50_cb <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#999999", "#117733",
  "#44AA99", "#88CCEE", "#DDCC77", "#AA4499", "#882255",
  "#332288", "#6699CC", "#661100", "#AA4466", "#4477AA",
  "#228833", "#66CCEE", "#CCBB44", "#EE6677", "#4477CC",
  "#AA3377", "#BBBBBB", "#004488", "#DDAA33", "#BB5566",
  "#0066AA", "#77AADD", "#EE8866", "#FFAABB", "#99DDFF",
  "#44BB99", "#BBCC33", "#AAAA00", "#DDDDDD", "#555555",
  "#774411", "#33AA55", "#AA7733", "#3377AA", "#77AA33",
  "#DD4477", "#44AADD", "#999933", "#CC6677", "#88AA99"
)


df <- read.csv("dados/processado/matriz_comunidades_familia_imaturo_largo.csv")

taxa <- colnames(df)[-(1:4)]

df_long <- df %>% 
  pivot_longer(
    cols = taxa,
    names_to = "familia",
    values_to = "Abundancia"
  ) %>%
  filter(metodo == "D Net") %>%
  group_by(campanha, rio, trecho) %>%   # ← rio precisa estar aqui
  mutate(
    abundancia_total = sum(Abundancia),
    abundancia_relativa = Abundancia / abundancia_total * 100,
    familia = if_else(abundancia_relativa <= 5, "Outros", familia)
  ) %>%
  group_by(campanha, rio, trecho, familia) %>%
  summarise(
    abundancia_relativa = sum(abundancia_relativa),
    .groups = "drop"
  ) %>%
  mutate(
    campanha = case_when(
      campanha == "campanha_1" ~ "C1",
      campanha == "campanha_2" ~ "C2",
      campanha == "campanha_3" ~ "C3",
      campanha == "campanha_4" ~ "C4"
    ),
    rio = case_when(
      rio == "rio_bebedouro" ~ "Rio Bebedouro",
      rio == "rio_da_barra" ~ "Rio da Barra",
      rio == "rio_mutuca" ~ "Rio Mutuca"
    ),
    trecho = case_when(
      trecho == "RioBebedouro-P1" ~ "P1",
      trecho == "RioBebedouro-P2" ~ "P2",
      trecho == "RioBebedouro-P3" ~ "P3",
      trecho == "RiodaBarra-P1/2" ~ "P1",
      trecho == "RiodaBarra-P2" ~ "P2",
      trecho == "RiodaBarra-P3" ~ "P3",
      trecho == "RioMutuca-P1" ~ "P1",
      trecho == "RioMutuca-P2" ~ "P2",
      trecho == "RioMutuca-P3" ~ "P3"
    ),
    rio = fct_relevel(
      rio,
      c("Rio Bebedouro", "Rio da Barra", "Rio Mutuca")
    ),
    etiqueta = campanha,
    familia = as.factor(familia)
  )

plot <-
  ggplot(
    df_long, 
    aes(
      x = etiqueta,
      y = abundancia_relativa, 
      fill = familia
    )
  ) +
  geom_bar(stat = "identity") +
  facet_wrap(~ interaction(rio, trecho), scales = "free_x") +
  theme_classic() +
  scale_fill_manual(values = cores_50_cb) +
  guides(fill = guide_legend(ncol = 1)) +
  labs(
    x = "Pontos e campanhas de amostragem",
    y = "Abundância relativa (%)",
    fill = "Taxons"
  )


   #%>%
  # mutate(
  #   estrato = fct_relevel(
  #     estrato,
  #     c("praia_superior", "meio", "oceano_inferior")
  #   ),
  #   Especie = fct_relevel(
  #     Especie,
  #     c("ourico", "ofiuridio", "pepino", "satelite" )
  #   ),
    # eixo = interaction(estrato, ponto, replica, sep = "_"),
    # ponto = fct_relevel(
    #   ponto,
    #   c("direita", "esquerda")
    # )
  # ) %>%
  # mutate(
  #   eixo = factor(eixo, levels = unique(eixo)
  #   )
  # ) %>% 
  # mutate(
  #   estrato = fct_recode(
  #     estrato,
  #     "Superior" = "praia_superior",
  #     "Intermediário" = "meio",
  #     "Inferior" = "oceano_inferior",
  #   ),
  #   ponto = fct_recode(
  #     ponto,
  #     "Leste" = "direita",
  #     "Oeste" = "esquerda"
  #   ),
  #   Especie = fct_recode(
  #     Especie,
  #     "Eucidaris tribuloides" = "satelite",
  #     "Echinometra lucunter" = "ourico",
  #     "Desc. em Ophiuroidea" = "ofiuridio",
  #     "Holothuria grisea" = "pepino"
  #   ),
  #   label = interaction(ponto, replica, sep = " ")
  # )
