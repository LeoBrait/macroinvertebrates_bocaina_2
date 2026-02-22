# Próximos passos
## 1. Fazer gráfico de abundância relativa por ordem
## 2. Fazer gráfico de abundância relativa por família
## 3. Renomear as campanhas e o nome dos pontos 
##### Ex.Rio Bebedouro - RBE - Campanha 1 - C1 

##############################Processamento#####################################

library(tidyverse)

df <- read.csv("dados/processado/matriz_comunidades_ordens_imaturos_largo.csv")

cores <- c(
 "Odonata" = "#6c292bff", # Vermelho forte
  "Hemiptera" = "#1b4e78", # Azul forte
  "Coleoptera" = "#2f7e2c", # Verde forte
  "Diptera" = "#9122a2", # Roxo
  "Ephemeroptera" = "#cd7218", # Laranja vivo
  "Veneirodea" = "#efc611", # Amarelo vibrante
  "Basommatophora" = "#A65628", # Marrom avermelhado
  "Oligochaeta" = "#bd0467", # Rosa choque
  "Plecoptera" = "#7e7575", # Cinza escuro
  "Amphipoda" = "#17795a", # Verde-água forte
  "Megaloptera" = "#792301", # Laranja avermelhado
  "Architaenioglossa" = "#607199", # Azul lavanda
  "Lepidoptera" = "#de7fba", # Rosa magenta
  "Littorinimorpha" = "#2d2e2b", # Verde limão forte
  "Tricladida" = "#0e3a62",  # Amarelo ouro
  "Trichoptera" = "#d1b58b"  # Bege
)

################################################################################

# Nome dos táxons
colnames(df)

taxa <- colnames(df)[-(1:4)]

# Transformar tabela larga em longa
df_long <- df %>% 
  pivot_longer(
    cols = taxa,
    names_to = "Ordem",
    values_to = "Abundancia"
  ) %>%
  filter(method == "D Net") %>%
  group_by(campanha, locality_name) %>%

  # Abundancias relativa 
  mutate(
    abundancia_total = sum(Abundancia),
    abundancia_relativa = Abundancia / abundancia_total * 100
  ) %>% 
  ungroup() %>%

  #Criação das etiquetas
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
    locality_name = case_when(
    locality_name == "RioBebedouro-P1" ~ "P1",
    locality_name == "RioBebedouro-P2" ~ "P2",
    locality_name == "RioBebedouro-P3" ~ "P3",
    locality_name == "RiodaBarra-P1/2" ~ "P1",
    locality_name == "RiodaBarra-P2" ~ "P2",
    locality_name == "RiodaBarra-P3" ~ "P3",
    locality_name == "RioMutuca-P1" ~ "P1",
    locality_name == "RioMutuca-P2" ~ "P2",
    locality_name == "RioMutuca-P3" ~ "P3"
    ),
    etiqueta = paste(campanha,  sep = ""),
    Ordem = as.factor(Ordem),
    Ordem = fct_recode(
      Ordem, 
      "Oligochaeta" =  "Desc..O..Classe.Oligochaeta"
    )
  ) %>%
  ungroup() %>%
  mutate(
    Ordem = as.factor(Ordem),
    Ordem = fct_relevel(
      Ordem, 
      c(
      "Littorinimorpha",
      "Lepidoptera",
      "Architaenioglossa",
      "Plecoptera",
      "Amphipoda",
      "Megaloptera",
      "Tricladida",
      "Basommatophora",
      "Oligochaeta",
     "Veneirodea",
     "Ephemeroptera",
      "Trichoptera",
       "Diptera",
      "Coleoptera",
      "Hemiptera",
      "Odonata"
    )
   )
  )

# Criar gráfico
plot <-
  ggplot(
    df_long, 
    aes(
      x = etiqueta,
      y = abundancia_relativa, 
      fill = Ordem
    )
  ) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ interaction(rio, locality_name),  scales = "free_x") +
  theme_classic() +
  scale_fill_manual(values = cores) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1)) +
  
  # scale_x_discrete(labels = function(x) df_long$label[match(x, df_long$eixo)]) +
  # Colocar legenda em negrito
  theme(
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    text = element_text(size = 16)
  ) +
  labs(
    x = "Pontos e campanhas de amostragem",
    y = "Abundância relativa (%)",
    fill = "Taxons"
  )

ggsave(
  "figuras/abundancia_insetos.png",
  plot,
  width = 10,
  height = 6
)

