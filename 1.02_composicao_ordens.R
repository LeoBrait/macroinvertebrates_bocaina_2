# Próximos passos
## 1. Fazer gráfico de abundância relativa por ordem
## 2. Fazer gráfico de abundância relativa por família
## 3. Renomear as campanhas e o nome dos pontos 
##### Ex.Rio Bebedouro - RBE - Campanha 1 - C1 


library(tidyverse)


cores <- c(
 "Odonata" = "#E41A1C", # Vermelho forte
  "Hemiptera" = "#377EB8", # Azul forte
  "Coleoptera" = "#4DAF4A", # Verde forte
  "Diptera" = "#984EA3", # Roxo
  "Ephemeroptera" = "#FF7F00", # Laranja vivo
  "Veneirodea" = "#FFD92F", # Amarelo vibrante
  "Basommatophora" = "#A65628", # Marrom avermelhado
  "Oligochaeta" = "#F781BF", # Rosa choque
  "Plecoptera" = "#999999", # Cinza escuro
  "Amphipoda" = "#66C2A5", # Verde-água forte
  "Megaloptera" = "#FC8D62", # Laranja avermelhado
  "Architaenioglossa" = "#8DA0CB", # Azul lavanda
  "Lepidoptera" = "#E78AC3", # Rosa magenta
  "Littorinimorpha" = "#A6D854", # Verde limão forte
  "Tricladida" = "#0e3a62",  # Amarelo ouro
  "Trichoptera" = "#E5C494"  # Bege
)





df <- read.csv("dados/processado/matriz_comunidades_ordens_imaturos_largo.csv")
colnames(df)
taxa <- colnames(df)[-(1:4)]

df_long <- df %>%
  pivot_longer(
    cols = taxa,
    names_to = "Ordem",
    values_to = "Abundancia"
  ) %>%
  filter(method == "D Net") %>%
  group_by(campanha, rio, locality_name) %>%
  # Abundancias relativa 
  mutate(
    abundancia_total = sum(Abundancia),
    abundancia_relativa = Abundancia / abundancia_total * 100
  ) %>% 
  ungroup() %>%
  #Criação das etiquetas
  mutate(
    local = case_when(
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
    campanha = case_when(
      campanha == "campanha_1" ~ "C1",
      campanha == "campanha_2" ~ "C2",
      campanha == "campanha_3" ~ "C3",
      campanha == "campanha_4" ~ "C4"
    ),
    etiqueta = paste(local, campanha,  sep = ""),
    #Ordem = as.factor(Ordem),
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


    





plot <-
  ggplot(
    df_long,
    aes(
      x = etiqueta,
      y = abundancia_relativa, fill = Ordem
    )
  ) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~rio, scales = "free_x") +
  theme_classic() +
  scale_fill_manual(values = cores) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  # scale_x_discrete(labels = function(x) df_long$label[match(x, df_long$eixo)]) +
  # Colocar legenda em negrito
  theme(
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    text = element_text(size = 16)
  ) +
  labs(
    x = "Rio_Localidade_Campanha",
    y = "Abundância",
    fill = "Espécie"
  )

ggsave(
  "figuras/abundancia_ouricos_estratos.png",
  plot,
  width = 10,
  height = 6
)

# somar bichos
df_summarised <- df_long %>%
  group_by(Especie) %>%
  summarise(
    abundancia_individual = sum(Abundancia)) %>%
  ungroup() %>%
  mutate(
    abundancia_total = sum(abundancia_individual),
    abundancia_relativa = abundancia_individual / abundancia_total
  )

df_summarised <- df_long %>%
  group_by(estrato) %>%
  summarise(
    abundancia_individual = sum(Abundancia)) %>%
  ungroup() %>%
  mutate(
    abundancia_total = sum(abundancia_individual),
    abundancia_relativa = abundancia_individual / abundancia_total
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
