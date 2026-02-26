# Próximos passos
## 1. Fazer gráfico de abundância relativa por ordem
## 2. Fazer gráfico de abundância relativa por família
## 3. Renomear as campanhas e o nome dos pontos 
##### Ex.Rio Bebedouro - RBE - Campanha 1 - C1 

##############################Processamento#####################################

library(tidyverse)
library(ggpubr)

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
  "Trichoptera" = "#d1b58b",
  "Sorbeoconcha" = "#0084ff"
)

############################## Preprocessamento ################################

# Nome dos táxons
colnames(df[5:length(df)])
taxa <- colnames(df)[-(1:4)]

# Ajustes do dado
df_long <- df %>% 
  pivot_longer(
    cols = all_of(taxa),
    names_to = "order",
    values_to = "abundance"
  ) %>%
  group_by(campanha, rio, locality_name, method) %>%
  mutate(
    abundancia_total = sum(abundance),
    abundancia_relativa = abundance / abundancia_total * 100
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
      rio == "rio_mutuca" ~ "Rio Mutuca",
      rio == "rio_da_serra" ~ "Rio da Serra",
      rio == "rio_carrapicho" ~ "Rio Carrapicho"
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
    etiqueta = paste(locality_name, campanha,  sep = ""),
    order = as.factor(order),
    order = fct_recode(
      order, 
      "Oligochaeta" =  "Desc..O..Classe.Oligochaeta"
    )
  ) %>%
  ungroup() %>%
  mutate(
    order = fct_relevel(
      order, 
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

# Preencher campanhas ausentes para melhor visualização
niveis_campanha <- c("C1", "C2", "C3", "C4")

df_long <- df_long %>%
  mutate(campanha = factor(campanha, levels = niveis_campanha))


############################ Separar Tabelas por rio ###########################

rio_bebedouro <- df_long %>% 
  filter(rio == "Rio Bebedouro") %>%
    complete(
    rio,
    method,
    locality_name,
    campanha,
    order,
    fill = list(
      abundance = 0,
      abundancia_relativa = 0
    )
  )

rio_da_barra <- df_long %>% 
  filter(rio == "Rio da Barra")

rio_mutuca <- df_long %>% 
  filter(rio == "Rio Mutuca")


########################### Plotagem da composição por ordem ###################

plot_bebedouro_dnet <- 
  ggplot(
    rio_bebedouro %>%
    filter(method == "D Net"),
    aes(
      x = campanha,
      y = abundancia_relativa, 
      fill = order
    )
  ) +
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  facet_wrap(~locality_name,  scales = "free_x", space = "free_x") +
  scale_x_discrete(drop = FALSE) +
  theme_classic() +
  scale_fill_manual(values = cores) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1)) +
  theme(
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    text = element_text(size = 16)
  ) +
  labs(
    x = "",
    y = "Abundância relativa (%)",
    fill = "Taxons"
  )


plot_bebedouro_poolnet <- 
  ggplot(
    rio_bebedouro %>%
      filter(method == "Pool Net"),
    aes(
      x = campanha,
      y = abundancia_relativa, 
      fill = order
    )
  ) +
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  facet_wrap(~locality_name,  scales = "free_x", space = "free_x") +
  theme_classic() +
  scale_fill_manual(values = cores) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1)) +
  theme(
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    text = element_text(size = 16)
  ) +
  labs(
    x = "",
    y = "Abundância relativa (%)",
    fill = "Taxons"
  )

plot_bebedouro_sieve <-
  ggplot(
    rio_bebedouro %>%
      filter(method == "Sieve"),
    aes(
      x = campanha,
      y = abundancia_relativa, 
      fill = order
    )
  ) +
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  facet_wrap(~locality_name,  scales = "free_x", space = "free_x") +
  theme_classic() +
  scale_fill_manual(values = cores) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1)) +
  theme(
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    text = element_text(size = 16)
  ) +
  labs(
    x = "",
    y = "Abundância relativa (%)",
    fill = "Taxons"
  )




merged_plot <- ggarrange(plot_bebedouro_poolnet, plot_bebedouro_dnet, plot_bebedouro_sieve, ncol = 1)




################################# Descarte #####################################
# Criar gráfico
plot <-
  ggplot(
    df_long, 
    aes(
      x = etiqueta,
      y = abundancia_relativa, 
      fill = order
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

