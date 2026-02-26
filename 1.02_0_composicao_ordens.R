
################################## Ambiente ####################################

library(tidyverse)
library(ggpubr)

# Pasta de figuras
pasta_figuras <- "figuras/composicao/"
if (!dir.exists(pasta_figuras)) {
  dir.create(pasta_figuras, recursive = TRUE)
}

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


################################### Funcões ####################################
draw_stacked_bar <- function(data) {
  
  plot <- 
    ggplot(
      data,
      aes(
        x = campanha,
        y = abundancia_relativa, 
        fill = order
    )
    ) +
    geom_bar(stat = "identity", position = "stack", width = 0.9) +
    facet_wrap(
      ~interaction(rio, locality_name),  
      scales = "free_x", 
      space = "free_x"
    ) +
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
  return(plot)


}

#################################### Dados #####################################

df <- read.csv("dados/processado/matriz_comunidades_ordens_imaturos_largo.csv")

############################## Prepr`ocessamento ################################


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
  group_by(campanha, rio, locality_name, method, order) %>%
  summarise(abundance = sum(abundance)) %>%
  ungroup() %>%
  group_by(campanha, rio, locality_name) %>%
  mutate(abundancia_total = sum(abundance)) %>%
  ungroup() %>%
  mutate(abundancia_relativa = abundance / abundancia_total * 100) %>%
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
   ),
   rio = fct_relevel(
     rio,
     c(
       "Rio da Barra",
       "Rio Bebedouro",
       "Rio Mutuca",
       "Rio Carrapicho",
       "Rio da Serra"
     )
   ),
   locality_name = fct_relevel(
     locality_name,
     c(
       "P1",
       "P2",
       "P3",
       "P4"
     )
   )
  ) 



niveis_campanha <- c("C1", "C2", "C3", "C4")

df_long <- df_long %>%
  mutate(campanha = factor(campanha, levels = niveis_campanha)) %>%
    complete(
    campanha,
    rio,
    locality_name,
    order,
    fill = list(
      abundance = 0,
      abundancia_relativa = 0
    )
  ) %>%
  drop_na(locality_name)


############################ Separar Tabelas por rio ###########################

rio_da_barra <- df_long %>% 
  filter(rio == "Rio da Barra")

rio_bebedouro <- df_long %>% 
  filter(rio == "Rio Bebedouro")

rio_mutuca <- df_long %>% 
  filter(rio == "Rio Mutuca")

rio_carrapicho <- df_long %>% 
  filter(rio == "Rio Carrapicho")

rio_da_serra <- df_long %>% 
  filter(rio == "Rio da Serra")


########################### Plotagem da composição por ordem ###################

plot_barra <- draw_stacked_bar(rio_da_barra)
plot_bebedouro <- draw_stacked_bar(rio_bebedouro)
plot_mutuca <- draw_stacked_bar(rio_mutuca)
plot_carrapicho <- draw_stacked_bar(rio_carrapicho)
plot_serra <- draw_stacked_bar(rio_da_serra)

merged_plot <- ggarrange(
  plot_barra, 
  plot_bebedouro, 
  plot_mutuca, 
  plot_carrapicho, 
  plot_serra, 
  ncol = 1, 
  common.legend = TRUE, 
  legend = "right",
  labels = c("A", "B", "C", "D", "E"),
  font.label = list(size = 16, face = "bold")
)



ggsave(
  paste0(pasta_figuras, "composicao_ordens_por_campanha_e_rio_GERAL.png"),  
  plot = merged_plot,
  width = 12, height = 20, dpi = 300
)

ggsave(
  paste0(pasta_figuras, "composicao_ordens_por_campanha_e_rio_GERAL.svg"),  
  plot = merged_plot,
  width = 12, height = 20, dpi = 300
)
