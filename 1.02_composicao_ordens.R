library(tidyverse)
library(ggpubr)


df <- read.csv("dados/processado/matriz_comunidades_ordens_imaturos_largo.csv")
colnames(df)
taxa <- colnames(df)[-(1:4)]

df_long <- df %>%
  pivot_longer(
    cols = taxa,
    names_to = "Ordem",
    values_to = "Abundancia"
  ) #%>%
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


plot <- 
  ggplot(
    df_long, 
    aes(
      x = interaction(campanha, rio, locality_name, sep = "_"),
      y = Abundancia, fill = Ordem
    )
  ) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~campanha, scales = "free_x") +
  theme_classic() +
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
