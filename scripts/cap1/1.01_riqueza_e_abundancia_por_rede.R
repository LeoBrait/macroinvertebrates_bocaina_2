# Resumo da base de dados. 
# Quantos insetos foram coletados? 
# Quantos táxons? Em cada Classe, Em cada ordem
# Quais táxons mais e menos representativos? 

############################### Ambiente #######################################
library(tidyverse)
library(forcats)
library(ggpubr)
# setwd("C:/Users/Giovana/Desktop/mestrado_giovana/")


# Pasta de tabelas
pasta_tabelas <- "tabelas/cap1/dados_descritivos/"
if (!dir.exists(pasta_tabelas)) {
  dir.create(pasta_tabelas, recursive = TRUE)
}

pasta_figuras <- "figuras/cap1/dados_descritivos/"
if (!dir.exists(pasta_figuras)) {
  dir.create(pasta_figuras, recursive = TRUE)
}

######################## Carregar e Limpar Dados ###############################
rios <- c("rio_bebedouro", "rio_da_barra", "rio_mutuca")
amostras_descartadas <- c("RiodaBarra-P1")
metodos <- c("D Net", "Pool Net", "Sieve")

df_cru <- read_csv("dados/processado/imaturos_piata_limpo.csv")

df <- df_cru %>%
  filter(rio %in% rios) %>%
  filter(!locality_name %in% amostras_descartadas) %>%
  filter(method %in% metodos) %>%
  group_by(campanha, rio, locality_name) %>%
  filter(all(metodos %in% method)) %>%
  ungroup()

########################## Processamento e Análise #############################

# Redes -----------------------------
sumario_redes_riqueza <- df %>%
  group_by(method) %>%
  summarise(
    abundancia_total = sum(abundance),
    riqueza_classes = n_distinct(class),
    riqueza_ordens = n_distinct(order),
    riqueza_familias = n_distinct(family)
  )
write.csv(
  sumario_redes_riqueza,
  file.path(pasta_tabelas, "bacia_sumario_redes_riqueza.csv")
)


sumario_classes_redes <- df %>%
  group_by(method) %>%
  mutate(
    abundancia_total = sum(abundance)
  ) %>%
  group_by(method, class) %>%
  summarise(
    abundancia_classe_na_rede = sum(abundance),
    abundancia_relativa_classe_na_rede = 
      (abundancia_classe_na_rede / first(abundancia_total)) * 100,
  )
write.csv(
  sumario_classes_redes,
  file.path(pasta_tabelas, "bacia_sumario_classes_redes.csv")
)


sumario_ordens_redes <- df %>%
  group_by(method, order) %>%
  summarise(
    abundancia_ordem_na_rede = sum(abundance),
    riqueza_familias_da_ordem_por_rede = n_distinct(family)
  ) %>%
  ungroup() %>%
  group_by(method) %>%
  mutate(
    total_rede = sum(abundancia_ordem_na_rede),
    abundancia_relativa_ordem_na_rede = 
    (abundancia_ordem_na_rede / total_rede) * 100
  )
write.csv(
  sumario_ordens_redes,
  file.path(pasta_tabelas, "bacia_sumario_ordens_redes.csv")
)


sumario_familias_redes <- df %>%
  group_by(method, family) %>%
  summarise(
    order = first(order),
    abundancia_familia_na_rede = sum(abundance)
  ) %>%
  ungroup() %>%
  group_by(method) %>%
  mutate(
    total_rede = sum(abundancia_familia_na_rede),
    abundancia_relativa_familia_na_rede = 
      (abundancia_familia_na_rede / total_rede) * 100
  )
write.csv(
  sumario_familias_redes,
  file.path(pasta_tabelas, "bacia_sumario_familias_redes.csv")
)

##################################### Plots ####################################

# Arrumando tabelas para plotagem ----------------------------------------------
sumario_ordens_redes <- sumario_ordens_redes %>%
  mutate(
    order = recode(
      order,
      "Desc. O. Classe Oligochaeta" = "Oligochaeta"
    ),
    method = recode(
      method,
      "Sieve" = "Peneira",
      "Pool Net" = "Rede de Piscina",
      "D Net" = "Rede D"
    )
  )

sumario_familias_redes <- sumario_familias_redes %>%
  mutate(
    method = recode(
      method,
      "Sieve" = "Peneira",
      "Pool Net" = "Rede de Piscina",
      "D Net" = "Rede D"
    )
  )

vetor_ordenamento_ordens <- sumario_ordens_redes %>%
  group_by(order) %>%
  summarise(
    abundancia_relativa_ordem_na_rede = 
      mean(abundancia_relativa_ordem_na_rede)
  ) %>%
  arrange(abundancia_relativa_ordem_na_rede) %>%
  pull(order)

vetor_ordenamento_familias <- sumario_familias_redes %>%
  group_by(family) %>%
  summarise(
    abundancia_relativa_familia_na_rede = 
      mean(abundancia_relativa_familia_na_rede)
  ) %>%
  arrange(abundancia_relativa_familia_na_rede) %>%
  pull(family)

# Ordenar
sumario_ordens_redes <- sumario_ordens_redes %>%
  mutate(
    order = factor(order, levels = vetor_ordenamento_ordens)
  )

sumario_familias_redes <- sumario_familias_redes %>%
  mutate(
    family = factor(family, levels = vetor_ordenamento_familias)
  )


# Plots ------------------------------------------------------------------------
grafico_ordens_relativo <-
  ggplot(
    sumario_ordens_redes,
    aes(
      x = order,
      y = abundancia_relativa_ordem_na_rede,
      fill = method
    )
  ) +

  geom_col(
    position = position_dodge(width = 0.8),
           width = 0.7) +

  #scale_y_continuous(
    #limits = c(0, 15),
    #breaks = seq(0, 15, 5),
    #expand = c(0, 0)
  #) +

  labs(
    x = "Ordens",
    y = "Abundância relativa (%)",
    fill = "Método de coleta"
  ) +

  theme_classic(base_size = 14) +

  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "right"
  )

grafico_ordens_absoluto <-
  ggplot(
    sumario_ordens_redes,
    aes(
      x = order,
      y = abundancia_ordem_na_rede,
      fill = method
    )
  ) +

  geom_col(
    position = position_dodge(width = 0.8),
           width = 0.7) +

  #scale_y_continuous(
    #limits = c(0, 15),
    #breaks = seq(0, 15, 5),
    #expand = c(0, 0)
  #) +

  labs(
    x = "Ordens",
    y = "Abundância absoluta",
    fill = "Método de coleta"
  ) +

  theme_classic(base_size = 14) +

  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      face = "italic"
    ),
    legend.position = "right"
  )

merged_plot <- ggarrange(
  grafico_ordens_relativo,
  grafico_ordens_absoluto,
  ncol = 1,
  nrow = 2,
  common.legend = TRUE,
  legend = "right",
  labels = c("A", "B"),
  align = "v",
  heights = c(0.85, 1)
)

ggsave(
  filename = paste0(pasta_figuras, "ordens_rede_absoluto_relativo.png"),
  plot = merged_plot,
  width = 12,
  height = 12,
  dpi = 300
)

################################## Desuso ######################################


# grafico_familia <-
#   ggplot(
#     sumario_familias_redes,
#     aes(
#       x = family,
#       y = abundancia_relativa_familia_na_rede,
#       fill = method
#     )
#   ) +

#   geom_col(
#     position = position_dodge(width = 0.8),
#            width = 0.7) +

#   #scale_y_continuous(
#     #limits = c(0, 15),
#     #breaks = seq(0, 15, 5),
#     #expand = c(0, 0)
#   #) +

#   labs(
#     x = "Familias",
#     y = "Abundância relativa (%)",
#     fill = "Método de coleta"
#   ) +

#   theme_classic(base_size = 14) +

#   theme(
#     axis.text.x = element_text(
#       angle = 45,
#       hjust = 1,
#       face = "italic"
#     ),
#     legend.position = "right"
#   )

# dev.off()
