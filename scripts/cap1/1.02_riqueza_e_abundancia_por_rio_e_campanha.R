# Resumo da base de dados. Agora considerando rio e campanha
# Quantos insetos foram coletados? (Por rio e campanha)
# Qual a riqueza de classes, ordens e familias? (Por rio e campanha)


############################### Ambiente #######################################
library(tidyverse)
library(ggpubr)

# setwd("C:/Users/Giovana/Desktop/mestrado_giovana/")

# Pasta de tabelas
pasta_tabelas <- "tabelas/cap1/dados_descritivos/"
if (!dir.exists(pasta_tabelas)) {
  dir.create(pasta_tabelas, recursive = TRUE)
}

# Pasta de figuras
pasta_figuras <- "figuras/cap1/dados_descritivos/"
if (!dir.exists(pasta_figuras)) {
  dir.create(pasta_figuras, recursive = TRUE)
}

cores <- c(
  "rio_bebedouro" = "#1B9E77",
  "rio_da_barra" = "#D95F02",
  "rio_mutuca" = "#7570B3",
  "rio_carrapicho" = "#E7298A",
  "rio_da_serra" = "#66A61E"
)


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

####################### Particionamento das campanhas ##########################


#---------------------------------- Abundancia ---------------------------------
sumario_especimes <- df %>%
  group_by(rio, campanha) %>% 
  summarise(abundancia_especimes = sum(abundance)) %>%
  ungroup() %>%
  group_by(campanha) %>%
  mutate(abundancia_total_da_campanha = sum(abundancia_especimes)) %>%
  ungroup() %>%
  mutate(
    abundancia_relativa_a_campanha = 
      abundancia_especimes / abundancia_total_da_campanha
  )

write_csv(
  sumario_especimes,
  file.path(pasta_tabelas, "rio_campanha_sumario_abundancia.csv")
)


plot_especimes <- 
  ggplot(
    sumario_especimes, 
    aes(x = "", y = abundancia_relativa_a_campanha, fill = rio)
  ) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ campanha) +
  labs(x = NULL, y = NULL, fill = "Campanha") +
  geom_text(
    aes(label = paste0(round(abundancia_relativa_a_campanha, 2) * 100, "%")),
    position = position_stack(vjust = 0.5), 
    size = 3
  ) +
  theme_minimal() +
  scale_fill_manual(values = cores) +
  theme(legend.position = "right") +
    theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) 

ggsave(
  file.path(pasta_figuras, "piechart_abundancia_POR_rio_campanha.png"),
  plot_especimes,
  width = 10,
  height = 6
)

#--------------------------------- Riqueza Ordens ------------------------------

sumario_riqueza_ordens <- df %>%
  group_by(rio, campanha) %>% 
  summarise(
    riqueza_ordem = n_distinct(order)
  ) %>%
  ungroup() %>%
  group_by(campanha) %>%
  mutate(riqueza_total_da_campanha = sum(riqueza_ordem)) %>%
  ungroup() %>%
  mutate(riqueza_relativa_a_campanha = riqueza_ordem / riqueza_total_da_campanha)

write_csv(
  sumario_riqueza_ordens,
  file.path(pasta_tabelas, "rio_campanha_sumario_riqueza_ordens.csv")
)

plot_riqueza_ordens <-
  ggplot(
    sumario_riqueza_ordens,
    aes(x = "", y = riqueza_relativa_a_campanha, fill = rio)
  ) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ campanha) +
  labs(x = NULL, y = NULL, fill = "Campanha") +
  geom_text(
    aes(label = paste0(round(riqueza_relativa_a_campanha, 2) * 100, "%")),
    position = position_stack(vjust = 0.5), 
    size = 3
  ) +
  theme_minimal() +
  scale_fill_manual(values = cores) +
  theme(legend.position = "right") +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) 

ggsave(
  file.path(pasta_figuras, "piechart_riqueza_ordens_POR_rio_campanha.png"),
  plot_riqueza_ordens,
  width = 10,
  height = 6
)



#------------------------------ Riqueza Familias -------------------------------
sumario_riqueza_familia <- df %>%
  group_by(rio, campanha) %>% 
  summarise(riqueza_familia = n_distinct(family)) %>%
  ungroup() %>%
  group_by(campanha) %>%
  mutate(riqueza_total_da_campanha = sum(riqueza_familia)) %>%
  ungroup() %>%
  mutate(
    riqueza_relativa_a_campanha = riqueza_familia / riqueza_total_da_campanha
  )

write_csv(
  sumario_riqueza_familia,
  file.path(pasta_tabelas, "rio_campanha_sumario_riqueza_familias.csv")
)


plot_riqueza_familias <- 
  ggplot(
    sumario_riqueza_familia, 
    aes(x = "", y = riqueza_relativa_a_campanha, fill = rio)
  ) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ campanha) +
  labs(x = NULL, y = NULL, fill = "Campanha") +
  geom_text(
    aes(label = paste0(round(riqueza_relativa_a_campanha, 2) * 100, "%")),
    position = position_stack(vjust = 0.5), 
    size = 3
  ) +
  theme_minimal() +
  scale_fill_manual(values = cores) +
  theme(legend.position = "right") +
    theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
  file.path(pasta_figuras, "piechart_riqueza_familias_POR_rio_campanha.png"),
  plot_riqueza_familias,
  width = 10,
  height = 6
)

############################### Analise dos Taxons #############################
# ----------------------------------  Rio --------------------------------------
sumario_ordens_rio <- df %>%
  group_by(rio, order) %>%
  summarise(abundancia_ordem = sum(abundance)) %>%
  ungroup() %>%
  group_by(rio) %>%
  mutate(abundancia_total_macroinvertebrados = sum(abundancia_ordem)) %>%
  ungroup() %>%
  mutate(
    abundancia_relativa_ordem = abundancia_ordem /
      abundancia_total_macroinvertebrados
  )
write_csv(
  sumario_ordens_rio,
  file.path(pasta_tabelas, "rio_sumario_abundancia_ordens.csv")
)


sumario_familias_rio <- df %>%
  group_by(rio, family) %>%
  summarise(abundancia_familia = sum(abundance)) %>%
  ungroup() %>%
  group_by(rio) %>%
  mutate(abundancia_total_macroinvertebrados = sum(abundancia_familia)) %>%
  ungroup() %>%
  mutate(
    abundancia_relativa_familia = abundancia_familia /
      abundancia_total_macroinvertebrados
  )
write_csv(
  sumario_familias_rio,
  file.path(pasta_tabelas, "rio_sumario_abundancia_familias.csv")
)


# -------------------------------  Rio e Local ---------------------------------
sumario_ordens_rio_local <- df %>%
  group_by(rio, locality_name, order) %>%
  summarise(abundancia_ordem = sum(abundance)) %>%
  ungroup() %>%
  group_by(rio, locality_name) %>%
  mutate(abundancia_total_macroinvertebrados = sum(abundancia_ordem)) %>%
  ungroup() %>%
  mutate(
    abundancia_relativa_ordem = abundancia_ordem /
    abundancia_total_macroinvertebrados
  )
write_csv(
  sumario_ordens_rio_local,
  file.path(pasta_tabelas, "ponto_rio_sumario_abundancia_ordens.csv")
)

sumario_familias_rio_local <- df %>%
  group_by(rio, locality_name, family) %>%
  summarise(abundancia_familia = sum(abundance)) %>%
  ungroup() %>%
  group_by(rio, locality_name) %>%
  mutate(abundancia_total_macroinvertebrados = sum(abundancia_familia)) %>%
  ungroup() %>%
  mutate(
    abundancia_relativa_familia = abundancia_familia /
    abundancia_total_macroinvertebrados
  )
write_csv(
  sumario_familias_rio_local,
  file.path(pasta_tabelas, "ponto_rio_sumario_abundancia_familias.csv")
)


# ------------------------------- Rio Local e Campanha -------------------------