# Resumo da base de dados. Agora considerando rio e campanha
# Quantos insetos foram coletados? (Por rio e campanha)
# Qual a riqueza de classes, ordens e familias? (Por rio e campanha)


############################### Ambiente #######################################
library(tidyverse)
library(ggpubr)

# setwd("C:/Users/Giovana/Desktop/mestrado_giovana/")

# Pasta de tabelas
pasta_tabelas <- "tabelas/dados_descritivos/"
if (!dir.exists(pasta_tabelas)) {
  dir.create(pasta_tabelas, recursive = TRUE)
}

# Pasta de figuras
pasta_figuras <- "figuras/dados_descritivos/"
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


############################## Processamento ###################################
df <- read_csv("dados/processado/imaturos_piata_limpo.csv")


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
  file.path(pasta_tabelas, "sumario_abundancia_POR_rio_campanha.csv")
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
  mutate(riqueza_relativa_a_campanha = riqueza_ordem/ riqueza_total_da_campanha)

write_csv(
  sumario_riqueza_ordens,
  file.path(pasta_tabelas, "sumario_riqueza_ordens_POR_rio_campanha.csv")
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
  file.path(pasta_tabelas, "sumario_riqueza_familias_POR_rio_campanha.csv")
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


