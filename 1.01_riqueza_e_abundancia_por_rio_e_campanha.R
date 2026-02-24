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

# Pasta de tabelas
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


# Abundancia -------------------------------------------------------------------
sumario_especimes <- df %>%
  group_by(rio, campanha) %>% 
  summarise(abundancia_especimes = sum(abundance)) %>%
  ungroup() %>%
  group_by(campanha) %>%
  mutate(abundancia_total_campanha = sum(abundancia_especimes)) %>%
  ungroup() %>%
  mutate(abundancia_relativa = abundancia_especimes / abundancia_total_campanha)

write_csv(
  sumario_especimes,
  file.path(pasta_tabelas, "sumario_abundancia_POR_rio_campanha.csv")
)


# Pie chart por rio
plot_especimes <- 
  ggplot(
    sumario_especimes, aes(x = "", y = abundancia_relativa, fill = rio)
  ) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ campanha) +
  labs(x = NULL, y = NULL, fill = "Campanha") +
  geom_text(aes(
    label = paste0(round(abundancia_relativa, 2) * 100, "%")),
    position = position_stack(vjust = 0.5), 
    size = 3
  ) +
  theme_minimal() +
  scale_fill_manual(values = cores) +
  theme(legend.position = "right") +
    theme(
    legend.position = "right",
    axis.text = element_blank(),     # remove números
    axis.ticks = element_blank(),    # remove ticks
    panel.grid = element_blank()     # remove linhas de grade
  ) 

ggsave(
  file.path(pasta_figuras, "piechart_abundancia_POR_rio_campanha.png"),
  plot_especimes,
  width = 10,
  height = 6
)

# Riqueza ----------------------------------------------------------------------

# Ordem---------------------------------
sumario_riqueza_ordens <- df %>%
  group_by(rio, campanha) %>% 
  summarise(
    riqueza_ordem = n_distinct(order)
  ) %>%
  ungroup() %>%
  group_by(campanha) %>%
  mutate(riqueza_total = sum(riqueza_ordem)) %>%
  mutate(riqueza_relativa = riqueza_ordem/ riqueza_total)

plot <- 
  ggplot(
    sumario_riqueza_ordens, aes(x = "", y = riqueza_relativa, fill = rio)
  ) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ campanha) +
  labs(x = NULL, y = NULL, fill = "Campanha") +
  theme_void() +
  theme(legend.position = "right")

# riqueza_familia------------
sumario_riqueza_familia <- df %>%
  group_by(rio, campanha) %>% 
  summarise(
    riqueza_familia = n_distinct(family)
  ) %>%
  ungroup() %>%
  group_by(campanha) %>%
  mutate(riqueza_total = sum(riqueza_familia)) %>%
  mutate(riqueza_relativa = riqueza_familia/ riqueza_total)

# Pie chart por rio
plot <- 
  ggplot(
    sumario_riqueza_familia, aes(x = "", y = riqueza_relativa, fill = rio)
  ) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ campanha) +
  labs(x = NULL, y = NULL, fill = "Campanha") +
  theme_void() +
  theme(legend.position = "right")





sumario_abundancia_ordens <- df %>%
  group_by(rio, campanha, order) %>% 
  summarise(abundancia_ordens = sum(abundance)) %>%
  ungroup() %>%
  group_by(rio, campanha) %>%
  mutate(total_tratamento = sum(abundancia_ordens))%>%
  ungroup() %>%
  mutate(abundancia_relativa = abundancia_ordens / total_tratamento) 


# Transformar abaixo de 1% em outros
sumario_abundancia_ordens <-
sumario_abundancia_ordens %>%
mutate(order = ifelse(abundancia_relativa < 0.02, "outros", order)) %>%
group_by(rio, campanha, order) %>%
summarise(abundancia_relativa = sum(abundancia_relativa))
  
# Plot -------------------------------------------------------------------------
# Garantir que a ordem das campanhas fique correta

sumario_abundancia_ordens <- 
  sumario_abundancia_ordens %>%
  mutate(
    campanha = factor(
      campanha, 
      levels = c("campanha_1", "campanha_2", "campanha_3", "campanha_4")
    )
  )

# gráfico de barras empilhadas
plot <- ggplot(sumario_abundancia_ordens,
       aes(x = campanha, y = abundancia_relativa, fill = order)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ rio) +
  scale_fill_manual(values = cores_contraste_escuro) +
  labs(x = "Campanha", 
       y = "Abundância relativa",
       fill = "Ordem") +
  theme_minimal(base_size = 8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))
plot

# Riqueza ----------------------------------------------------------------------

# riqueza por classe ---
sumario_riqueza_classe <- df %>%
  group_by(rio, campanha, class) %>%
  summarise(riqueza = n_distinct(family))

# riqueza por ordem ---
sumario_riqueza_ordem <- df %>%
  group_by(rio, campanha, order) %>%
  summarise(riqueza = n_distinct(family))

# riqueza por rio e campanha ---
sumario_riqueza_rio_campanha <- df %>%
  group_by(rio, campanha) %>%
  summarise(riqueza = n_distinct(family))


