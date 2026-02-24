# Resumo da base de dados. Agora considerando rio e campanha
# Quantos insetos foram coletados? (Por rio e campanha)
# Qual a riqueza de classes, ordens e familias? (Por rio e campanha)
# Quais táxons mais e menos representativos? (Por rio e campanha) SERÁ??


############################### Ambiente #######################################
library(tidyverse)
library(RColorBrewer)
# setwd("C:/Users/Giovana/Desktop/mestrado_giovana/")


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

# Pie chart por rio
plot <- 
  ggplot(
    sumario_especimes, aes(x = "", y = abundancia_relativa, fill = rio)
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



################################### Descarte ###################################
cores_contraste_escuro <- c(
  "#1B9E77", "#D95F02", "#7570B3", "#E7298A",
  "#66A61E", "#E6AB02", "#A6761D", "#666666",
  "#8DD3C7", "#FB8072", "#80B1D3", "#FDB462",
  "#B3DE69", "#FCCDE5", "#BC80BD", "#CCEBC5", "#219107"
)