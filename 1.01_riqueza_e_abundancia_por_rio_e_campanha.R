# 1. Qual a riqueza e abundancia de cada classe por rio em cada campanha
# 2. Qual a riqueza e abundancia de cada ordem por rio em cada campanha
# 3. Qual a composição relativa de familias por rio em cada campanha

############################### Ambiente #######################################
library(tidyverse)
library(RColorBrewer)
# setwd("C:/Users/Giovana/Desktop/mestrado_giovana/")


############################## Processamento ###################################
df <- read_csv("dados/processado/imaturos_piata_limpo.csv")

cores_contraste_escuro <- c(
  "#1B9E77", "#D95F02", "#7570B3", "#E7298A",
  "#66A61E", "#E6AB02", "#A6761D", "#666666",
  "#8DD3C7", "#FB8072", "#80B1D3", "#FDB462",
  "#B3DE69", "#FCCDE5", "#BC80BD", "#CCEBC5"
)

 
df <- read_csv("dados/processado/imaturos_piata_limpo.csv")

############################## Processamento ###################################


# Abundancia -------------------------------------------------------------------
sumario_abundancia_especimes <- df %>%
  group_by(rio, campanha) %>% 
  summarise(abundancia_especimes = sum(abundance)) %>%
  ungroup() %>%
  mutate(abundancia_total = sum(abundancia_especimes)) %>%
  mutate(abundancia_relativa = abundancia_especimes / abundancia_total)

sumario_abundancia_ordens_por_rio_e_campanha <- df %>%
  group_by(rio, campanha, order) %>% 
  summarise(abundancia_ordens = sum(abundance)) %>%
  ungroup() %>%
  group_by(rio, campanha) %>%
  mutate(total_tratamento = sum(abundancia_ordens))%>%
  ungroup() %>%
  mutate(abundancia_relativa = abundancia_ordens / total_tratamento) 


# Transformar abaixo de 1% em outros
sumario_abundancia_ordens_por_rio_e_campanha <-
sumario_abundancia_ordens_por_rio_e_campanha %>%
mutate(order = ifelse(abundancia_relativa < 0.05, "outros", order)) %>%
group_by(rio, campanha, order) %>%
summarise(abundancia_relativa = sum(abundancia_relativa))
  
# Plot -------------------------------------------------------------------------
# Garantir que a ordem das campanhas fique correta

sumario_abundancia_ordens_por_rio_e_campanha <- 
  sumario_abundancia_ordens_por_rio_e_campanha %>%
  mutate(
    campanha = factor(
      campanha, 
      levels = c("campanha_1", "campanha_2", "campanha_3", "campanha_4")
    )
  )


# gráfico de barras empilhadas
ggplot(sumario_abundancia_ordens_por_rio_e_campanha,
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
