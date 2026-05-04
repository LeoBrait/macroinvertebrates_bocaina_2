library(tidyverse)
library(ggpubr)



#configuracoes
rios <- c("rio_bebedouro", "rio_da_barra", "rio_mutuca")
metodos <- c("D Net", "Pool Net", "Sieve")
colunas <- c(
    "campanha", "rio", "locality_name", "method",
    "total_macroinvertebrados", "riqueza_ordens", "riqueza_familias"
)


#carregar dados
df <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv") %>%
  filter(rio %in% rios) %>%
  filter(method %in% metodos) %>%
  select(colunas) %>%
  pivot_longer(
    cols = c("total_macroinvertebrados", "riqueza_ordens", "riqueza_familias"),
    names_to = "categoria_da_resposta",
    values_to =  "valor"
  )


sumario <- df %>%
  group_by(rio, locality_name, method, categoria_da_resposta) %>%
  summarise(
    media = mean(valor),
    desvio_padrao = sd(valor),
    n = n(),
    se =  desvio_padrao / sqrt(n)
  ) %>%
  ungroup()

sumario_rede_d <- sumario %>% filter(method == "D Net")
sumario_piscina <- sumario %>% filter(method == "Pool Net")
sumario_peneira <- sumario %>% filter(method == "Sieve")


#plotar
plot_rede_d <-
  ggplot(
    sumario_rede_d, #ATENÇÃO
    aes(y = media, x = locality_name, fill = categoria_da_resposta)
  ) +
  theme_minimal() +
  geom_errorbar(
    aes(
      ymin = 0.2,
      ymax = media + se
    ),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.9)
  ) +
#   geom_jitter(
#    data = df %>% filter(method == "D Net"), 
#    aes(y = valor, x = locality_name),
#    position = position_jitterdodge(
#       jitter.width = 0.2,
#       dodge.width = 0.9
#     )
#   ) +
  facet_wrap(~ rio, scales = "free_x", ncol = 1) +
  scale_y_continuous( limits = c(0, 125)) 

dev.off()

plot_piscina <-
  ggplot(
    sumario_piscina, #ATENÇÃO
    aes(y = media, x = locality_name, fill = categoria_da_resposta)
  ) +
  theme_minimal() +
  geom_errorbar(
    aes(
      ymin = 0.2,
      ymax = media + se
    ),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.9)
  ) +
#   geom_jitter(
#    data = df %>% filter(method == "D Net"), 
#    aes(y = valor, x = locality_name),
#    position = position_jitterdodge(
#       jitter.width = 0.2,
#       dodge.width = 0.9
#     )
#   ) +
  facet_wrap(~ rio, scales = "free_x", ncol = 1) +
  scale_y_continuous( limits = c(0, 125)) 

dev.off()

plot_peneira <-
  ggplot(
    sumario_peneira, #ATENÇÃO
    aes(y = media, x = locality_name, fill = categoria_da_resposta)
  ) +
  theme_minimal() +
  geom_errorbar(
    aes(
      ymin = 0.2,
      ymax = media + se
    ),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.9)
  ) +
#   geom_jitter(
#    data = df %>% filter(method == "D Net"), 
#    aes(y = valor, x = locality_name),
#    position = position_jitterdodge(
#       jitter.width = 0.2,
#       dodge.width = 0.9
#     )
#   ) +
  facet_wrap(~ rio, scales = "free_x", ncol = 1) +
  scale_y_continuous( limits = c(0, 125)) 

dev.off()

merged_plot <- ggarrange(
    plot_rede_d,
    plot_piscina,
    plot_peneira,
    common.legend = TRUE,
    legend = "right",
    ncol = 3,
    labels = c("REDE D", "PISCINA", "PENEIRA"),
    font.label = list(size = 16, face = "bold")
)

ggsave(
    "barras.png",
    width = 20,
    height = 30,
    dpi = 300
)


    
dev.off()
