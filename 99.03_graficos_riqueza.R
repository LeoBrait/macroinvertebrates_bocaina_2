library(tidyverse)
library(ggpubr)



############################### configuracoes ##################################
rios <- c("rio_bebedouro", "rio_da_barra", "rio_mutuca")
metodos <- c("D Net", "Pool Net", "Sieve")
colunas <- c(
    "campanha", "rio", "locality_name", "method",
    "total_macroinvertebrados", "riqueza_ordens", "riqueza_familias"
)

amostras_descartadas_vetor <- c("RiodaBarra-P1")



#carregar dados
df <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv") %>%
  filter(rio %in% rios) %>%
  filter(method %in% metodos) %>%
  filter(!locality_name %in% amostras_descartadas_vetor) %>%
  select(colunas) %>%
  pivot_longer(
    cols = c("total_macroinvertebrados", "riqueza_ordens", "riqueza_familias"),
    names_to = "categoria_da_resposta",
    values_to =  "valor"
  )

  # Melhorar estetica
  df <- df %>%
  mutate(
    locality_name = case_when(
      str_detect(locality_name, "P1") ~ "P1",
      str_detect(locality_name, "P2") ~ "P2",
      str_detect(locality_name, "P3") ~ "P3",
      str_detect(locality_name, "P1/2") ~ "P1+",
    ),
    rio = case_when(
      rio == "rio_bebedouro" ~ "Bebedouro",
      rio == "rio_da_barra" ~ "Barra",
      rio == "rio_mutuca" ~ "Mutuca"
    )
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


write_csv(sumario, "teste.csv")
grafico <- 
  ggplot(sumario, aes(x = locality_name, y = media, fill = method)) +
  
  geom_errorbar(
    aes(ymin = media - se, ymax = media + se),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_point(
    data = df,
    aes(y = valor, x = locality_name),
    position = position_dodge(width = 0.9),
    size = 1
  ) +
  facet_grid(categoria_da_resposta ~ rio, scales = "free_y") +
  labs(
    x = "Pontos de amostragem",
    y = "Valor do índice",
    fill = "Método de amostragem"
  ) +
  theme_bw() +
  theme(
    color = "black"
  ) +
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 12, face = "bold"),
    strip.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

ggsave(
  filename = "teste.png",
  plot = grafico,
  width = 12,
  height = 8,
  dpi = 300
)
