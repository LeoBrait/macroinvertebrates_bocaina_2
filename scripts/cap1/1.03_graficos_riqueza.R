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

# Pasta de figuras
pasta_figuras <- "figuras/cap1/dados_descritivos/"
if (!dir.exists(pasta_figuras)) {
  dir.create(pasta_figuras, recursive = TRUE)
}


#carregar dados
df <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv") %>%
  filter(rio %in% rios) %>%
  filter(method %in% metodos) %>%
  filter(!locality_name %in% amostras_descartadas_vetor) %>%
  group_by(campanha, rio, locality_name) %>%
  filter(all(metodos %in% method)) %>%
  ungroup() %>%
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
      rio == "rio_bebedouro" ~ "Rio Bebedouro",
      rio == "rio_da_barra" ~ "Rio da Barra",
      rio == "rio_mutuca" ~ "Rio Mutuca"
    )
  ) %>%
  mutate(
    categoria_da_resposta = case_when(
      categoria_da_resposta == "total_macroinvertebrados" ~ "Abundância",
      categoria_da_resposta == "riqueza_ordens" ~ "Riqueza de ordens",
      categoria_da_resposta == "riqueza_familias" ~ "Riqueza de famílias"
    ),
    campanha = case_when(
      campanha == "campanha_1" ~ "Campanha 1",
      campanha == "campanha_2" ~ "Campanha 2",
      campanha == "campanha_3" ~ "Campanha 3",
      campanha == "campanha_4" ~ "Campanha 4"
    )
  ) %>%
  mutate(
    method = case_when(
      method == "D Net" ~ "Rede D",
      method == "Pool Net" ~ "Rede de Piscina",
      method == "Sieve" ~ "Peneira"
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
    aes(
      y = valor, x = locality_name, 
      shape = as.factor(campanha), group = method
    ),
    position = position_dodge(width = 0.9),
    size = 2
  ) +
  facet_grid(categoria_da_resposta ~ rio, scales = "free_y") +
  labs(
    x = "Pontos de amostragem",
    y = "Valor do índice",
    fill = "Método de amostragem",
    shape = "Campanha"
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
  ) +
  scale_shape_manual(
    values = c(1, 0, 2, 3),
    labels = c("Campanha 1", "Campanha 2", "Campanha 3", "Campanha 4"),
    name = "Campanha"
  )

ggsave(
  filename = paste0(pasta_figuras, "painel_riqueza_abundancia.png"),
  plot = grafico,
  width = 12,
  height = 8,
  dpi = 300
)
