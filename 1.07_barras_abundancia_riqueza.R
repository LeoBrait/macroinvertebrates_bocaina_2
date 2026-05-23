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


################################## funcoes #####################################

desenhar_barras <- function(df, metodo) {
  sumario <- df %>%
    filter(method == metodo) %>%
    group_by(rio, locality_name, categoria_da_resposta) %>%
    summarise(
      media = mean(valor),
      desvio_padrao = sd(valor),
      n = n(),
      se =  desvio_padrao / sqrt(n)
    ) %>%
    ungroup()

  plot <- 
    ggplot(
      sumario,
      aes(y = media, x = locality_name, fill = categoria_da_resposta)
    ) +
    theme_minimal() +
    geom_errorbar(
      aes(
        ymin = 1,
        ymax = media + se
      ),
      width = 0.2,
      position = position_dodge(width = 0.9)
    ) +
    geom_bar(
      stat = "identity",
      position = position_dodge(width = 0.9)
    ) +
    geom_point(
      data = df %>% filter(method == metodo), 
      aes(y = valor, x = locality_name),
      position = position_dodge(
        width = 0.9
      )
    ) +
    facet_wrap(~ rio, scales = "free_x", ncol = 1) +
    labs(x = "Localidade", y = "Valor", fill = "Categoria da Resposta") +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0, size = 15),
      axis.title.x = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(size = 12, face = "bold")
    ) +
    scale_y_continuous( limits = c(0, 25)) 

    return(plot)
}


#carregar dados
df <- read_csv("dados/processado/matriz_indices_imaturos_piata.csv") %>%
  filter(rio %in% rios) %>%
  filter(method %in% metodos) %>%
  filter(!locality_name %in% amostras_descartadas_vetor) %>%
  mutate(total_macroinvertebrados = total_macroinvertebrados * 0.1) %>%
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

plot_rede_d <- desenhar_barras(df, "D Net")
plot_piscina <- desenhar_barras(df, "Pool Net")
plot_peneira <- desenhar_barras(df, "Sieve")

painel <- ggarrange(
    plot_rede_d,
    plot_piscina,
    plot_peneira,
    common.legend = TRUE,
    legend = "bottom",
    ncol = 3,
    labels = c("REDE D", "PISCINA", "PENEIRA"),
    font.label = list(size = 16, face = "bold")
)

ggsave(
    "barras.svg",
    painel,
    width = 16,
    height = 12,
    dpi = 300
)

