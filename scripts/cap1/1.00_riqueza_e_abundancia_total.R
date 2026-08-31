# Resumo da base de dados. 
# Quantos insetos foram coletados? 
# Quantos táxons? Em cada Classe, Em cada ordem
# Quais táxons mais e menos representativos? 

############################### Ambiente #######################################
library(tidyverse)
library(forcats)
# setwd("C:/Users/Giovana/Desktop/mestrado_giovana/")


# Pasta de tabelas
pasta_tabelas <- "tabelas/dados_descritivos/"
if (!dir.exists(pasta_tabelas)) {
  dir.create(pasta_tabelas, recursive = TRUE)
}

############################## Configurações ###################################
rios <- c("rio_bebedouro", "rio_da_barra", "rio_mutuca")
amostras_descartadas <- c("RiodaBarra-P1")
metodos <- c("D Net", "Pool Net", "Sieve")





############################## Processamento ###################################

df_cru <- read_csv("dados/processado/imaturos_piata_limpo.csv")

df <- df_cru %>%
  filter(rio %in% rios) %>%
  filter(!locality_name %in% amostras_descartadas) %>%
  filter(method %in% metodos)



# Geral -----------------------------
n_distinct(df$class)
unique(df$class)

n_distinct(df$order)
unique(df$order)

n_distinct(df$family)
unique(df$family)

# Classes --------------------------

sumario_classes <- df %>% 
  group_by(class) %>%
  summarise(
    abundancia_classe = sum(abundance),
    riqueza_ordens = n_distinct(order),
    riqueza_familias = n_distinct(family)
    ) %>%
  ungroup() %>%
  mutate(abundancia_total = sum(abundancia_classe)) %>%
  mutate(abundancia_relativa = abundancia_classe / abundancia_total)

write_csv(
  sumario_classes,
  file.path(pasta_tabelas, "sumario_classes.csv")
)

# abundancia por ordem ---------------------------------------------------------
sumario_ordens <- df %>% 
  group_by(order) %>%
  mutate(

    # Total de organismos coletados de cada ordem na bacia
    abundancia_ordem_total_bacia = sum(abundance),
  
    # total de familias coletadas de cada ordem na bacia
    riqueza_familias_por_ordem = n_distinct(family)
  ) %>%
  ungroup() %>%
  group_by(method) %>%
  mutate(
    abundancia_de_organismos_coletados_na_rede = sum(abundance),
    riqueza_ordens_por_rede = n_distinct(order)
  ) %>%
  ungroup() %>%
  group_by(method, order) %>%
  mutate(

    # Total de organismos coletados de cada ordem em cada rede
    abundancia_ordem_na_rede = sum(abundance),

    # Proporção de organimos coletados de cada ordem para o total daquela ordem
    # coletada na rede (Dominância da ordem na rede)
    abundancia_relativa_ordem_na_rede =
      (abundancia_ordem_na_rede
      / abundancia_de_organismos_coletados_na_rede) * 100,

    # Proporção de organismos coletados de cada ordem em cada rede para o total
    # daquela ordem coletada na bacia (Dominância da rede para a ordem)
    indice_de_captura_da_rede =
      (abundancia_ordem_na_rede / abundancia_ordem_total_bacia) * 100,

    riqueza_familias_da_ordem_por_rede = n_distinct(family)
  ) %>%
  select(
    method,
    order,
    family,
    abundancia_ordem_total_bacia,
    riqueza_familias_por_ordem,
    abundancia_de_organismos_coletados_na_rede,
    riqueza_ordens_por_rede,
    abundancia_ordem_na_rede,
    abundancia_relativa_ordem_na_rede,
    indice_de_captura_da_rede,
    riqueza_familias_da_ordem_por_rede
  ) 

write.csv(
  sumario_ordens,
  file.path(pasta_tabelas, "sumario_ordens_redes.csv")
)


# Arrumando grafico para plotagem ----------------------------------------------
sumario_ordens <- sumario_ordens %>%
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

# Remove as repetições
dominancia_ordens_redes <- sumario_ordens %>%
  group_by(method, order) %>%
  summarise(
    abundancia_relativa_ordem_na_rede = mean(abundancia_relativa_ordem_na_rede)
  )

# Ordena as ordens de acordo com a abundância relativa na rede
vetor_ordenamento <- dominancia_ordens_redes %>%
  group_by(order) %>%
  summarise(
    abundancia_relativa_ordem_na_rede = mean(
      abundancia_relativa_ordem_na_rede)
  ) %>%
  arrange(abundancia_relativa_ordem_na_rede) %>%
  pull(order)

dominancia_ordens_redes <- dominancia_ordens_redes %>%
  mutate(
    order = factor(order, levels = vetor_ordenamento)
  )

# Plot -------------------------------------------------------------------------
grafico_ordens <-
  ggplot(
    dominancia_ordens_redes,
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
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      face = "italic"
    ),
    legend.position = "right"
  )

dev.off()

############################ abundancia por familia ############################
sumario_familia <- df %>% 
  group_by(family) %>%
  mutate(

    # Total de organismos coletados de cada ordem na bacia
    abundancia_familia_total_bacia = sum(abundance),
  ) %>%
  ungroup() %>%
  group_by(method) %>%
  mutate(
    abundancia_de_organismos_coletados_na_rede = sum(abundance),
    riqueza_familias_na_rede = n_distinct(family)
  ) %>%
  ungroup() %>%
  group_by(method, family) %>%
  mutate(

    # Total de organismos coletados de cada familia em cada rede
    abundancia_familia_na_rede = sum(abundance),

    # Proporção de organimos coletados de cada ordem para o total daquela ordem
    # coletada na rede (Dominância da ordem na rede)
    abundancia_relativa_familia_na_rede =
      (abundancia_familia_na_rede
      / abundancia_de_organismos_coletados_na_rede) * 100,

    # Proporção de organismos coletados de cada ordem em cada rede para o total
    # daquela ordem coletada na bacia (Dominância da rede para a familia)
    indice_de_captura_da_rede =
      (abundancia_familia_na_rede / abundancia_familia_total_bacia) * 100

    
  ) %>%
  select(
    method,
    family,
    abundancia_familia_total_bacia,
    riqueza_familias_na_rede,
    abundancia_de_organismos_coletados_na_rede,
    abundancia_familia_na_rede,
    abundancia_relativa_familia_na_rede,
    indice_de_captura_da_rede
  ) 





write.csv(
  sumario_familia,
  file.path(pasta_tabelas, "sumario_familias_redes.csv")
)

# Arrumando grafico para plotagem ----------------------------------------------

sumario_familia <- sumario_familia %>%
  mutate(method = recode(method,
                    "Sieve" = "Peneira",
                    "Pool Net" = "Rede de Piscina",
                    "D Net" = "Rede D")
  )

dominancia_familias_redes <- sumario_familia %>%
  group_by(method, family) %>%
  summarise(
    abundancia_relativa_familia_na_rede = mean(abundancia_relativa_familia_na_rede)
  )

vetor_ordenamento_familias <- dominancia_familias_redes %>%
  group_by(family) %>%
  summarise(
    abundancia_relativa_familia_na_rede = mean(
      abundancia_relativa_familia_na_rede)
  ) %>%
  arrange(abundancia_relativa_familia_na_rede) %>%
  pull(family)

dominancia_familias_redes <- dominancia_familias_redes %>%
  mutate(
    family = factor(family, levels = vetor_ordenamento_familias)
  )



grafico_familia <-
  ggplot(
    dominancia_familias_redes,
    aes(
      x = family,
      y = abundancia_relativa_familia_na_rede,
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
    x = "Familias",
    y = "Abundância relativa (%)",
    fill = "Método de coleta"
  ) +

  theme_classic(base_size = 14) +

  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      face = "italic"
    ),
    legend.position = "right"
  )

dev.off()
write_csv(
  sumario_familia,
  file.path(pasta_tabelas, "sumario_familias_redes.csv")
)
