library(tidyverse)
library(vegan)

setwd("C:/Users/Giovana/Desktop/mestrado_giovana/")


################################### Carregar Dados #############################
df_total <- 
  read_csv("dados/processado/imaturos_piata_limpo.csv")
df_classes <- 
  read_csv("dados/processado/matriz_comunidades_classes_imaturos_largo.csv")
df_ordens <- 
  read_csv("dados/processado/matriz_comunidades_ordens_imaturos_largo.csv")
df_familias <-
  read_csv("dados/processado/matriz_comunidades_familias_imaturos_largo.csv")

################################### Processamento ##############################
matriz_total <- data.frame(
  campanha = df_classes$campanha,
  rio = df_classes$rio,
  locality_name = df_classes$locality_name,
  method = df_classes$method
)

# Riqueza ----------------------------------------------------------------------
riqueza_classes <- df_total %>%
  group_by(rio, campanha, locality_name, method) %>%
  summarise(riqueza_classes = n_distinct(class))

riqueza_ordens <- df_total %>%
  group_by(rio, campanha, locality_name, method) %>%
  summarise(riqueza_ordens = n_distinct(order))

riqueza_familias <- df_total %>%
  group_by(rio, campanha, locality_name, method) %>%
  summarise(riqueza_familias = n_distinct(family))

matriz_total <- merge(matriz_total, riqueza_classes)
matriz_total <- merge(matriz_total, riqueza_ordens)
matriz_total <- merge(matriz_total, riqueza_familias)

rm(riqueza_classes, riqueza_ordens, riqueza_familias, riqueza_df_parcial)

# Diversidade Shanon -----------------------------------------------------------

classes_numericos <- df_classes[, 5:ncol(df_classes)]
ordens_numericos <- df_ordens[, 5:ncol(df_ordens)]
familias_numericos <- df_familias[, 5:ncol(df_familias)]

matriz_total$shannon_classes <- 
  diversity(classes_numericos, index = "shannon")
matriz_total$shannon_ordens <- 
  diversity(ordens_numericos, index = "shannon")
matriz_total$shannon_familias <- 
  diversity(familias_numericos, index = "shannon")

rm(classes_numericos, ordens_numericos, familias_numericos)

# ---------------------------- Pielou's Evenness -------------------------------

# Cálculo da equabilidade de Pielou (J = H / ln(S))
matriz_total$pielou_classes <- 
  matriz_total$shannon_classes / log(matriz_total$riqueza_classes)
matriz_total$pielou_ordens <- 
  matriz_total$shannon_ordens / log(matriz_total$riqueza_ordens)
matriz_total$pielou_familias <- 
  matriz_total$shannon_familias / log(matriz_total$riqueza_familias)

# Substituir valores infinitos ou NaN por NA (casos com riqueza = 0 ou 1)
matriz_total <- matriz_total %>%
  mutate(
    across(
      starts_with("pielou_"), 
      ~ ifelse(is.infinite(.x) | is.nan(.x), NA, .x)
    )
  )

#------------------------------ Riqueza de taxa in taxa ------------------------
ephemeroptera_riqueza <- df_total %>%
  filter(order == "Ephemeroptera") %>%
  group_by(rio, campanha, locality_name, method) %>%
  summarise(riqueza_familias_ephemeroptera = n_distinct(family))

trichoptera_riqueza <- df_total %>%
  filter(order == "Trichoptera") %>%
  group_by(rio, campanha, locality_name, method) %>%
  summarise(riqueza_familias_trichoptera = n_distinct(family))

plecoptera_riqueza <- df_total %>%
  filter(order == "Plecoptera") %>%
  group_by(rio, campanha, locality_name, method) %>%
  summarise(riqueza_familias_plecoptera = n_distinct(family)) 

coleoptera_riqueza <- df_total %>%
  filter(order == "Coleoptera") %>%
  group_by(rio, campanha, locality_name, method) %>%
  summarise(riqueza_familias_coleoptera = n_distinct(family)) 

matriz_total <- left_join(matriz_total, ephemeroptera_riqueza)
matriz_total <- left_join(matriz_total, trichoptera_riqueza)
matriz_total <- left_join(matriz_total, plecoptera_riqueza)
matriz_total <- left_join(matriz_total, coleoptera_riqueza)

# remover Na's
matriz_total <- matriz_total %>%
  mutate(
    riqueza_familias_ephemeroptera = replace_na(riqueza_familias_ephemeroptera, 0),
    riqueza_familias_trichoptera = replace_na(riqueza_familias_trichoptera, 0),
    riqueza_familias_plecoptera = replace_na(riqueza_familias_plecoptera, 0),
    riqueza_familias_coleoptera = replace_na(riqueza_familias_coleoptera, 0)
  )

# EPT
matriz_total <- matriz_total %>%
  mutate(
    ept_riqueza = riqueza_familias_ephemeroptera
         + riqueza_familias_trichoptera
         +riqueza_familias_plecoptera
  )

rm(
  ephemeroptera_riqueza, trichoptera_riqueza, 
  plecoptera_riqueza, coleoptera_riqueza
)


# 1. Calcular metricas
#   1.01. Riqueza
#       Quantidade de táxons diferentes
#
#   1.02. Equitabilidade
#       Pielou’s Eveness
#	      (J) mede o quanto as espécies estão distribuídas de forma equilibrada 
#	      J = 1 todas as espécies têm o mesmo número de indivíduos 
#       (comunidade é totalmente equilibrada).
#	      J =  0 indica que algumas espécies dominam, enquanto outras são raras
#
#   1.03. Dominância
#       Simpson D (medida de diversidade)
#	      Leva em consideração o número de espécies presentes, bem como a 
#       abundância relativa de cada espécie. 
#	      À medida que a riqueza e a equitatividade das espécies aumentam, 
#       a diversidade também aumenta.
#     	Valores próximos de 1 = alta dominância (baixa diversidade),
#       Valores próximos de 0 = baixa dominância
#
#   1.04. Diversidade alfa 
#       Diversidade de um local específico
#
#   1.05. Diversidade Beta
#       É a diferença de espécies entre diferentes locais
#
#   1.06 Shannon Wiener diversity
#       Diversidade de espécies = H
#	      H > maior a diversidade de espécies 
#       H <  menor a diversidade
#	      H = 0 indica uma comunidade que possui apenas uma espécie
#   
#   1.07. Ephemeroptera taxa
#       Quantidade de táxons diferentes dentro da ordem ephemeroptera
#
#   1.08. Plecoptera taxa 
#       Quantidade de táxons diferentes dentro da ordem Plecoptera
#        NAO UTILIZADO
#
#   1.09. Trichoptera taxa
#       Quantidade de táxons diferentes dentro da ordem
#
#   1.10 Coleoptera taxa
#     Quantidade de táxons diferentes dentro da ordem
#
#   1.11. EPT taxa
#     Quantidade de táxons diferentes dentro da ordem
#
#   1.12. BMWP'
#     Atribui diferentes pontuações a famílias de macroinvertebrados de acordo 
#     com seu grau de tolerância ou sensibilidade a poluição.
#
#   1.13. ASPT
#     Média do BMWP'
#
#   1.14. EPT/Chironomidae
#     Razão EPT / Chironomidade
#
#   1.15. Baetidae/Ephemeroptera
#     Razão Baetidae / Ephemeroptera
#
#   2.6. Diversidade  Shannon Winner
#   2.7. % Chironomidae
#         n total de chironomidae/n total de macroinvertebrados
#   2.8. %Oligochaeta
#         n total de Oligochaeta/n total de macroinvertebrad
#   2.9. CHOL
#         n total de Chironomidae e Oligochaeta/ n total de macroinvertebra
#   3.0. %Ephemeroptera
#         n total de Ephemeroptera/n total de macroinvertebrad
#   3.1. %Plecoptera
#            n total de Plecoptera/n total de macroinvertebrad
#   3.2. %Trichoptera
#            n total de Trichoptera/n total de macroinvertebrad
#   3.3. %Coleoptera
#            n total de Coleoptera/n total de macroinvertebrad
#   3.4. %Diptera
#            n total de Diptera/n total de macroinvertebrad
#   3.5. %Odonata
#            n total de Odonata/n total de macroinvertebrad
#   3.6. %Gastropoda
#            n total de Gastropoda / n total de macroinvertebrad
#   3.7 %Crustacea
#            n total de Crustacea / n total de macroinvertebrad
#   3.8 %Colletor-filtrador
#        n total de %Coletor-filtrador/n total de macroinvertebrad
#   3.9 %Coletor-gatheres
#         n total de Coletor-gatheres/n total de macroinvertebrad
#   4.0 %Shredders
#      n total de Shredders/n total de macroinvertebrad
#   4.1 %Scrapers
#      n total de Scrapers/n total de macroinvertebrad
#   4.2 %Predadores
#      n total de Predadores/n total de macroinvertebrad




