library(tidyverse)
library(vegan)
#setwd("C:/Users/Giovana/Desktop/mestrado_giovana/")
# Para os casos de razoes por zero, colocamos o valor do numerador


################################### Carregar Dados #############################

# Comunidades
df_total <-
  read_csv("dados/processado/imaturos_piata_limpo.csv")
df_classes <-
  read_csv("dados/processado/matriz_comunidades_classes_imaturos_largo.csv")
df_ordens <-
  read_csv("dados/processado/matriz_comunidades_ordens_imaturos_largo.csv")
df_familias <-
  read_csv("dados/processado/matriz_comunidades_familias_imaturos_largo.csv")

# Bmwp
bmwp_dicionario_df <-
  read_csv("dados/20250406_referencia_bmwp.csv")

# Metadado
metadado_df <- df_classes %>%
  select(c(campanha, rio, locality_name, method))


################################### Processamento ##############################
matriz_total <- data.frame(
  campanha = df_classes$campanha,
  rio = df_classes$rio,
  locality_name = df_classes$locality_name,
  method = df_classes$method
)

# ----------------------------------- Geral ------------------------------------
# Riqueza ------------------------------
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

# Diversidade Shanon -------------------

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

# Pielou's Evenness --------------------

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

# --------------------------- Riqueza Taxa in Taxa -----------------------------
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
         + riqueza_familias_plecoptera
  )

rm(
  ephemeroptera_riqueza, trichoptera_riqueza,
  plecoptera_riqueza, coleoptera_riqueza
)

# ------------------ Abundancia Relativa e Razões ------------------------------
total_macroinvertebrados_vetor <- df_familias %>%
  select(-c(rio, campanha, locality_name, method)) %>%
  rowSums()

relativo_macroinvertebrados_classes <- df_classes %>%
   mutate(total_macroinvertebrados = total_macroinvertebrados_vetor) %>%
   mutate(
    relativo_insecta = (Insecta / total_macroinvertebrados),
    relativo_oligochaeta = (Oligochaeta / total_macroinvertebrados),
    relativo_gastropoda = (Gastropoda / total_macroinvertebrados),
    relativo_crustacea = (Crustacea / total_macroinvertebrados)
   ) %>%
   select(c(colnames(metadado_df), starts_with("relativo_")))

relativo_macroinvertebrados_ordens <- df_ordens %>%
   mutate(total_macroinvertebrados = total_macroinvertebrados_vetor) %>%
   mutate(
    relativo_ephemeroptera = (Ephemeroptera / total_macroinvertebrados),
    relativo_plecoptera = (Plecoptera / total_macroinvertebrados),
    relativo_trichoptera = (Trichoptera / total_macroinvertebrados),
    relativo_coleoptera = (Coleoptera / total_macroinvertebrados),
    relativo_diptera = (Diptera / total_macroinvertebrados),
    relativo_odonata = (Odonata / total_macroinvertebrados)
   ) %>%
    select(c(colnames(metadado_df), starts_with("relativo_")))

relativo_macroinvertebrados_familias <- df_familias %>%
  mutate(total_macroinvertebrados = total_macroinvertebrados_vetor) %>%
  mutate(
    relativo_chironomidae = (Chironomidae / total_macroinvertebrados),
  ) %>%
  select(c(colnames(metadado_df), relativo_chironomidae))

relativo_chol <- metadado_df %>%
  mutate(
    oligochaeta = df_classes$Oligochaeta,
    chironomidae = df_familias$Chironomidae
  ) %>%
  mutate(
    relativo_chol =
    (oligochaeta + chironomidae) / total_macroinvertebrados_vetor
  ) %>%
  select(c(colnames(metadado_df), relativo_chol))

razao_ept_chironomidae <- metadado_df %>%
  mutate(
    ept_abundancia =
      df_ordens$Ephemeroptera +
      df_ordens$Plecoptera +
      df_ordens$Trichoptera,
    chironomidae_abundancia = df_familias$Chironomidae
  ) %>%
  mutate(
    razao_ept_chironomidae =
      ifelse(
        chironomidae_abundancia == 0,
        ept_abundancia,
        ept_abundancia / chironomidae_abundancia
      )
  ) %>%
  select(c(colnames(metadado_df), razao_ept_chironomidae))

razao_baetidae_ephemeroptera <- metadado_df %>%
  mutate(
    baetidae_abundancia = df_familias$Baetidae,
    ephemeroptera_abundancia = df_ordens$Ephemeroptera
  ) %>%
  mutate(
    razao_baetidae_ephemeroptera =
      ifelse(
        ephemeroptera_abundancia == 0,
        baetidae_abundancia,
        baetidae_abundancia / ephemeroptera_abundancia
      )
  ) %>%
  select(c(colnames(metadado_df), razao_baetidae_ephemeroptera))


matriz_total <- merge(relativo_macroinvertebrados_classes, matriz_total)
matriz_total <- merge(relativo_macroinvertebrados_ordens, matriz_total)
matriz_total <- merge(relativo_macroinvertebrados_familias, matriz_total)
matriz_total <- merge(relativo_chol, matriz_total)
matriz_total <- merge(razao_ept_chironomidae, matriz_total)
matriz_total <- merge(razao_baetidae_ephemeroptera, matriz_total)

rm(
  relativo_macroinvertebrados_classes,
  relativo_macroinvertebrados_ordens,
  relativo_macroinvertebrados_familias,
  relativo_chol,
  total_macroinvertebrados_vetor,
  razao_ept_chironomidae,
  razao_baetidae_ephemeroptera
)


# ------------------------ Indices de Qualidade -------------------------------
bmwp_df <- df_total %>%
  left_join(bmwp_dicionario_df, by = "family") %>% 
  mutate(bmwp_score = ifelse(is.na(bmwp_score), 0, bmwp_score)) %>%
  group_by(rio, campanha, locality_name, method) %>%
  summarise(bmwp = sum(bmwp_score))

matriz_total <- merge(matriz_total, bmwp_df)
matriz_total <- matriz_total %>%
  mutate(
    aspt = bmwp / riqueza_familias
  )

rm(bmwp_df)

#################################### Gerais ####################################
#
#   1.01.  [ok]
#       Quantidade de táxons diferentes
#
#   1.02. Equitabilidade de Pielou  [ok]
#       Pielou’s Eveness
#	      (J) mede o quanto as espécies estão distribuídas de forma equilibrada 
#	      J = 1 todas as espécies têm o mesmo número de indivíduos 
#       (comunidade é totalmente equilibrada).
#	      J =  0 indica que algumas espécies dominam, enquanto outras são raras
#
#   1.03. Dominância [ok]
#       Simpson D (medida de diversidade)
#	      Leva em consideração o número de espécies presentes, bem como a 
#       abundância relativa de cada espécie. 
#	      À medida que a riqueza e a equitatividade das espécies aumentam, 
#       a diversidade também aumenta.
#     	Valores próximos de 1 = alta dominância (baixa diversidade),
#       Valores próximos de 0 = baixa dominância
#
#   1.04. Shannon Wiener  [ok]
#       Diversidade de espécies = H
#	      H > maior a diversidade de espécies 
#       H <  menor a diversidade
#	      H = 0 indica uma comunidade que possui apenas uma espécie
#
############################## Taxa in Taxa Richness ###########################
#
#   2.01. Ephemeroptera taxa richness [ok]
#       Quantidade de táxons diferentes dentro da ordem ephemeroptera
#
#   2.02. Plecoptera taxa richness [ok]
#       Quantidade de táxons diferentes dentro da ordem Plecoptera
#        NAO UTILIZADO
#
#   2.03. Trichoptera  taxa richness [ok]
#       Quantidade de táxons diferentes dentro da ordem
#
#   2.04. Coleoptera taxa richness [ok]
#     Quantidade de táxons diferentes dentro da ordem
#
#   2.05. EPT taxa richness [ok]
#     Quantidade de táxons diferentes dentro da ordem EPT
#       (Ephemeroptera, Plecoptera e Trichoptera)
#
############################## Abundancia relativa #############################
#
#   3.01 % Chironomidae
#         n total de chironomidae/n total de macroinvertebrados
#   3.02 %Oligochaeta
#         n total de Oligochaeta/n total de macroinvertebrad
#   3.03 %CHOL
#         n total de Chironomidae e Oligochaeta/ n total de macroinvertebra
#   3.04 %Ephemeroptera
#         n total de Ephemeroptera/n total de macroinvertebrad
#   3.05 %Plecoptera
#            n total de Plecoptera/n total de macroinvertebrad
#   3.06 %Trichoptera
#            n total de Trichoptera/n total de macroinvertebrad
#   3.07 %Coleoptera
#            n total de Coleoptera/n total de macroinvertebrad
#   3.08 %Diptera
#            n total de Diptera/n total de macroinvertebrad
#   3.09 %Odonata
#            n total de Odonata/n total de macroinvertebrad
#   3.10 %Gastropoda
#            n total de Gastropoda / n total de macroinvertebrad
#   3.11 %Crustacea
#         n total de Crustacea / n total de macroinvertebrad
#
############################## Indices de Qualidade ############################
#
#   4.01 BMWP'
#     Atribui diferentes pontuações a famílias de macroinvertebrados de acordo 
#     com seu grau de tolerância ou sensibilidade a poluição.
#
#   4.02 ASPT
#     Média do BMWP'
#
#   4.03 EPT/Chironomidae
#     Razão EPT / Chironomidade
#
#   4.04 Baetidae/Ephemeroptera
#     Razão Baetidae / Ephemeroptera
#
#
############################### Grupos Funcionais ##############################
#
#   5.01 %Colletor-filtrador
#        n total de %Coletor-filtrador/n total de macroinvertebrad
#   5.02 %Coletor-gatheres
#         n total de Coletor-gatheres/n total de macroinvertebrad
#   5.03 %Shredders
#      n total de Shredders/n total de macroinvertebrad
#   5.04 %Scrapers
#      n total de Scrapers/n total de macroinvertebrad
#   5.05 %Predadores
#      n total de Predadores/n total de macroinvertebrad





