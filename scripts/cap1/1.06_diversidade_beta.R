# Missoes
# 1) Transformar a abundancia das familias em 0 e 1
# 2) familias sao as colunas e localidades sao as linhas
# 3) Como eu tenho dados de diferentes campanhas preciso colocar so como presente
## ou ausente


#### Ativar  Pacotes ########
library(tidyverse)
library(betapart)
library(wesanderson)
library(reshape2)


# Carregar caminho
#setwd("C:/Users/Giovana/Desktop/mestrado_giovana/")
rios <- c("rio_bebedouro", "rio_da_barra", "rio_mutuca")
# Carregar dados
df_cru <- read.csv(file = "dados/processado/matriz_comunidades_familias_imaturos_largo.csv", sep = ",") %>%
  filter(rio %in% rios)



######################## Carregar e Limpar Dados ###############################
rios <- c("rio_bebedouro", "rio_da_barra", "rio_mutuca")
amostras_descartadas <- c("RiodaBarra-P1")
metodos <- c("D Net", "Pool Net", "Sieve")


dados_df <- df_cru %>%
  filter(rio %in% rios) %>%
  filter(!locality_name %in% amostras_descartadas) %>%
  filter(method %in% metodos) %>%
  group_by(campanha, rio, locality_name) %>%
  filter(all(metodos %in% method)) %>%
  ungroup()  

### Pre processamento###########################################################
processado <- dados_df %>%
  group_by(locality_name) %>%
  summarise(
    across(
      where(is.numeric),
      ~ as.integer(sum(.x, na.rm = TRUE) > 0)
    ),
    .groups = "drop"
  )

dados_mat <- processado %>%
  column_to_rownames("locality_name")#Transformar a coluna em rownames

dim(dados_mat) 
 

# Calculo de diversidade total

beta_total <- beta.multi(dados_mat,
                         index.family = "sorensen") 
beta_total #Print


# calcula a diversidade beta par-a-par entre todos os locais amostrados



beta_par <- beta.pair(dados_mat,
                      index.family = "sorensen")


# Organizacao da matriz
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# Extrai a matriz de turnover (βSIM) da sua análise de diversidade beta par-a-par,

sim <- get_upper_tri(as.matrix(beta_par$beta.sim))

# Extrai a matriz de a dissimilaridade total (Sørensen) 
sor <- get_upper_tri(as.matrix(beta_par$beta.sor))

# Extrai a matriz de o nestedness (aninhamento).
sne <- get_upper_tri(as.matrix(beta_par$beta.sne))

# Essas três linhas transformam as matrizes de dissimilaridade (turnover, total 
##e nestedness) em data frames no formato longo (long format)
melted_cormat <- reshape2::melt(sim)
melted_cormat2 <- reshape2::melt(sor)
melted_cormat3 <- reshape2::melt(sne)

## empilha (concatena verticalmente) os três data frames
melted_cormat <- rbind(melted_cormat2, melted_cormat, melted_cormat3)


# adiciona uma coluna (metric) ao data frame melted_cormat, identificando qual 
# componente da diversidade beta cada linha representa
melted_cormat$metric <- factor(rep(c("Total", "Turnover", "Aninhamento"), 
                                   each = nrow(melted_cormat2)), 
                               c("Total", "Turnover", "Aninhamento"))

# Cria uma paleta de cores para o grafico
pal <- wes_palette("Zissou1", 100, type = "continuous")

# grafico
g <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradientn(colours = pal, 
                       name="Diversidade Beta") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0),
        text = element_text(size = 16)) +
  coord_fixed() +
  xlab("") +
  ylab("") +
  facet_wrap(metric ~.) +
  ggtitle("Componentes da beta diversidade entre localidades")
g

ggsave(
  "figuras/cap1/beta_diversidade/beta_diversidade_familias_total.png",
  g,
  width = 10,
  height = 8
)












