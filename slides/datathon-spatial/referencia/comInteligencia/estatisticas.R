## Pacotes ----
require(data.table)
require(dplyr)
require(purrr)
require(stringr)
## Lendo as bases ----
sinan <- readRDS(file = "~/Documentos/S4G/SINAN/R/sinan.RDS")
load(file = "~/Documentos/S4G/IBGE/dados/ibge.RData")
## Estatísticas ----
## + Filtrando 2016 ----
sinan <- sinan %>%
  filter(ANO == 2016, SEX_ESTUPR == "Sim")

ibge <- ibge %>%
  filter(ANO == 2016)
## + Crimes por sexo ----
table(sinan$CS_SEXO)
prop.table(table(sinan$CS_SEXO))
## + Filtrando o sexo feminino ----
sinan <- sinan %>%
  filter(CS_SEXO == "Feminino")

ibge <- ibge %>%
  filter(SEXO == "Feminino")
## + Municípios com alguma notificação e migração ----
ibge$ID_MUNICIP <- str_sub(ibge$COD_MUN, start = 1, end = 6)
sinan_mun <- sinan %>% select(ID_MUNICIP, ID_MN_OCOR, ID_MN_RESI)

mun_notific <- merge(sinan_mun, ibge, by = "ID_MUNICIP", all.x = T)
mun_notific <- mun_notific %>% filter(!is.na(POP))
uniqueN(mun_notific$ID_MUNICIP)

table(mun_notific$ID_MN_RESI == mun_notific$ID_MUNICIP)
## + Proporção por raça ----
prop.table(table(sinan$CS_RACA))

## + Proporção por escolaridade ----
sort(prop.table(table(sinan$CS_ESCOL_N)))

## + Proporção por escolaridade ----
sort(prop.table(table(sinan$CS_ESCOL_N)))

## + Proporção por idade ----
faixas_etarias <- c(0, 9, 14, 19, 29, 59, 118)

sinan <- sinan %>%
  mutate(IDADE_DIAS = as.numeric(DT_OCOR - DT_NASC),
         IDADE = IDADE_DIAS/365,
         IDADE_CAT = cut(x = IDADE, breaks = faixas_etarias, include.lowest = T))

sort(prop.table(table(sinan$IDADE_CAT)), decreasing = T)

## + Parentesco ----

prop.table(table(sinan$REL_PAI))[3] +
prop.table(table(sinan$REL_MAE))[3] +
prop.table(table(sinan$REL_PAD))[3] +
prop.table(table(sinan$REL_FILHO))[3] +
prop.table(table(sinan$REL_IRMAO))[3] +
prop.table(table(sinan$REL_MAD))[3] +
prop.table(table(sinan$REL_CONJ))[3] +
prop.table(table(sinan$REL_NAMO))[3] 
