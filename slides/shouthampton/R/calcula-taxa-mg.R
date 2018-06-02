dados <- readRDS("../data/dados-iam.rds")
fx_etaria <- unique(dados$FAIXA_ETARIA)
fx_etaria <- c(fx_etaria[1], fx_etaria[7], fx_etaria[-c(1, 7)])

dados <- dados %>%
  mutate(DATA = as.Date(paste(ANO, MES, "01", sep = "-")),
         FAIXA_ETARIA = factor(FAIXA_ETARIA, levels = fx_etaria))

pop_mg_2010 <- dados %>%
  filter(ANO == 2010 & MES == 1) %>%
  group_by(FAIXA_ETARIA) %>%
  summarise(POP_PADRAO = sum(POP))

pop_aih_2010 <- dados %>%
  filter(ANO == 2010) %>%
  group_by(FAIXA_ETARIA) %>%
  summarise(POP_PADRAO_AIH = sum(N_INTER))

pop_mg_2010 <- merge(pop_mg_2010, pop_aih_2010, all.x = T)
dados  <- merge(dados, pop_mg_2010)

# saveRDS(dados, "../data/dados-iam.rds")
consorcio_df <- dados %>%
  select(CONSORCIO, DATA_IMPLEMENTACAO) %>%
  distinct()

saveRDS(consorcio_df, "data/consorcio.rds")

mg <- dados %>%
  group_by(DATA, FAIXA_ETARIA) %>%
  summarise(N_OBITO_SIM = sum(N_OBITO_SIM), 
            N_OBITO_SIH = sum(N_OBITO),
            N_INTER = sum(N_INTER),
            POP = sum(POP)) %>%
  left_join(pop_mg_2010) %>%
  mutate(TX_MORTALIDADE = N_OBITO_SIM/POP, 
         E_OBITO = TX_MORTALIDADE * POP_PADRAO,
         TX_INTERNACAO = N_INTER/POP,
         E_INTER = TX_INTERNACAO * POP_PADRAO_AIH,
         TX_LETALIDADE = N_OBITO_SIH/N_INTER,
         E_LETALIDADE = TX_LETALIDADE * POP_PADRAO_AIH) %>%
  group_by(DATA) %>%
  summarise(TAXA_MORTALIDADE = sum(E_OBITO)/sum(POP_PADRAO),
            TAXA_INTERNACAO =  sum(E_INTER)/sum(POP_PADRAO_AIH),
            TAXA_LETALIDADE =  sum(E_LETALIDADE, na.rm = T)/sum(POP_PADRAO_AIH))

saveRDS(mg, "data/mg_rate.rds")
