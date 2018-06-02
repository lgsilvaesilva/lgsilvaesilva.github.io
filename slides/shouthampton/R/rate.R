dados <- readRDS("../data/dados-iam.rds")
dados <- dados %>%
  mutate(TX_BRUTA_SIM = N_OBITO_SIM/POP,                      ## Taxa Bruta SIM faixa etaria (óbitos SIM)
         TX_BRUTA_SIH = N_OBITO/POP,                          ## Taxa Bruta SIH faixa etaria (óbitos hospitalares)
         TX_BRUTA_INTERNACAO = N_INTER/POP,                   ## Taxa Bruta INTERNACAO faixa etaria
         LETALIDADE_BRUTA_AIH = N_OBITO/N_INTER) %>%          ## Taxa de Letalidade Bruta SIH faixa etaria
  group_by(COD_MUN, ANO, MES) %>%
  summarise(MUN_RESID = unique(MUN_RESID),                           ## Codigo IBGE do municipio
            CONSORCIO = unique(CONSORCIO),                           ## Nome do consorcio do SAMU
            DATA_IMPLEMENTACAO = unique(DATA_IMPLEMENTACAO),         ## Data da implementação do SAMU
            N_INTER = sum(N_INTER),                                  ## Número de internações
            N_OBITO = sum(N_OBITO),                                  ## Número de óbitos hospitalares
            N_OBITO_SIM = sum(N_OBITO_SIM),                          ## Número de óbitos SIM
            E_AGE_SIM   = sum(POP_PADRAO*TX_BRUTA_SIM),              ## Óbitos esperados SIM padronizado
            E_AGE_SIH   = sum(POP_PADRAO*TX_BRUTA_SIH),              ## Óbitos esperados SIH padronizado
            E_AGE_INTER = sum(POP_PADRAO*TX_BRUTA_INTERNACAO),       ## Óbitos esperados SIM padronizado
            TX_PADRONIZADA_SIM = E_AGE_SIM/sum(POP_PADRAO),          ## Taxa de mortalidade SIM padronizada
            TX_PADRONIZADA_SIH = E_AGE_SIH/sum(POP_PADRAO),          ## Taxa de mortalidade SIH padronizada
            TX_PADRONIZADA_INTERNACAO = E_AGE_INTER/sum(POP_PADRAO), ## Taxa de internacoes SIH padronizada
            TX_BRUTA_SIM = N_OBITO_SIM/sum(POP),                     ## Taxa de mortalidade SIM bruta
            TX_BRUTA_SIH = N_OBITO/sum(POP),                         ## Taxa de mortalidade SIH bruta
            TX_BRUTA_INTERNACAO = N_INTER/sum(POP),                  ## Taxa bruta internação
            E_AGE_AIH = sum(POP_PADRAO_AIH*LETALIDADE_BRUTA_AIH, na.rm = T), ## Obitos/Letalidade esperada padronizada
            LETALIDADE_PADRONIZADA_AIH = E_AGE_AIH/sum(POP_PADRAO_AIH),      ## Letalidade esperada padronizada
            POP = sum(POP)) %>%                                              ## Populacao do municipio
  mutate(LETALIDADE_BRUTA_AIH = N_OBITO/N_INTER,                             ## Letalidade bruta
         DATA = as.Date(paste(ANO, MES, '01', sep = '-'), format = '%Y-%m-%d'))  ## Mes
