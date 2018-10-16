## Pacotes ----
require(data.table)
require(dplyr)
require(purrr)
require(stringr)
require(shiny)
require(leaflet)
require(highcharter)
require(htmltools)
require(ggplot2)
require(ggthemes)
require(treemap)
require(gridExtra)
require(mapsBR)
require(maptools)
## Estupro ----
## + Cores ----
colLine <- "#db4716"
## + Lendo a base ----
## ++ SINAN ----
sinan_pop <- readRDS(file = "~/Documentos/S4G/sinan_pop.RDS")
sinan <- readRDS(file = "~/Documentos/S4G/SINAN/R/sinan.RDS")

sinan <- sinan %>%
  filter(CS_SEXO == "Feminino", SEX_ESTUPR == "Sim") %>%
  select(-CS_SEXO)

faixas_etarias <- c(0, 9, 14, 19, 29, 59, 118)

sinan <- sinan %>%
  mutate(IDADE_DIAS = as.numeric(DT_OCOR - DT_NASC),
         IDADE = IDADE_DIAS/365,
         IDADE_CAT = cut(x = IDADE, breaks = faixas_etarias, include.lowest = T))

## + Gráfico da evolução das notificações ao longo dos anos ----
contagens_anuais <- sinan_pop %>%
  filter(!(ANO %in% c(2009, 2010))) %>%
  group_by(ANO) %>%
  summarise(CONTAGENS = sum(NOTIFICACOES))

h1 <- highchart() %>%
  hc_add_series(data = contagens_anuais, 
                mapping = hcaes(y = CONTAGENS, x = factor(ANO)), 
                type = "line", 
                name = "Número de notificações",
                lineColor = colLine, 
                color = colLine,
                lineWidth = 4,
                marker = list(lineColor = colLine,
                              lineWidth = 3)) %>%
  hc_xAxis(categories = 2011:2016)
## + Gráfico da evolução das notificações ao longo dos anos por faixa etária ----
contagens_idades <- sinan %>%
  group_by(ANO, IDADE_CAT) %>%
  summarise(CONTAGENS = n()) %>%
  filter(!is.na(IDADE_CAT))

h2 <- highchart() %>%
  hc_add_series(data = contagens_idades, 
                mapping = hcaes(y = CONTAGENS, x = ANO, group = IDADE_CAT), 
                type = "line", 
                lineWidth = 4) %>%
  hc_xAxis(categories = 2011:2016)

## + Mapas ----
data("regMun")
regMun@data$COD_MUN <- str_sub(string = regMun@data$COD, start = 1, end = 6)
regMun_pts <- fortify(regMun, region = "COD_MUN")

sinan_pop_2016 <- sinan_pop %>%
  filter(ANO == 2016)

regMun_df = inner_join(regMun_pts, sinan_pop_2016, by = c("id" = "COD_MUN"))
regMun_df$OCORREU = ifelse(regMun_df$NOTIFICACOES > 0, "Sim", "Não")

## ++ Criando breaks ----
breaks <- unique(c(0, quantile(sinan_pop_2016$TAXA, probs = c(0, 0.25, 0.5, 0.75, 0.9, .99, 1))))
regMun_df$TAXA_CAT <- cut(regMun_df$TAXA, breaks = breaks)

breaks_UF <- c(0, quantile(sinan_pop_2016$TAXA_BAYESIANA_UF, probs = c(0, 0.25, 0.5, 0.75, 0.9, .99, 1)))
regMun_df$TAXA_CAT_UF <- cut(regMun_df$TAXA_BAYESIANA_UF, breaks = breaks_UF)

breaks_BR <- c(0, quantile(sinan_pop_2016$TAXA_BAYESIANA_BR, probs = c(0, 0.25, 0.5, 0.75, 0.9, .99, 1)))
regMun_df$TAXA_CAT_BR <- cut(regMun_df$TAXA_BAYESIANA_BR, breaks = breaks_BR)

cores <- RColorBrewer::brewer.pal(n = length(breaks_BR), name = "Reds")
## ++ Mapas ----

g1 <- ggplot(regMun_df, mapping = aes(long, lat, group = group, fill = OCORREU)) + 
  geom_polygon() +
  geom_path(color = "white", size = 0.1) +
  coord_equal() +
  scale_fill_manual("Ocorrência de crime notificado", values = c("Sim" = cores[length(cores)-2],
                                                                 "Não" = cores[3])) +
  xlab("") + ylab("") +
  theme_map()

g2 <- ggplot(regMun_df, mapping = aes(long, lat, group = group, fill = TAXA_BAYESIANA_BR)) + 
  geom_polygon() +
  geom_path(color = "white", size = 0.1) +
  coord_equal() +
  scale_fill_gradient_tableau("Red", "Taxa bayesiana empírica \n por 100.000 hab.") +
  xlab("") + ylab("") +
  theme_map()

g3 <- ggplot(regMun_df, mapping = aes(long, lat, group = group, fill = TAXA_CAT)) + 
  geom_polygon() +
  geom_path(color = "white", size = 0.1) +
  coord_equal() +
  scale_fill_manual("Taxa bayesiana empírica \n por 100.000 hab.", 
                    values = cores[1:length(breaks)], 
                    labels = levels(regMun_df$TAXA_CAT_UF)) +
  xlab("") + ylab("") +
  theme_map()

g4 <- ggplot(regMun_df, mapping = aes(long, lat, group = group, fill = TAXA_CAT_UF)) + 
  geom_polygon() +
  geom_path(color = "white", size = 0.1) +
  coord_equal() +
  scale_fill_manual("Taxa bayesiana empírica \n por 100.000 hab.", 
                    values = cores, 
                    labels = levels(regMun_df$TAXA_CAT_UF)) +
  xlab("") + ylab("") +
  theme_map()

g5 <- ggplot(regMun_df, mapping = aes(long, lat, group = group, fill = TAXA_CAT_BR)) + 
  geom_polygon() +
  geom_path(color = "white", size = 0.1) +
  coord_equal() +
  scale_fill_manual("Taxa bayesiana empírica \n por 100.000 hab.", 
                    values = cores, 
                    labels = levels(regMun_df$TAXA_CAT_BR)) +
  xlab("") + ylab("") +
  theme_map()

grid.arrange(g1, g2, nrow = 1)

## Portal da transparência ----
## + Bolsa família ----
dados_mun <- readRDS(file = "~/Dropbox/outros_projetos/UFMG/Stats4Good/apresentacoes/seminario_augusto/app-mapas/data/dados_mun_complet.rds")
data(regMun)

dados_2016 <- dados_mun[ANO == 2016, ]
regiao_shp_2016 <- subset(regMun, UF == "MG")
regiao_shp_2016 <- merge(regiao_shp_2016, dados_2016, by.x = "COD", by.y = "MUNCODDV")

bbox_br <- bbox(regiao_shp_2016)
pal <- colorNumeric("Reds", regiao_shp_2016@data[, "valor_medio"])
## ++ 2016 ----
popup_out <- sprintf("<strong>%s</strong><br><br>Valor: R$ %s<br><br>Nº beneficiários: %s", 
                     regiao_shp_2016@data[, "NOME"], 
                     round(regiao_shp_2016@data[, "valor_medio"], 1),
                     regiao_shp_2016@data[, "n_favorecido"])

cores_mapa <-  pal(regiao_shp_2016@data[, "valor_medio"])

l <- leaflet(data = regiao_shp_2016) %>%
  clearShapes() %>%
  addPolygons(stroke = T,
              color = "#bbbbbb",
              popup = popup_out,
              weight = 0.75,
              smoothFactor = 0.1,
              fillOpacity = 0.9,
              fillColor = cores_mapa,
              highlightOptions = highlightOptions(color = "white", 
                                                  weight = 2,
                                                  bringToFront = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron,
                   options = providerTileOptions(noWrap = T)) %>%
  addLegend("bottomright", pal = pal, values = regiao_shp_2016@data[, "valor_medio"],
            title = "Valor Médio",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1) 

## + Dados de transferências ----
load("~/Dropbox/outros_projetos/UFMG/Stats4Good/apresentacoes/comInteligencia/data/dadosMG.RData")

dados <- dados %>%
  filter(`Nome Municipio` == "BELO HORIZONTE") %>%
  group_by(`Nome Funcao`, `Nome Sub Funcao`) %>%
  summarise(valor = round(sum(`Valor Parcela`), 2),
            valor_label = format(valor, big.mark = ".", decimal.mark = ",")) %>%
  filter(`Nome Funcao` != "Encargos Especiais")

tm <- treemap(dados, index = c("Nome Funcao", "Nome Sub Funcao"),
              vSize = "valor", vColor = "Nome Sub Funcao", draw = F)

hctreemap(tm, 
          allowDrillToNode = T, 
          layoutAlgorithm = "squarified",
          dataLabels = list(enabled = F),
          levelIsConstant = F,
          levels = list(list(level = 1,
                             dataLabels = list(enabled = T)))) %>%
  hc_tooltip(useHTML = T,
             headerFormat = "<table>",
             pointFormat = "Valor: R$ {point.value}",
             footerFormat = "</table>") %>%
  hc_title(text = "Transferências realizada pelo governo federal para BH, em 2016")

## + Salvando bases para facilitar a apresentação ----
save(contagens_anuais, contagens_idades, regMun_df, cores, dados_mun, tm,
     file = "~/Dropbox/outros_projetos/UFMG/Stats4Good/apresentacoes/comInteligencia/data/dados_graficos.RData")
