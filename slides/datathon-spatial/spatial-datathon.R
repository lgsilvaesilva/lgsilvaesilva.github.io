## ----setup, include=FALSE------------------------------------------------
options(htmltools.dir.version = FALSE)
knitr::opts_chunk$set(fig.bg = function(before, options, envir) {
  if (before) par(bg = options$fig.bg)
})

## ----ex1-leaflet, eval=TRUE, fig.height=4, include=TRUE, out.width='100%'----
library(leaflet)
data(quakes)
head(quakes)

## ----ex2-leaflet, eval=FALSE, fig.height=4, include=TRUE, out.width='100%'----
## leaflet(data = quakes[1:20,]) %>%
##   addTiles() %>%
##   addMarkers(lng = ~long,
##              lat = ~lat,
##              popup = ~mag),
##              label = ~mag)

## ----ex1-leaflet-run, echo=FALSE, fig.height=6.5, out.width='100%'-------
library(leaflet)
data(quakes)

leaflet(data = quakes[1:20,]) %>% 
  addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))

## ----mapa-acidente, echo=FALSE, fig.cap='', fig.align='center', fig.height=5, fig.width=5, message=FALSE, warning=FALSE----
require(sf)
si <- read.csv2('~/Downloads/si-bol_2015.csv', encoding = 'latin1')
si$coordenada_x <- as.numeric(as.character(si$coordenada_x))/100
si$coordenada_y <- as.numeric(as.character(si$coordenada_y))/100
si <- subset(si, coordenada_x != 0)
pts <- st_multipoint(x = as.matrix(si[, c('coordenada_x', 'coordenada_y')])) %>%
  st_sfc() %>%
  st_cast('POINT') %>%
  st_set_crs("+init=epsg:29193")

par(mar = rep(0, 4), bg = 'transparent')
plot(st_geometry(pts), pch = 19, col = '#BBBBBB22')

## ----mapa-acidente-ex1, echo=TRUE, fig.align='center', fig.cap='', fig.height=5, fig.width=5, message=FALSE, warning=FALSE, eval=FALSE, size='tiny'----
## require(sf)
## si <- read.csv2('~/Downloads/si-bol_2015.csv', encoding = 'latin1')
## pts <- st_multipoint(x = as.matrix(si[, c('coordenada_x', 'coordenada_y')])) %>%
##   st_sfc() %>%
##   st_cast('POINT') %>%
##   st_set_crs("+init=epsg:29193")
## 
## par(mar = rep(0, 4), bg = 'transparent')
## plot(st_geometry(pts), pch = 19, col = '#BBBBBB22')

## ----mapa-acidente-ex2, echo=FALSE, fig.cap='Mapa', fig.align='center', fig.height=4.5, fig.width=5, message=FALSE, warning=FALSE----
require(sp)
si <- read.csv2('~/Downloads/si-bol_2015.csv', encoding = 'latin1')
si$coordenada_x <- as.numeric(as.character(si$coordenada_x))
si$coordenada_y <- as.numeric(as.character(si$coordenada_y))
si <- subset(si, coordenada_x != 0)
pts <- SpatialPointsDataFrame(coords = si[, c('coordenada_x', 'coordenada_y')], data = si, proj4string = CRS("+init=epsg:29193"))
par(mar = rep(0, 4), bg = 'transparent')
plot(pts, pch = 19, col = '#BBBBBB22')

## ----mapa-uf-ex1, echo=FALSE, fig.align='center', fig.height=5, message=FALSE, warning=FALSE, out.width='100%'----
data(rio, package = 'spGoogle')
library(leaflet)
bins <- c(0, 1, 2, 3, 4, 5, Inf)
pal <- colorBin("YlOrRd", domain = rio$SMR, bins = bins)

leaflet(rio) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addPolygons(fillColor = ~pal(SMR), 
              weight = 1.5,
              opacity = 1,
              fillOpacity = 0.7,
              color = "gray",
              highlight = highlightOptions(
    weight = 5,
    color = "#666",
    # dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
    label = sprintf("%s - SMR: %s", rio$Name, round(rio$SMR, 3)),
    labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.7, title = 'SMR',
  position = "bottomright")

## ----echo=FALSE, message=FALSE, warning=FALSE, fig.align='center', paged.print=FALSE, prompt=TRUE----
p <- rbind(c(3.2,4), c(3,4.6), c(3.8,4.4), c(3.5,3.8), c(3.4,3.6), c(3.9,4.5))
mp <- st_multipoint(p)
s1 <- rbind(c(0,3),c(0,4),c(1,5),c(2,5))
ls <- st_linestring(s1)
s2 <- rbind(c(0.2,3), c(0.2,4), c(1,4.8), c(2,4.8))
s3 <- rbind(c(0,4.4), c(0.6,5))
mls <- st_multilinestring(list(s1,s2,s3))
p1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
p2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
pol <-st_polygon(list(p1,p2))
p3 <- rbind(c(3,0), c(4,0), c(4,1), c(3,1), c(3,0))
p4 <- rbind(c(3.3,0.3), c(3.8,0.3), c(3.8,0.8), c(3.3,0.8), c(3.3,0.3))[5:1,]
p5 <- rbind(c(3,3), c(4,2), c(4,3), c(3,3))
mpol <- st_multipolygon(list(list(p1,p2), list(p3,p4), list(p5)))
gc <- st_geometrycollection(list(mp, mpol, ls))
par(mar = c(0.1, 0.1, 1.3, 0.1), mfrow = c(2, 3))
plot(mp, col = 'red')
box()
title("MULTIPOINT")

plot(ls, col = 'red')
box()
title("LINESTRING")

plot(mls, col = 'red')
box()
title("MULTILINESTRING")

plot(pol, border = 'red', col = 'grey', xlim = c(0,4))
box()
title("POLYGON")

plot(mpol, border = 'red', col = 'grey')
box()
title("MULTIPOLYGON")

plot(gc, border = 'grey', col = 'grey')
box()
title("GEOMETRYCOLLECTION")
par(mfrow = c(1, 1))

## ----message=FALSE, warning=FALSE, cache=T, fig.align='center',fig.height=5/1.618, fig.width=5, fig.bg='transparent', dpi=150----
require(sf)
rs_sf <- st_read('rs_sf.shp', quiet = T) # shapefile disponibilizado pelo IBGE
par(mar = rep(0, 4), bg = 'transparent')
plot(st_geometry(rs_sf), col = 'steelblue', border = 'gray90')

## ----message=FALSE, warning=FALSE, cache=T, fig.align='center', fig.height=4, fig.width=4----
head(rs_sf)

## ----message=FALSE, warning=FALSE, cache=T, fig.align='center', fig.height=6, fig.width=7----
distancia <- st_distance(subset(rs_sf, NOME == "Porto Alegre"), rs_sf)
rs_sf$distancia <- distancia[1, ]
par(mar = rep(0, 4), bg = 'transparent')
plot(rs_sf['distancia'], main = "Distância para PoA")

## ----message=FALSE, warning=FALSE, cache=T, fig.align='center', fig.height=4, fig.width=4----
require(sf)
rs_sf <- st_read('rs_sf.shp', quiet = T)
head(rs_sf, 3)

## ----message=FALSE, warning=FALSE, cache=T, fig.align='center', fig.height=4, fig.width=4----
class(rs_sf)
class(rs_sf$geometry)
pa_sf <- subset(rs_sf, NOME == "Porto Alegre")
st_write(pa_sf, dsn = 'pa_sf.shp', quiet = F, delete_dsn = T)

## ----message=FALSE, warning=FALSE, cache=T,fig.width=5, fig.height=5/1.618,fig.align='center'----
rio <- st_read(dsn = 'rio_sf.shp', quiet = T)
par(mar = rep(0, 4), bg = 'transparent')
plot(rio['Income'], # variável a ser visualizada
     axes = TRUE,   # apresentar os eixos no gráfico
     key.pos = 4,   # posição da legenda
     breaks = 'quantile', # tipo de quebra da variável
     nbreaks = 5,         # número de quebras
     pal = sf.colors(5),  # paleta de cores
     key.width = lcm(2.5)) # larguea da legenda

## ----message=FALSE, warning=FALSE, cache=T, fig.width=5, fig.height=5/1.618, fig.align='center'----
par(mar = rep(0, 4), bg = 'transparent')
plot(rio['Income'], 
     axes = TRUE, 
     key.pos = 1,
     breaks = c(0, .2, .4, .6, .8, 1), # quebras customizadas
     pal = RColorBrewer::brewer.pal(n = 5, 'Blues'), # usando RColorBrewer
     key.width = lcm(2.5))

## ----message=FALSE, warning=FALSE, cache=T, fig.width=5, fig.height=5/1.618, fig.align='center'----
library(dplyr)
rio <- rio %>%
  mutate(risco = cut(SMR, breaks = c(0, 1, 1.5, Inf), 
                     labels = c('Baixo', 'Médio', 'Alto')))

plot(rio['risco'], 
     axes = TRUE, 
     key.pos = 1,
     pal = c('lightgreen', 'orange', 'darkred'))

## ----message=FALSE, warning=FALSE, cache=T, fig.width=5, fig.height=5/1.618, fig.align='center'----
library(dplyr)
rio <- rio %>%
  mutate(risco = cut(SMR, breaks = c(0, 1, 1.5, Inf), 
                     labels = c('Baixo', 'Médio', 'Alto')))
par(mfrow = c(1, 2))
plot(rio[c('Urban', 'Income')])

## ----eval=FALSE, fig.align='center', fig.height=5/1.618, fig.width=5, message=FALSE, warning=FALSE, cache=T, include=T----
## library(ggplot2)
## rio_pts <- rio %>%
##   as.data.frame() %>%
##   mutate(Long = as.numeric(as.character(Long)),
##          Lat = as.numeric(as.character(Lat))) %>%
##   st_as_sf(coords = c("Long", "Lat"), crs = st_crs(rio))
## 
## ggplot() +
##   geom_sf(data = rio, aes(fill = 1000*Dengue/Pop)) +
##   geom_sf(data = rio_pts, aes(size = Urban, color = Urban)) +
##   scale_fill_continuous(low = "lightblue", high = "darkblue") +
##   theme_minimal()

## ----echo=FALSE, fig.align='center', fig.height=8/1.618, fig.width=8, message=FALSE, warning=FALSE, cache=T, paged.print=FALSE----
library(ggplot2)
rio_pts <- rio %>%
  as.data.frame() %>%
  mutate(Long = as.numeric(as.character(Long)),
         Lat = as.numeric(as.character(Lat))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = st_crs(rio))

ggplot() +
  geom_sf(data = rio, aes(fill = 1000*Dengue/Pop)) +
  geom_sf(data = rio_pts, aes(size = Urban, color = Urban)) +
  scale_fill_continuous(low = "lightblue", high = "darkblue") +
  theme_minimal()


## ----eval=FALSE, prompt=TRUE---------------------------------------------
## install.packages("leaflet")
## library(leaflet) # ou require(leaflet)

## ----fig.align='center', fig.height=5, out.width='100%', message=FALSE, warning=FALSE----
library(leaflet)
latlong_rede <- readRDS("dados/lat_long_rede_atendimento.rds")
leaflet() %>%
  addTiles() %>%
  addMarkers(lng=latlong_rede$lng, lat=latlong_rede$lat)

## ----echo=FALSE, fig.align='center', fig.height=5, message=FALSE, warning=FALSE----
library(leaflet)
latlong_rede <- readRDS("dados/lat_long_rede_atendimento.rds")
latlong_rede <- latlong_rede[!duplicated(latlong_rede$ID), ]
rede_unimed <- readRDS("dados/rede-atendimento-unimed.rds")
latlong_rede <- cbind(latlong_rede, rede_unimed)

## ----fig.align='center', fig.height=5, out.width='100%', message=FALSE, warning=FALSE----
library(leaflet)
leaflet() %>%
  addTiles() %>%
  addMarkers(lng=latlong_rede$lng,
             lat=latlong_rede$lat,
             popup = latlong_rede$razao_social, 
             label = latlong_rede$tipo)

## ----fig.align='center', fig.height=5, out.width='100%', message=FALSE, warning=FALSE----
library(leaflet.extras)
leaflet(quakes) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView( 178, -20, 5 ) %>%
  addHeatmap(lng = ~long, lat = ~lat, intensity = ~mag,
             blur = 20, max = 0.05, radius = 15)

## ----echo=TRUE, eval=FALSE, fig.align='center', fig.height=5, message=FALSE, warning=FALSE, out.width='100%', include=TRUE----
## data(rio)
## bins <- c(0, 1, 2, 3, 4, 5, Inf)
## pal <- colorBin("YlOrRd", domain = rio$SMR, bins = bins)
## 
## leaflet(rio) %>%
##   addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
##   addPolygons(fillColor = ~pal(SMR),
##               weight = 1.5,
##               opacity = 1,
##               fillOpacity = 0.7,
##               color = "gray",
##                highlight = highlightOptions(
##     weight = 5,
##     color = "#666",
##     dashArray = "",
##     fillOpacity = 0.7,
##     bringToFront = TRUE),
##     label = sprintf("%s - SMR: %s", rio$Name, round(rio$SMR, 3)),
##     labelOptions = labelOptions(
##     style = list("font-weight" = "normal", padding = "3px 8px"),
##     textsize = "15px",
##     direction = "auto")) %>%
##   addLegend(pal = pal, values = ~SMR, opacity = 0.7, title = NULL,
##   position = "bottomright")

## ----echo=FALSE, fig.align='center', fig.height=5, message=FALSE, warning=FALSE, out.width='100%'----
data(rio)
bins <- c(0, 1, 2, 3, 4, 5, Inf)
pal <- colorBin("YlOrRd", domain = rio$SMR, bins = bins)

leaflet(rio) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addPolygons(fillColor = ~pal(SMR), 
              weight = 1.5,
              opacity = 1,
              fillOpacity = 0.7,
              color = "gray",
               highlight = highlightOptions(
    weight = 5,
    color = "#666",
    fillOpacity = 0.7,
    bringToFront = TRUE),
    label = sprintf("%s - SMR: %s", rio$Name, round(rio$SMR, 3)),
    labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.7, title = NULL,
  position = "bottomright")

