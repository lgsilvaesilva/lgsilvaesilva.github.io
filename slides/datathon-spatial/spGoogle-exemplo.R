##-- Instalando e carregando o pacote --##
# install.packages('devtools')
library(devtools)
install_github('lgsilvaesilva/spGoogle')
library(spGoogle)
##-----##

##-- Funções do pacote spGoogle --##
?spRplot
?spGplot
##-----##

##-- Carregando dados --##
data(rio)       #carregando os dados (spGoogle)
class(rio)      #classe do objeto
slotNames(rio)  #quais os slots?
head(rio@data)  #como acesso slots?? Basta usar o operador
##-----##

##-- Mapas estáticos --##
spRplot(data = rio) #mapa simples
spRplot(data = rio, var = 'Dengue')
spRplot(data = rio, var = 'Dengue', maptype = 'roadmap') #alterando o estilo do mapa
spRplot(data = rio, var = 'Dengue', maptype = 'roadmap', col.pallete = list(alpha = 0.5)) #acrescentando transparencia ao mapa
spRplot(data = rio, var = "Income", cuts = c(0,.2,.4,.6,.8,1), col.pallete = list(alpha = 0.5)) #cuts predefinidos
spRplot(data = rio, var = 'Dengue', maptype = 'roadmap', 
        col.pallete = list(alpha = 0.5), 
        cuts = 6, cuts.type = 'quantile', 
        legend.att = list(x = 'topleft', under = '<', over = '>', title = 'Dengue', cex = 1.5))
##-----##

##-- Paleta de cores --##
# install.packages('RColorBrewer')
library(RColorBrewer)
display.brewer.all()

ncuts <- 6
paleta <- brewer.pal(n = ncuts, name = 'Blues')

spRplot(data = rio, var = 'Dengue', maptype = 'roadmap', 
        col.pallete = list(col = paleta, alpha = 0.7), 
        cuts = 6, cuts.type = 'quantile', 
        legend.att = list(x = 'topleft', under = '<', over = '>', title = 'Dengue', cex = 1.5))
##-----##

##-- Pontos/Lines/Grid e o argumento add --##
# install.packages('rgdal')
library(rgdal)

data(meuse.grid)
data(meuse.riv)
data(meuse)

latlong <-  CRS("+proj=longlat +datum=WGS84") 
meuse.grid <- spTransform(meuse.grid, latlong) #transformando para a projeção latlong

spRplot(data = meuse.grid, col.pallete = list(col="lightgrey",alpha=0.5), maptype = "hybrid")
spRplot(meuse.riv, col.pallete=list(col="steelblue", alpha = 0.3), add = T)
spRplot(meuse, col.pallete=list(col="black"), add = TRUE)
spRplot(meuse, var = "zinc", add = T)
##-----##

##-- spGplot --##
#troque o R pelo G

spGplot(data = rio) #mapa simples
spGplot(data = rio, var = 'Dengue')
spGplot(data = rio, var = 'Dengue', maptype = 'roadmap') #alterando o estilo do mapa
spGplot(data = rio, var = 'Dengue', maptype = 'roadmap', col.pallete = list(alpha = 0.5)) #acrescentando transparencia ao mapa
spGplot(data = rio, var = "Income", cuts = c(0,.2,.4,.6,.8,1), col.pallete = list(alpha = 0.5)) #cuts predefinidos
spGplot(data = rio, var = 'Dengue', maptype = 'roadmap', 
        col.pallete = list(alpha = 0.5), 
        cuts = 6, cuts.type = 'quantile', 
        legend.att = list(title = 'Dengue', cex = 1.3))
##-----##

##-- Description --##

#Gráfico de barras no balao
spGplot(data = rio, var = 'Dengue', maptype = 'roadmap', 
        col.pallete = list(alpha = 0.5), 
        description = list(title = 'Name', var = c('Income', 'Urban'), type = 'bar'),
        cuts = 6, cuts.type = 'quantile', 
        legend.att = list(title = 'Dengue', cex = 1.3))

#Gráfico de pizza no balao
spGplot(data = rio, var = 'Dengue', maptype = 'roadmap', 
        col.pallete = list(alpha = 0.5), 
        description = list(title = 'Name', var = c('Income', 'Urban'), type = 'pie'),
        cuts = 6, cuts.type = 'quantile', 
        legend.att = list(title = 'Dengue', cex = 1.3))

#tabela no balao
spGplot(data = rio, var = 'Dengue', maptype = 'roadmap', 
        col.pallete = list(alpha = 0.5), 
        description = list(title = 'Name', var = c('Income', 'Urban'), type = 'table'),
        cuts = 6, cuts.type = 'quantile', 
        legend.att = list(title = 'Dengue', cex = 1.3))

##-- um pouco mais avancado --##
#inserindo figura png no balao
#install.packages('ggplot2')
library(ggplot2)
# dir.create('figures')

setwd("/Users/lgsilvaesilva/Dropbox/Pesquisa-Ajuda/r-development/spGoogle-short_course")
figureName <- file.path('figures', paste0( 'f', 1:nrow(rio) ,'.png'))
# for(i in 1:nrow(rio)){
#   dd <- reshape2::melt(rio@data[i, c('GeoCode', 'Name','Dengue', 'E')], c('GeoCode', 'Name'))
#   gg <- ggplot(data = dd, aes(x = factor(variable), y = value, fill = factor(variable))) +
#     geom_bar(stat = 'identity') +
#     scale_fill_manual(name = '', labels = c('Qtd. observada', 'Qtd. esperada'), breaks = c('Dengue', 'E'), values = c('indianred', 'steelblue')) + 
#     scale_x_discrete(breaks=c("Dengue", "E"), labels=c('Qtd. observada', 'Qtd. esperada')) + 
#     labs(x = '', y = '', title = dd$Name)
#   ggsave(filename = figureName[i], plot = gg, width = 5.71 , height = 5.25)
# }

rio$fig <- path.expand(figureName)
spGplot(rio, 
        var = 'SMR', 
        description = list(var = 'fig', type = 'png', title = 'Name'), 
        cuts = c(0, 0.5, 1, 2, 5, 10), 
        savekml = 'rio.kml',
        col.pallete = list(alpha = 0.8), maptype = 'roadmap')
##-----##

##-- Exemplo Pampulha --##
#install.packages('ggmap')
library(ggmap)
adress <- paste("Avenida Otacilio Negrao de Lima,", seq(1, 30000, by = 300), ", Belo Horizonte - Minas Gerais")
geo.pt <- geocode(adress, messaging = F, output = 'more')
geo.pt <- as.data.frame(geo.pt)
load('geoPampulha.RData')
pampu.pt <- SpatialPointsDataFrame(coords = geo.pt[,1:2], data = geo.pt, proj4string = latlong)

spRplot(pampu.pt, col.pallete = list(col = 'red'), zoom = 14)
spGplot(pampu.pt, col.pallete = list(col = 'steelblue'))
spGplot(pampu.pt, col.pallete = list(col = 'steelblue', alpha = 0.5), maptype = 'roadmap')

pampu.pt$hello <- 'Oi! Eu sou um ponto!'
spGplot(pampu.pt, 
        col.pallete = list(col = 'steelblue', alpha = 0.5), maptype = 'roadmap', 
        description = list(title = 'hello'))
##-----##

##-- mapsBR --##
#exmplificando o uso da função +
# install_github('lgsilvaesilva/mapsBR')
library(mapsBR)
data(regMun) #municipios do Brasil
data("regUF")
head(regMun@data)

mg <- subset(regMun, UF == 'MG')
gv <- subset(regMun, NOME == 'Governador Valadares')
jf <- subset(regMun, NOME == 'Juiz de Fora')
bh <- subset(regMun, NOME == 'Belo Horizonte')

mg_mapa <- spGplot(mg, col.pallete = list(col = 'indianred', alpha = 0.6), maptype = 'roadmap') + 
  spGplot(gv, col.pallete = list(col = 'steelblue', alpha = 1), description = list(title = 'NOME'), maptype = 'roadmap') +
  spGplot(jf, col.pallete = list(col = 'steelblue', alpha = 1), description = list(title = 'NOME'), maptype = 'roadmap') +
  spGplot(bh, col.pallete = list(col = 'steelblue', alpha = 1), description = list(title = 'NOME'), maptype = 'roadmap')

mg_mapa + 
  spGplot(regUF, col.pallete = list(col = 'white', alpha = 0))
##-----##

##-- Pontos + Polygons --##
rio_pts <- SpatialPointsDataFrame(coords = coordinates(rio),  data = rio@data, proj4string = CRS(proj4string(rio)) )
spGplot(data = rio, var = 'Income', maptype = 'roadmap', legend.att = list(title = 'Income'), col.pallete = list(alpha = 0.6)) +
  spGplot(data = rio_pts, var = 'Urban', legend.att = list(title = 'Urban'), description = list(title = 'Name', var = c('Income', 'Urban'), type = 'bar'))
##-----##

