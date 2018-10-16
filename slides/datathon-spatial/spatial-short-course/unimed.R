require(rvest)
require(httr)
require(stringr)

url_path <- 'https://portal.unimedbh.com.br/wps/portal/inicio/home/espaco_do_cliente/catalogo'
unimed_session <- html_session(url_path)
unimed_page <- read_html(url_path)

forms <- html_form(unimed_page)
rede_form <- forms[[3]]
rede_form_filled <- rede_form

rede_form_filled$fields$`viewns_Z7_L00AAB1A0OUL90ILO56VKC0O81_:frmRedeAtendimento:cbPlanosRED`$value <- 'UNIFAC_UNIFAC'
rede_form_filled$fields$`viewns_Z7_L00AAB1A0OUL90ILO56VKC0O81_:frmRedeAtendimento:cbRedeAtendimento`$value <- '0'
rede_form_filled$fields$`viewns_Z7_L00AAB1A0OUL90ILO56VKC0O81_:frmRedeAtendimento:cbAbrangenciaRED`$value <- '0'
rede_form_filled$fields$`viewns_Z7_L00AAB1A0OUL90ILO56VKC0O81_:frmRedeAtendimento:txAbrangenciaRED`$value <- '1'
rede_form_filled$fields$`viewns_Z7_L00AAB1A0OUL90ILO56VKC0O81_:frmRedeAtendimento:cbUfRED`$value <- '0'
rede_form_filled$fields$`viewns_Z7_L00AAB1A0OUL90ILO56VKC0O81_:frmRedeAtendimento:txUfRED`$value <- 'MG'
rede_form_filled$fields$`viewns_Z7_L00AAB1A0OUL90ILO56VKC0O81_:frmRedeAtendimento:cbRedRED`$name <- '0'
rede_form_filled$fields$`viewns_Z7_L00AAB1A0OUL90ILO56VKC0O81_:frmRedeAtendimento:cbCidadesRED`$value <- '0'
rede_form_filled$fields$`viewns_Z7_L00AAB1A0OUL90ILO56VKC0O81_:frmRedeAtendimento:txCidadeRED`$value <- '2375'
rede_form_filled$fields$`viewns_Z7_L00AAB1A0OUL90ILO56VKC0O81_:frmRedeAtendimento:cbBairrosRED`$value <- '0'
rede_form_filled$fields$`viewns_Z7_L00AAB1A0OUL90ILO56VKC0O81_:frmRedeAtendimento:cbTipoLocalRED`$value <- '0'
rede_form_filled$fields$`viewns_Z7_L00AAB1A0OUL90ILO56VKC0O81_:frmRedeAtendimento:cbEspecialidadesRED`$value <- '0'
rede_form_filled$fields$`viewns_Z7_L00AAB1A0OUL90ILO56VKC0O81_:frmRedeAtendimento:cbQualificacaoREDE`$value <- '0'
  
resp <- submit_form(session = unimed_session, form = rede_form_filled)
out <- data.frame()
##-- Tipo
tipo <- resp %>%
  html_nodes(xpath = '//*[@id="resultados"]/div/div[1]/span[5]') %>%
  html_text() %>%
  str_replace_all(pattern = '\r+|\t+|\n+|Tipo: ', replacement = '') %>%
  str_trim()

##-- Razão Social
razao_social <- resp %>%
  html_nodes(xpath = '//*[@id="resultados"]/div/div[2]/text()[1]') %>%
  html_text() %>%
  str_replace_all(pattern = '\r+|\t+|\n+|Razão Social: ', replacement = '') %>%
  str_trim()

##-- Endereco
endereco <- resp %>%
  # html_nodes(xpath = '//*[@id="resultados"]/div/div[2]/div[1]') %>%
  html_nodes(xpath = '//*[@id="resultados"]/div/div[2]/div[1]/text()') %>%
  html_text()

endereco <- str_replace_all(string = endereco, pattern = '\r+|\t+|\n+', replacement = '')
endereco <- endereco[endereco != '']

endereco_aux <- resp %>%
  html_nodes(xpath = '//*[@id="resultados"]/div/div[2]/div[1]') %>%
  html_text()
n_enderecos <- str_count(string = endereco_aux, pattern = 'Como chegar')

##-- Dataframe
razao_social <- rep(razao_social, n_enderecos)
tipo <- rep(tipo, n_enderecos)
page <- 1
df_out <- data.frame(page, razao_social, tipo, endereco)
out <- rbind(out, df_out)

##-- Dez primeiras páginas
for(i in 1:9) {
  xpath_page <- sprintf('//*[@id="viewns_Z7_L00AAB1A0OUL90ILO56VKC0O81_:j_id_jsp_1496521624_76:%s:lkPGS"]', i)
  next_page <- resp %>%
    follow_link(xpath = xpath_page)
  
  ##-- Tipo
  tipo <- next_page %>%
    html_nodes(xpath = '//*[@id="resultados"]/div/div[1]/span[5]') %>%
    html_text() %>%
    str_replace_all(pattern = '\r+|\t+|\n+|Tipo: ', replacement = '') %>%
    str_trim()
  
  ##-- Razão Social
  razao_social <- next_page %>%
    html_nodes(xpath = '//*[@id="resultados"]/div/div[2]/text()[1]') %>%
    html_text() %>%
    str_replace_all(pattern = '\r+|\t+|\n+|Razão Social: ', replacement = '') %>%
    str_trim()
  
  ##-- Endereco
  endereco <- next_page %>%
    # html_nodes(xpath = '//*[@id="resultados"]/div/div[2]/div[1]') %>%
    html_nodes(xpath = '//*[@id="resultados"]/div/div[2]/div[1]/text()') %>%
    html_text()
  
  endereco <- str_replace_all(string = endereco, pattern = '\r+|\t+|\n+', replacement = '')
  endereco <- endereco[endereco != '']
  
  endereco_aux <- next_page %>%
    html_nodes(xpath = '//*[@id="resultados"]/div/div[2]/div[1]') %>%
    html_text()
  n_enderecos <- str_count(string = endereco_aux, pattern = 'Como chegar')
  
  ##-- Dataframe
  razao_social <- rep(razao_social, n_enderecos)
  tipo <- rep(tipo, n_enderecos)
  page <- i + 1
  df_out <- data.frame(page, razao_social, tipo, endereco)
  out <- rbind(out, df_out)
}

out2 <- out

##---
for(i in 7:9) {
  xpath_page <- sprintf('//*[@id="viewns_Z7_L00AAB1A0OUL90ILO56VKC0O81_:j_id_jsp_1496521624_76:%s:lkPGS"]', i)
  next_page <- next_page %>%
    follow_link(xpath = xpath_page)
  
  ##-- Tipo
  tipo <- next_page %>%
    html_nodes(xpath = '//*[@id="resultados"]/div/div[1]/span[5]') %>%
    html_text() %>%
    str_replace_all(pattern = '\r+|\t+|\n+|Tipo: ', replacement = '') %>%
    str_trim()
  
  ##-- Razão Social
  razao_social <- next_page %>%
    html_nodes(xpath = '//*[@id="resultados"]/div/div[2]/text()[1]') %>%
    html_text() %>%
    str_replace_all(pattern = '\r+|\t+|\n+|Razão Social: ', replacement = '') %>%
    str_trim()
  
  ##-- Endereco
  endereco <- next_page %>%
    # html_nodes(xpath = '//*[@id="resultados"]/div/div[2]/div[1]') %>%
    html_nodes(xpath = '//*[@id="resultados"]/div/div[2]/div[1]/text()') %>%
    html_text()
  
  endereco <- str_replace_all(string = endereco, pattern = '\r+|\t+|\n+', replacement = '')
  endereco <- endereco[endereco != '']
  
  endereco_aux <- next_page %>%
    html_nodes(xpath = '//*[@id="resultados"]/div/div[2]/div[1]') %>%
    html_text()
  n_enderecos <- str_count(string = endereco_aux, pattern = 'Como chegar')
  
  ##-- Dataframe
  razao_social <- rep(razao_social, n_enderecos)
  tipo <- rep(tipo, n_enderecos)
  page <- page + 1
  df_out <- data.frame(page, razao_social, tipo, endereco)
  out <- rbind(out, df_out)
}

saveRDS(object = out, file = '~/Desktop/rede-atendimento-unimed.rds')

##-- Geolocalizando
require(ggmap)
rede_unimed <- readRDS('~/Desktop/rede-atendimento-unimed.rds')
rede_unimed$endereco_google <- paste(as.character(rede_unimed$endereco), 'BELO HORIZONTE', 'BRASIL', sep = '-')

lat_long_df <- geocode(location = rede_unimed$endereco_google, output = 'more')

require(googleway)
key <- "AIzaSyB1MeOcdmRex3n7ShUxOgZyXM5RTxa48tk"
lat_long <- data.frame()

for(i in 1:length(rede_unimed$endereco_google)) {
  aux <- google_geocode(rede_unimed$endereco_google[i], language = "pt-BR",
                        key = key, region = "br")
  
  if(aux$status == "OK")
  {
    aux <- data.frame(ID = i, lat = aux$results$geometry$location$lat, lng = aux$results$geometry$location$lng)
    lat_long <- rbind(lat_long, aux)
  }
  
  else {
    aux <- data.frame(ID = i, lat = NA, lng = NA)
    lat_long <- rbind(lat_long, aux)
  }
  
}

require(mapsBR)
require(sp)
require(spGoogle)
data(regMun)
bh <- subset(regMun, NOME == "Belo Horizonte" & UF == "MG")
lat_long_df <- as.data.frame(lat_long)
saveRDS(lat_long_df, file = '~/Desktop/lat_long_rede_atendimento.rds')

par(mar = c(0, 0, 0, 0))
plot(bh)
points(lat_long_df$lng, lat_long_df$lat, cex = 0.6, pch = 19, col = '#BBBBBBBB')

filtro <- !is.na(lat_long_df[, 2])
xx <- SpatialPointsDataFrame(coords = lat_long_df[filtro, 3:2], data = rede_unimed[filtro, ], proj4string = CRS("+proj=longlat"))
plot(xx)

spGplot(bh, col.pallete = list(col = 'blue', alpha = 0.1), maptype = 'roadmap') + 
  spGplot(xx, description = list(title = 'razao_social', var = 'endereco'))



