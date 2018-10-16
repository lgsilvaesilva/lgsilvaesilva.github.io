## Pacotes ----
require(data.table)
require(dplyr)
require(stringr)
require(ggplot2)
require(ggthemes)
require(gridExtra)
require(mapsBR)
## Lendo a base ----
sinan_pop <- readRDS(file = "data/sinan_pop.RDS")
## + Mapas ----
data("regMun")
regMun@data$COD_MUN <- str_sub(string = regMun@data$COD, start = 1, end = 6)
regMun_pts <- fortify(regMun, region = "COD_MUN")

sinan_pop_2016 <- sinan_pop %>%
  filter(ANO == 2016)

regMun_df = inner_join(regMun_pts, sinan_pop_2016, by = c("id" = "COD_MUN"))
## ++ Criando breaks ----
#breaks <- unique(c(0, quantile(sinan_pop_2016$TAXA, probs = c(0, 0.25, 0.5, 0.75, 0.9, .99, 1))))
#regMun_df$TAXA_CAT <- cut(regMun_df$TAXA, breaks = breaks, include.lowest = T)

# breaks_UF <- c(0, quantile(sinan_pop_2016$TAXA_BAYESIANA_UF, probs = c(0, 0.25, 0.5, 0.75, 0.9, .99, 1)))
# regMun_df$TAXA_CAT_UF <- cut(regMun_df$TAXA_BAYESIANA_UF, breaks = breaks_UF, include.lowest = T)
# 
# breaks_BR <- c(0, quantile(sinan_pop_2016$TAXA_BAYESIANA_BR, probs = c(0, 0.25, 0.5, 0.75, 0.9, .99, 1)))
# regMun_df$TAXA_CAT_BR <- cut(regMun_df$TAXA_BAYESIANA_BR, breaks = breaks_BR, include.lowest = T)

breaks <- unique(c(0, 1, quantile(c(sinan_pop_2016$TAXA,
                                    sinan_pop_2016$TAXA_BAYESIANA_UF,
                                    sinan_pop_2016$TAXA_BAYESIANA_BR),
                                  probs = c(0, 0.25, 0.5, 0.75, 0.9, .99, 1))))

regMun_df$TAXA_CAT <- cut(regMun_df$TAXA, breaks = breaks, include.lowest = T)
regMun_df$TAXA_CAT_UF <- cut(regMun_df$TAXA_BAYESIANA_UF, breaks = breaks, include.lowest = T)
regMun_df$TAXA_CAT_BR <- cut(regMun_df$TAXA_BAYESIANA_BR, breaks = breaks, include.lowest = T)

breaks_BR <- breaks_UF <- breaks

low <- "#fee8c8"
high <- "#990000"
cores <- colorRampPalette(colors = c(low, high))
## ++ Mapas ----
g1 <- ggplot(regMun_df, mapping = aes(long, lat, group = group, fill = TAXA)) + 
  geom_polygon() +
  geom_path(color = "transparent") +
  coord_equal() +
  scale_fill_continuous("Taxa por 100.000 hab.", low = low, high = high) +
  xlab("") + ylab("") +
  theme_map() +
  theme(legend.background = element_blank())

g2 <- ggplot(regMun_df, mapping = aes(long, lat, group = group, fill = TAXA_BAYESIANA_UF)) + 
  geom_polygon() +
  geom_path(color = "transparent") +
  coord_equal() +
  scale_fill_continuous("Taxa bayesiana empírica (UF) \n por 100.000 hab.", low = low, high = high) +
  xlab("") + ylab("") +
  theme_map() +
  theme(legend.background = element_blank())

g3 <- ggplot(regMun_df, mapping = aes(long, lat, group = group, fill = TAXA_BAYESIANA_BR)) + 
  geom_polygon() +
  geom_path(color = "transparent") +
  coord_equal() +
  scale_fill_continuous("Taxa bayesiana empírica (BR) \n por 100.000 hab.", low = low, high = high) +
  xlab("") + ylab("") +
  theme_map() +
  theme(legend.background = element_blank())

g4 <- ggplot(regMun_df, mapping = aes(long, lat, group = group, fill = TAXA_CAT)) + 
  geom_polygon() +
  geom_path(color = "transparent") +
  coord_equal() +
  scale_fill_manual("Taxa bayesiana empírica \n por 100.000 hab.", 
                    values = cores(length(breaks)), 
                    labels = levels(regMun_df$TAXA_CAT_UF)) +
  xlab("") + ylab("") +
  theme_map() +
  theme(legend.background = element_blank())

g5 <- ggplot(regMun_df, mapping = aes(long, lat, group = group, fill = TAXA_CAT_UF)) + 
  geom_polygon() +
  geom_path(color = "transparent") +
  coord_equal() +
  scale_fill_manual("Taxa bayesiana empírica (UF) \n por 100.000 hab.", 
                    values = cores(length(breaks_UF)), 
                    labels = levels(regMun_df$TAXA_CAT_UF)) +
  xlab("") + ylab("") +
  theme_map() +
  theme(legend.background = element_blank())

g6 <- ggplot(regMun_df, mapping = aes(long, lat, group = group, fill = TAXA_CAT_BR)) + 
  geom_polygon() +
  geom_path(color = "transparent") +
  coord_equal() +
  scale_fill_manual("Taxa bayesiana empírica (BR) \n por 100.000 hab.", 
                    values = c("grey", cores(length(breaks_BR)-1)), 
                    labels = levels(regMun_df$TAXA_CAT_BR)) +
  xlab("") + ylab("") +
  theme_map() +
  theme(legend.background = element_blank())

# grid.arrange(g1, g2, g3, nrow = 1)
# grid.arrange(g4, g5, g6, nrow = 1)

grid.arrange(g1, g2, g3, g4, g5, g6, nrow = 2)

regMun_df_uf <- subset(regMun_df, COD_UF == "31")

ggplot(regMun_df_uf, mapping = aes(long, lat, group = group, fill = TAXA_CAT_BR)) + 
  geom_polygon() +
  geom_path(color = "transparent") +
  coord_equal() +
  scale_fill_manual("Taxa bayesiana empírica (BR) \n por 100.000 hab.", 
                    values = c("grey", cores(length(breaks_BR)-1)), 
                    labels = levels(regMun_df$TAXA_CAT_BR)) +
  xlab("") + ylab("") +
  theme_map() +
  theme(legend.background = element_blank())
