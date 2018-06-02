# <img src='logo-mg.png' alt='' style = 'width:40%' opacity = '0'> <br/>
require(animation)
require(mapsBR)
require(spdep)
require(scales)
verde_ts <- "#1C5B34"

data(regMun)
mg_mun <- subset(regMun, UF == "MG")
cents <- coordinates(mg_mun)
nb_mg <- poly2nb(mg_mun)

png(filename = "logo-mg.png", width = 9, height = 9/1.618, units = 'in', res = 300)
par(mar = rep(0, 4), bg = "transparent")
plot(mg_mun, col = "transparent", border = NA)
plot(nb_mg, coords = cents, pch = 19, cex = 0.5, add = T, lwd = 1, col = "green2")
points(cents[, 1], cents[, 2], pch = 19, cex = 0.5, col = "white")
dev.off()

png(filename = "logo-mg.png", width = 9, height = 9/1.618, units = 'in', res = 300)
par(mar = rep(0, 4), bg = "transparent")
plot(mg_mun, col = "transparent", border = NA)
plot(nb_mg, coords = cents, pch = 19, cex = 0.5, add = T, lwd = 1, col = alpha("forestgreen", 0.5))
points(cents[, 1], cents[, 2], pch = 19, cex = 0.5, col = "#aaaaaa")
dev.off()

saveGIF({
  for(cols in c("#777777", "#aaaaaa")) {
    par(mar = rep(0, 4), bg = "transparent")
    plot(mg_mun, col = "transparent", border = NA)
    plot(nb_mg, coords = cents, pch = 19, cex = 0.5, add = T, lwd = 1, col = alpha("forestgreen", 0.5))
    points(cents[, 1], cents[, 2], pch = 19, cex = 0.5, col = cols)  
  }
}, movie.name = "logo.gif", interval = .8, nmax = 30, 
ani.width = 600, res = 300)

