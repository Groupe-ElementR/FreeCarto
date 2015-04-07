
?spTransform


# library(raster)
data(TNdeleg)

rOSM <- getOSMLayer(spdf = TNdeleg.spdf, type = "stamen-watercolor")
osmLayer(rOSM, add = F)
staticLayer(spdf = TNdeleg.spdf, col = "#00ff0050", border = "#ffffff10", lwd = 4, add = T)
layoutLayer(title = "check This", sources = "OSM", author = "Mouam", col = "red", coltitle = "black", scale = 100, frame = T, north = T)
choroLayer(spdf = TNdeleg.spdf, df = TNdeleg, var =  "housing", nbclass = 20, distr = c(0,1000,10000,10000000000),col = c("red", "blue", "green"),
           add = T,alpha = T)
propSymbolsLayer(spdf = TNdeleg.spdf, df = TNdeleg, var = "pop_t", breakval = 50000 , col2 = "blue", add=T)
