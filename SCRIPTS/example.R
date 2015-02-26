setwd("/mnt/data/depot/FreeCarto/SCRIPTS")
source("sources/freeCarto.R")

geomTunisie <- readOGR(dsn ="data/",layer = "Tunisie_snuts4" )
coast <- readOGR(dsn ="data/",layer = "coast" )
dataTunisie <- read.csv( "data/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",encoding="latin1",)




# plot(geomTunisie)
# head(dataTunisie)


# Add a map from osm, stamen, bing...
OSMMap(obj = geomTunisie, type = "stamen-watercolor", add = F) 

# Add layers (point, poly, line)
StaticMap(obj = geomTunisie, add = T, col = "#D2EAE7", border = "#56726E", lwd = 0.5)
StaticMap(obj = coast, add = T, col = "purple", lwd = 2)
# Add a nice layout
LayoutMap(title = "Check Dat Sweet Map of Tunisia", sources = "Recensement Tunisie", author = "Ouam", frame = T, north = T)
# Add a nice proportional symbols layer
SymbolsMap(obj = geomTunisie, data = dataTunisie, datavar = "SUP2010", 
           symbols = "squares", col = "blue", col2 = "green", 
           breakval = 50000, k = 0.05, add = T) 



StaticMap(obj = geomTunisie, add = F, col = "#D2EAE7", border = "#56726E", lwd = 0.5)
StaticMap(obj = coast, add = T, col = "purple", lwd = 2)
LayoutMap(title = "Check Dat Sweet Map of Tunisia", sources = "Recensement Tunisie", author = "Ouam", frame = T, north = T)
SymbolsDuoMap(obj = geomTunisie, data = dataTunisie, datavar = "IMMIG9404", datavar2 = "EMIG9404", k=300000)






# ## création d'un fond snuts3 (gouvernorats)
# snuts3 <- unionSpatialPolygons(SpP = geomTunisie, IDs = geomTunisie@data$id_snuts3)
# toto <- data.frame(ids = row.names(snuts3))
# snuts3 <- SpatialPolygonsDataFrame(Sr = snuts3, data = toto, match.ID = "ids")
# # centroides
# snuts3pt <- SpatialPointsDataFrame (coords = coordinates(snuts3), data = snuts3@data, proj4string = snuts3@proj4string)
# StaticMap(obj = snuts3pt, col = "yellow", border = "red", lwd = 5, add = T)
# ## data link entre gouvernorats
# snuts3@data$x <- 1
# df <- merge(snuts3@data, snuts3@data, by = "x")
# df <- df[df$ids.x %in% c("TS111", "TS214"),]
# names(df) <- c("fij", "ori", "des")
# df$rnd <- runif(nrow(df),min = 1,max = 15)