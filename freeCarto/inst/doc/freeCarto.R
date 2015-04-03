## ------------------------------------------------------------------------
library(freeCarto)
# load the data
data("TNdeleg")
# This dataset contains a data.frame and a SpatialPolygonsDataFrame 
# of the tunisian delegations
?TNdeleg
?TNdeleg.spdf
# The StaticMap function allows to plot a Spatial*DataFrame
StaticMap(obj = TNdeleg.spdf, col = "#04233B", border = "#295170", lwd = 0.75, 
          add = FALSE)


## ------------------------------------------------------------------------
StaticMap(obj = TNdeleg.spdf, col = "#04233B", border = "#295170", lwd = 0.75, 
          add = FALSE)
SymbolsMap(obj = TNdeleg.spdf, data = TNdeleg, datavar = "pop_t", 
           title = "Total Population", 
           pos = "topleft", add = TRUE, symbols = "circles", k = 0.2, 
           col = "#469372")

## ------------------------------------------------------------------------
par(mar = c(0,0,1.25,0))
StaticMap(obj = TNdeleg.spdf, col = "#04233B", border = "#295170", lwd = 0.75,
          add = FALSE)
SymbolsMap(obj = TNdeleg.spdf, data = TNdeleg, datavar = "pop_t", 
           title = "Total Population", 
           pos = "left", add = TRUE, symbols = "circles", k = 0.2, 
           col = "#469372")
LayoutMap(title = "Population distribution in Tunisia, 2004", 
          sources = "INS - Tunisia, General census of population and housing 2004.",
          col = "#003F24", txtcol = "white",
          author = "Timothée Giraud, 2015",scale = 100, frame = TRUE, 
          north = TRUE )

## ------------------------------------------------------------------------
OSMMap(obj = TNdeleg.spdf, type = "osm", add = FALSE)
SymbolsMap(obj = TNdeleg.spdf, data = TNdeleg, datavar = "pop_t", 
           title = "Total Population", 
           pos = "left", add = TRUE, symbols = "circles", k = 0.2, 
           col = "#469372")
LayoutMap(title = "Population distribution in Tunisia, 2004", 
          sources = "INS - Tunisia, General census of population and housing 2004.",
          col = "#003F24", 
          txtcol = "white",
          author = "Timothée Giraud, 2015",scale = 100, frame = TRUE, 
          north = TRUE )

## ------------------------------------------------------------------------
data("FRdep")
# Selection of departments from the Midi-Pyrenées region
MP <- FRdep.spdf[FRdep.spdf@data$dep_code %in% 
                   c("09", "12", "31", "32", "46", "65", "81", "82" ),]
OSMMap(obj = MP, type = "stamen-watercolor", add = FALSE)
StaticMap(obj = MP, col = NA, border = "red", lwd = 1.25, add = TRUE)
SymbolsMap(obj = MP, data = FRdep, datavar = "ptota_2011", 
           title = "Total Population", pos = "topleft", add = TRUE, 
           symbols = "circles", k = 0.05, col = "#469372")
LabelMap(obj = MP, objid = "dep_code", data = FRdep, dataid = "dep_code",
         cex = 0.6, txt = "dep_name", col = "black")
LayoutMap(title = "Population Distribution in Midi-Pyrénées", 
          sources = "INSEE 2011", col = "#003F24", 
          txtcol = "white",
          author = "Timothée Giraud, 2015",scale = 100, frame = TRUE, 
          north = TRUE )


