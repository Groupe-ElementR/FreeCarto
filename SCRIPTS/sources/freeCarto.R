# 1. Sources

source("/mnt/data/depot/FreeCarto/SCRIPTS/lib/PropSymbols.class.R")
source("/mnt/data/depot/FreeCarto/SCRIPTS/lib/PropSymbolsDuo.class.R")
source("/mnt/data/depot/FreeCarto/SCRIPTS/lib/StaticLayer.class.R")
source("/mnt/data/depot/FreeCarto/SCRIPTS/lib/MapLayout.class.R")
source("/mnt/data/depot/FreeCarto/SCRIPTS/lib/OSMLayer.class.R")
# source("lib/LinkLayer.class.R")
# source("lib/FITLayer.class.R")

# 2. Constructeurs

OSMMap <- function(obj, type, zoom, add){
  map <- new(Class = "OSMLayer")
  map@geom <- obj
  if(missing(add)){} else {map@add <- add}
  if(missing(type)){} else {map@type <- type}
  if(missing(zoom)){} else {map@zoom <- zoom}
  AddOSMLayer(map)
}

LayoutMap <- function(title, sources, author, scale, frame, extent, north){
  map <- new(Class = "MapLayout")
  if(missing(title)){} else {map@mapTitle <- title}
  if(missing(sources)){} else {map@mapSource <- sources}
  if(missing(author)){} else {map@mapAuthor <- author}
  if(missing(scale)){} else {map@scaleSize <- scale}
  if(missing(frame)){} else {map@mapFrame <- frame}
  if(missing(extent)){} else {map@mapExtent <- extent}
  if(missing(north)){} else {map@north <- north}
  AddLayout(map)
}

StaticMap <- function(obj, col, border, lwd, add){
  map <- new(Class = "StaticLayer")
  map@geom <- obj
  if (missing (col)){} else {map@col <- col}
  if (missing (border)){} else {map@border <- border}
  if (missing (lwd)){} else {map@thickness <- lwd}
  if (missing (add)){} else {map@add <- add}
  AddStaticLayer(object = map)
}


SymbolsMap <- function(obj, data, objid, dataid, datavar, symbols, col, col2, breakval, k, fixmax, pos, title, add){
  map <- new(Class = "PropSymbols")
  map@geom <- obj
  map@data <- data
  map@dataField <- datavar
  
  if (missing (objid)){} else {map@geomId <- objid}
  if (missing (dataid)){} else {map@dataId <- dataid}
  if (missing (col)){} else {map@col <- col}
  if (missing (col2)){} else {map@col2 <- col2}
  if (missing (breakval)){} else {map@breakVal <- breakval}
  if (missing (symbols)){} else {map@type <- symbols}
  if (missing (k)){} else {map@k <- k}
  if (missing (fixmax)){} else {map@fixMax <- fixmax}
  if (missing (add)){} else {map@add <- add}
  if (missing (pos)){} else {map@legPos <- pos}
  if (missing (title)){} else {map@legTitle <- title}
  
  AddPropSymbolsLayer(map)
  AddPropSymbolsLegend(map)
}

SymbolsDuoMap <- function(obj, data, objid, dataid, datavar, datavar2, col, col2, k){
  map <- new(Class = "PropSymbolsDuo")
  map@geom <- obj
  map@data <- data
  map@dataField1 <- datavar
  map@dataField2 <- datavar2
  if (missing (objid)){} else {map@geomId <- objid}
  if (missing (dataid)){} else {map@dataId <- dataid}
  if (missing (col)){} else {map@col <- col}
  if (missing (col2)){} else {map@col2 <- col2}
  if (missing (k)){} else {map@k <- k}
  AddPropSymbolsDuo(map)
}



