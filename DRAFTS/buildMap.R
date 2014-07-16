setwd("/home/nlambert/Documents/R/R_neocarto")
source("pgm/cartography/PropSymbols.class.R")
source("pgm/cartography/StaticLayer.class.R")

delegations<-new(Class = "StaticLayer",add=F)
delegations@geom <- readShapeSpatial("data/TUN/shp/Tunisie_snuts4.shp")
delegations@col<- "#CCCCCC"
AddStaticLayer(delegations)

myMap<-new(Class = "PropSymbolsLayer",add=T)
myMap@geom <- readShapeSpatial("data/TUN/shp/Tunisie_snuts4_centres.shp")
myMap@data<-read.csv( "data/TUN/csv/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",encoding="latin1",)
myMap@dataField <- "SUP2010"
myMap@type <- "squares"
AddPropSymbolsLayer(myMap)

myMap<-new(Class = "PropSymbolsLayer",add=T)
myMap@geom <- readShapeSpatial("data/TUN/shp/Tunisie_snuts4_centres.shp")
myMap@data<-read.csv( "data/TUN/csv/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",encoding="latin1",)
myMap@dataField <- "SUP2010"
myMap@col<-"blue"
myMap@type<-"height"
myMap@k<-1
AddPropSymbolsLayer(myMap)


myMap<-new(Class = "PropSymbolsLayer",add=F)
myMap@geom <- readShapeSpatial("data/TUN/shp/Tunisie_snuts4_centres.shp")
myMap@data<-read.csv( "data/TUN/csv/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",encoding="latin1",)
myMap@dataField <- "SUP2010"
AddPropSymbolsLayer(myMap)

coast<-new(Class = "StaticLayer",add=T)
coast@geom <- readShapeSpatial("data/TUN/shp/coast.shp")
AddStaticLayer(coast)

