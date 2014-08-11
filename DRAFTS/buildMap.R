setwd("/home/nlambert/Documents/R/FreeCarto")
source("DRAFTS/PropSymbols.class.R")
source("DRAFTS/StaticLayer.class.R")

delegations<-new(Class = "StaticLayer",add=F)
delegations@geom <- readShapeSpatial("DATA/TUN/Tunisie_snuts4.shp")
delegations@col<- "#CCCCCC"
AddStaticLayer(delegations)

myMap<-new(Class = "PropSymbols",add=T)
myMap@geom <- readShapeSpatial("DATA/TUN/Tunisie_snuts4.shp")
myMap@data<-read.csv( "DATA/TUN/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",encoding="latin1",)
myMap@dataField <- "SUP2010"
myMap@type <- "squares"
AddPropSymbolsLayer(myMap)
AddPropSymbolsLegend(myMap)