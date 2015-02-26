

library(RColorBrewer)
library(classInt)


setClass (
  Class = "Choropleth" ,
  slots = c(
    geom = "Spatial", 
    data = "data.frame", 
    geomId = "character", 
    dataId = "character",
    dataField = "character",
    style = "character",
    nclass = "numeric"
  ),
  prototype = list(
    geomId = "undefined",
    dataId = "undefined"
  )
)



setGeneric(
  name = "AddChoroplethLayer" ,
  def=function (object){ 
    standardGeneric ("AddChoroplethLayer")
  }
)

setMethod("AddChoroplethLayer","Choropleth",
          function (object){
            #             nameObject <- deparse ( substitute ( object ))
            if (object@geomId == "undefined"){object@geomId <- names(object@geom@data)[1]}
            if (object@dataId == "undefined"){object@dataId <- names(object@data)[1]}
             
            object@geom@data <- data.frame(object@geom@data, 
                                           object@data[match(object@geom@data[, object@geomId], 
                                                             object@data[, object@dataId ]), ]
            )
            distr <- classIntervals(object@geom@data[,object@dataField],object@nclass,style=object@style)$brks
            #             colours <- c(brewer.pal(9,"YlOrRd"))
            colours <- c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026")
            colMap <- colours[(findInterval(object@geom@data[,object@dataField],distr,all.inside=TRUE))]
            plot(object@geom, col=colMap)
          }
)

?setClass
prototype(map)
ChoroplethMap <- function(obj, data, objid, dataid, datavar, nclass = 9, style = "quantile"){
  map <- new(Class = "Choropleth")
  map@geom <- obj
  map@data <- data
  map@dataField <- datavar
  if (missing (objid)){} else {map@geomId <- objid}
  if (missing (dataid)){} else {map@dataId <- dataid}
  map@nclass <- nclass
  map@style <- style
  AddChoroplethLayer(map)
}

geomTunisie <- readOGR(dsn ="data/",layer = "Tunisie_snuts4" )
dataTunisie <- read.csv( "data/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",encoding="latin1",)

ChoroplethMap(obj = geomTunisie, data = dataTunisie, datavar = "SUP2010", nclass = 6)

