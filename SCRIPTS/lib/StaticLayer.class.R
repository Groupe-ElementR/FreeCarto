# ----------------------------------------------
# CLASSE: StaticLayer
# DESCRIPTION: Display spatial dataframes
# AUTHOR(s): Nicolas LAMBERT / CNRS / UMS RIATE
# EMAIL(s): nicolas.lambert@ums-riate.fr
# LAST REVISION: oct 2014
# ----------------------------------------------

library ("maptools")

setClass (
  Class = "StaticLayer" ,
  slots = c(
    geom = "Spatial",   
    data = "data.frame",
    geomId = "character",
    dataId = "character",
    labelField = "character",
    labelSize = "numeric",
    labelCol = "character",
    col = "character", 
    border = "character",
    thickness = "numeric",
    add="logical"
  ),
  prototype=list(
    col = "#EFDEC1",
    border = "white", 
    thickness = 1,
    geomId="undefined",
    dataId="undefined",
    labelSize=0.5,
    labelCol="black",
    add=F
  )
)

setGeneric(
  name = "AddStaticLayer" ,
  def=function (object){ standardGeneric ("AddStaticLayer")
  }
)

setMethod("AddStaticLayer","StaticLayer",
          function (object){
            
            if (class(object@geom)=="SpatialPolygonsDataFrame")
            {plot(object@geom, border=object@border, col=object@col,lwd=object@thickness,add=object@add)}
            
            if (class(object@geom)=="SpatialLinesDataFrame")
            {plot(object@geom, col=object@col,lwd=object@thickness,add=object@add)}
            
            if (class(object@geom)=="SpatialPointsDataFrame")
            {plot(object@geom, col=object@col,lwd=object@thickness,pch=19,add=object@add)}
          }
)

setGeneric(
  name = "AddLabels" ,
  def=function (object){ standardGeneric ("AddLabels")
  }
)

setMethod("AddLabels","StaticLayer",
          function (object){
            
            if (object@geomId=="undefined"){object@geomId<-names(object@geom@data)[1]}
            if (object@dataId=="undefined"){object@dataId<-names(object@data)[1]}    
            
            
            if (class(object@geom)=="SpatialPolygonsDataFrame")
            {
              dots <- cbind(object@geom@data[,object@geomId],as.data.frame(coordinates(object@geom)))
              colnames(dots) <- c(object@geomId,"x","y")  
              dots = data.frame(dots, object@data[match(dots[,object@geomId], object@data[,object@dataId]),])
              dots <- dots[order(dots[,object@labelField],decreasing=TRUE),]
              text(dots$x, dots$y , labels = dots[,object@labelField],cex=object@labelSize,col=object@labelCol)
              
            }
            
            if (class(object@geom)=="SpatialLinesDataFrame")
            {}
            
            if (class(object@geom)=="SpatialPointsDataFrame")
            {}
            
          }
)
