library ("maptools")

setClass (
  Class = "StaticLayer" ,
  slots = c(
    geom = "Spatial",    
    col = "character", 
    border = "character",
    thickness = "numeric",
    add="logical"
  ),
  prototype=list(
    col = "blue",
    border = "white", 
    thickness = 1,
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
