#### Class Definition
#' Class StaticLayer.
#'
#' Class StaticLayer defines a static layer, not related to statistical information.
#' @name StaticLayer-class
#' @rdname StaticLayer-class
#' @exportClass StaticLayer
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
  )
)

#### Methods declaration
#' Method AddStaticLayer
#' @name AddStaticLayer
#' @rdname AddStaticLayer-method
#' @aliases AddStaticLayer,StaticLayer-method
#' @param object Object of class StaticLayer
#' @exportMethod AddStaticLayer
#' @docType methods
setGeneric(
  name = "AddStaticLayer" ,
  def=function (object){ standardGeneric ("AddStaticLayer")
  }
)

#' Method AddLabels
#' @name AddLabels
#' @aliases AddLabels,StaticLayer-method
#' @rdname AddLabels-method
#' @param object Object of class StaticLayer
#' @exportMethod AddLabels
#' @docType methods
setGeneric(
  name = "AddLabels" ,
  def=function (object){ standardGeneric ("AddLabels")
  }
)



#### Methods creation
#' @rdname AddStaticLayer-method
#' @aliases AddStaticLayer,StaticLayer,StaticLayer-method
#' @docType methods
#' @import sp
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


# #' @rdname AddLabels-method
#' @aliases AddLabels,StaticLayer,StaticLayer-method
#' @import sp
#' @docType methods
setMethod("AddLabels","StaticLayer",
          function (object){
            if (class(object@geom)=="SpatialPolygonsDataFrame")
            {
              dots <- cbind(object@geom@data[,object@geomId],as.data.frame(coordinates(object@geom)))
              colnames(dots) <- c(object@geomId,"x","y")
              print(object@geomId)
              dots <- data.frame(dots, object@data[match(dots[,object@geomId], object@data[,object@dataId]),])
              dots <- dots[order(dots[,object@labelField],decreasing=TRUE),]
              text(dots$x, dots$y , labels = dots[,object@labelField],cex=object@labelSize,col=object@labelCol)

            }

            if (class(object@geom)=="SpatialLinesDataFrame")
            {}

            if (class(object@geom)=="SpatialPointsDataFrame")
            {
              dots <- cbind(object@geom@data[,object@geomId],as.data.frame(coordinates(object@geom)))
              colnames(dots) <- c(object@geomId,"x","y")
              dots <- data.frame(dots, object@data[match(dots[,object@geomId], object@data[,object@dataId]),])
              dots <- dots[order(dots[,object@labelField],decreasing=TRUE),]
              text(dots$x, dots$y , labels = dots[,object@labelField],cex=object@labelSize,col=object@labelCol)
            }

          }
)



#### Methods wrappers
#' StaticMap function.
#'
#' @name StaticMap
#' @param obj Spatial*DataFrame
#' @param col Filling color
#' @param border Border color
#' @param lwd Border thickness
#' @param add Whether to add the layer to an existing map (TRUE) or not (FALSE)
#' @export
#' @examples
#' data("TNdeleg")
#' StaticMap(obj = TNdeleg.spdf)
#' data("FRdep")
#' StaticMap(obj = FRdep.spdf, col = "black", border = "red", lwd = 0.75, add = FALSE)
StaticMap <- function(obj, col = "#EFDEC1", border = "black", lwd = 1, add = FALSE){
  map <- new(Class = "StaticLayer")
  if (is.na(col)){col <- NA_character_}
  if (is.na(border)){border <- NA_character_}
  map@geom <- obj
  map@col <- col
  map@border <- border
  map@thickness <- lwd
  map@add <- add
  AddStaticLayer(object = map)
}


#' StaticMap function.
#'
#' @name LabelMap
#' @param obj Spatial*DataFrame
#' @param data DataFrame with Ids and Labels
#' @param objid Ids of the obj Spatial*DataFrame
#' @param dataid Ids of the DataFrame
#' @param txt Labels field in data
#' @param col Labels color
#' @param cex Labels size
#' @export
#' @examples
#' data("TNdeleg")
#' StaticMap(obj = TNdeleg.spdf)
#' LabelMap(obj = TNdeleg.spdf, data = TNdeleg)
#'
#' data("FRdep")
#' StaticMap(obj = FRdep.spdf, col = "black", border = "red", lwd = 0.75, add = FALSE)
#' LabelMap(obj = FRdep.spdf, objid = "dep_code", data = FRdep, dataid = "dep_code",
#'          cex = 0.6, txt = "dep_name", col = "green")
LabelMap <- function(obj, data, objid = NA, dataid = NA, txt, col = "black", cex = 0.7){
  map <- new(Class = "StaticLayer")
  map@geom <- obj
  map@data <- data
  if (is.na(objid)){map@geomId <- names(map@geom@data)[1]}else{map@geomId <- objid}
  if (is.na(dataid)){map@dataId<-names(map@data)[1]}else{map@dataId <- dataid}
  if (missing(txt)){map@labelField <-map@geomId}else{map@labelField <- txt}
  map@labelCol <- col
  map@labelSize <- cex
  AddLabels(object = map)
}
