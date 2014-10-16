library ("OpenStreetMap")

setClass (
  Class = "OSMLayer" ,
  slots = c(
    geom = "Spatial",
    type = "character", 
    add="logical",
    zoom="numeric"
  ),
  prototype=list(
    type = "osm",
    add = F,
    zoom = 12345678
  )
)



setGeneric(
  name = "AddOSMLayer" ,
  def=function (object){ standardGeneric ("AddOSMLayer")
  }
)



setMethod("AddOSMLayer","OSMLayer",
          function (object){
            if (is.na(object@geom@proj4string)){
              cat("The Spatial object must contain information on its projection.")
            } else {
              bboxGeom <- object@geom@bbox
              bboxGeomSP <- SpatialPoints(coords = data.frame(x = c(bboxGeom[1,1],bboxGeom[1,2] ), 
                                                              y = c(bboxGeom[2,2],bboxGeom[2,1])), 
                                          proj4string = object@geom@proj4string)
              latlon <- CRS("+proj=longlat +datum=WGS84")
              bboxGeomSP <- spTransform(x = bboxGeomSP, CRSobj = latlon)
              bboxGeomSP <- bboxGeomSP@bbox
              bboxGeomSP[2,2] <- bboxGeomSP[2,2] + 0.1 * (bboxGeomSP[2,2] - bboxGeomSP[2,1]) 
              bboxGeomSP[1,1] <- bboxGeomSP[1,1] - 0.1 * (bboxGeomSP[1,2] - bboxGeomSP[1,1])
              bboxGeomSP[2,1] <- bboxGeomSP[2,1] - 0.1 * (bboxGeomSP[2,2] - bboxGeomSP[2,1]) 
              bboxGeomSP[1,2] <- bboxGeomSP[1,2] + 0.1 * (bboxGeomSP[1,2] - bboxGeomSP[1,1])
              if(object@zoom == 12345678){
                tempOSM <- openmap(upperLeft = c(bboxGeomSP[2,2],bboxGeomSP[1,1]), 
                                   lowerRight = c(bboxGeomSP[2,1],bboxGeomSP[1,2]),
                                   type = object@type)
              } else {
                tempOSM <- openmap(upperLeft = c(bboxGeomSP[2,2],bboxGeomSP[1,1]), zoom = object@zoom,
                                   lowerRight = c(bboxGeomSP[2,1],bboxGeomSP[1,2]),
                                   type = object@type)
              }
              finalOSM <- openproj(x = tempOSM, projection = object@geom@proj4string)
              plot(finalOSM, add = object@add, removeMargin = F)
            }
          }
)
