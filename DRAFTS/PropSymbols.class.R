library ("maptools")

setClass (
  Class = "PropSymbolsLayer" ,
  slots = c(
    geom = "Spatial",    
    data = "data.frame",
    geomId = "character",
    dataId = "character",
    dataField = "character",
    col = "character",
    type = "character",
    k="numeric",
    add="logical"
  ),
  prototype=list(
    type="circles",
    col="red",
    geomId="undefined",
    dataId="undefined",
    k=0.2,
    add=F
  )
)

setGeneric(
  name = "AddPropSymbolsLayer" ,
  def=function (object){ standardGeneric ("AddPropSymbolsLayer")
  }
)

setMethod("AddPropSymbolsLayer","PropSymbolsLayer",
  function (object){

  if (object@geomId=="undefined"){object@geomId<-names(object@geom@data)[1]}
  if (object@dataId=="undefined"){object@dataId<-names(object@data)[1]}    
  
  k2<-30 # coef à sortir

  dots <- cbind(object@geom@data[,object@geomId],as.data.frame(coordinates(object@geom)))
  colnames(dots) <- c(object@geomId,"x","y")  
  dots = data.frame(dots, object@data[match(dots[,object@geomId], object@data[,object@dataId]),])
  dots <- dots[order(dots[,object@dataField],decreasing=TRUE),]
  
  x1 <- bbox(object@geom)[1]
  y1 <- bbox(object@geom)[2]
  x2 <- bbox(object@geom)[3]
  y2 <- bbox(object@geom)[4]
  hfdc<-(x2-x1)
  sfdc <- (x2-x1)*(y2-y1)
  sc <- sum(dots[,object@dataField],na.rm=TRUE)

  dots$circleSize <- sqrt((dots[,object@dataField]*object@k*sfdc/sc)/pi) # surface des cercles
  dots$squareSize <-  sqrt(dots[,object@dataField]*object@k*sfdc/sc) # surface des carrés
  dots$heightSize <- dots[,object@dataField]*object@k*hfdc/sc*10 # Hauteur des barres

  if (object@type=="squares"){symbols(dots[,c("x","y")],squares=dots$squareSize,bg=object@col,add=object@add,inches=FALSE,asp=1,axes=FALSE,xlab="",ylab="")}
  if (object@type=="circles"){symbols(dots[,c("x","y")],circles=dots$circleSize,bg=object@col,add=object@add,inches=FALSE,asp=1,axes=FALSE,xlab="",ylab="")}
  if (object@type=="height"){
    width<-(x2-x1)/30
    tmp<-as.matrix(data.frame(width,dots$heightSize))
    dots$y2<-dots$y+dots$heightSize/2
    symbols(dots[,c("x","y2")],rectangles=tmp,add=object@add,bg=object@col,inches=FALSE,asp=1,axes=FALSE,xlab="",ylab="")
  }  
  return(max(dots$circleSize))
}
)
