#### Class Definition
#' Class MapLayout.
#'
#' Class Maplayout defines a layout around the map.
#' @name MapLayout-class
#' @rdname MapLayout-class
#' @exportClass MapLayout
setClass (
  Class = "MapLayout" ,
  slots = c(
    mapTitle = "character",
    mapSource = "character",
    mapAuthor = "character",
    scaleSize = "numeric",
    mapFrame = "logical",
    col = "character",
    txtcol = "character",
    north = "logical"
  )
)

#### Methods declaration
#' Method AddLayout
#' @name AddLayout
#' @rdname AddLayout-method
#' @param object Object of class MapLayout
#' @exportMethod AddLayout
#' @docType methods
setGeneric(
  name = "AddLayout" ,
  def=function (object){ standardGeneric ("AddLayout")
  }
)

#### Methods creation
#' @rdname AddLayout-method
#' @docType methods
setMethod("AddLayout","MapLayout",
          function (object){
            # INIT
            mapExtent <- par()$usr
            x1 <- mapExtent[1]
            x2 <- mapExtent[2]
            y1 <- mapExtent[3]
            y2 <- mapExtent[4]
            yextent<-(y2-y1)/3
            xextent<-(x2-x1)/3
            delta<-min((y2-y1)/40,(x2-x1)/40)

            # SCALE
            if(object@scaleSize==0){
              scalesize <- (x2-x1)/10
              scalesize <- signif(scalesize, digits = 0)
            }
            if(object@scaleSize!=0){scalesize<-object@scaleSize*1000}
            labelscale <- paste(scalesize/1000,"km",sep=" ")
            rect(x2 - scalesize - delta/2, y1+delta, x2-delta/2, y1+(y2-y1)/200+delta/2, col = "black", border = "black")
            rect(x2 - scalesize - delta/2, y1+delta, x2-delta/2-scalesize/2, y1+(y2-y1)/200+delta/2, col = "white", border = "black")
            rect(x2 - scalesize - delta/2, y1+delta, x2-delta/2-scalesize+scalesize/4, y1+(y2-y1)/200+delta/2, col = "black", border = "black")
            rect(x2 - scalesize / 4 - delta/2, y1+delta, x2-delta/2, y1+(y2-y1)/200+delta/2, col = "white", border = "black")
            text(x2 - scalesize / 2 - delta/2,y1+(y2-y1)/200+delta,paste(labelscale,"\n",sep=""),cex=0.6)

            # NORTH
            if(object@north==T){
              xarrow<-x2-delta*1.5;yarrow<- y2-delta*2
              xx <- c(xarrow,xarrow+delta/2,xarrow+delta*1) ; yy <- c(yarrow,yarrow+delta*1.5,yarrow)
              polygon(xx, yy, col = "#DDDDDD", border = "#DDDDDD")
              text(xarrow+delta*.5,yarrow,"N",adj=c(0.5,1.5),cex=0.8,font=2,col="#DDDDDD")
            }

            # TITLE
            size<-0.8
            par(xpd=TRUE)
            rect(x1, y2, x2, y2+delta+strheight(object@mapTitle,cex = size),border = object@col, col = object@col)
            mapTitle<-text(x1+delta/2,y2+delta/2,object@mapTitle,adj=c(0,0),cex=size, col = object@txtcol,font=2)
            par(xpd=FALSE)


            # SOURCES
            text(x1+delta/2,y1+delta/2,paste(object@mapSource,object@mapAuthor,sep="\n"),adj=c(0,0),cex=0.6,font=3)

            # FRAME
            if(object@mapFrame==T){rect(x1, y1, x2, y2,border = object@col)  }


          }
)




#### Methods wrappers
#' LayoutMap function.
#'
#'
#'
#' @details Must only be called after a plot
#' @name LayoutMap
#' @param title Title of the map
#' @param sources Sources of the map
#' @param author Author of the map
#' @param scale Size of the scale
#' @param frame Whether displaying a frame or not
#' @param col Color of the frame
#' @param txtcol Color of the Title
#' @param north Noth arrow
#' @export
#' @examples
#' data("TNdeleg")
#'StaticMap(obj = TNdeleg.spdf)
#'LabelMap(obj = TNdeleg.spdf, data = TNdeleg, cex = 0.4)
#'LayoutMap(title = "Hell Yeah!", sources = "Sources Inconnues",
#'          author = "Mister T",scale = 150, frame = FALSE, north = TRUE )

LayoutMap <- function(title = "Title of the map, year",
                      sources = "Source(s)", author = "Author(s)",
                      col = "black", txtcol = "white",
                      scale = 0, frame = TRUE, north = FALSE){
  map <- new(Class = "MapLayout")
  map@mapTitle <- title
  map@mapSource <- sources
  map@mapAuthor <- author
  map@scaleSize <- scale
  map@mapFrame <- frame
  map@north <- north
  map@col <- as.character(col)
  map@txtcol <- as.character(txtcol)
  AddLayout(map)
}
