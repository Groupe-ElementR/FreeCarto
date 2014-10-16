# ----------------------------------------------
# CLASSE: MapLayout
# DESCRIPTION: Layout of the map
# AUTHOR(s): Nicolas LAMBERT / CNRS / UMS RIATE
# EMAIL(s): nicolas.lambert@ums-riate.fr
# LAST REVISION: oct 2014
# ----------------------------------------------


setClass (
  Class = "MapLayout" ,
  slots = c(
    mapTitle = "character",
    mapSource = "character",
    mapAuthor = "character",
    scaleSize="numeric",
    mapFrame="logical",
    mapExtent="numeric",
    north="logical"
  ),
  prototype=list(
    scaleSize=0,
    mapAuthor="Author(s)",
    mapSource="Source(s)",
    mapTitle = "Title of the map, year",
    mapFrame=T,
    mapExtent = 0,
    north=F
  )
)


setGeneric(
  name = "AddLayout" ,
  def=function (object){ standardGeneric ("AddLayout")
  }
)

setMethod("AddLayout","MapLayout",
          function (object){
            
            
            # INIT
            if(object@mapExtent == 0){object@mapExtent <- par()$usr}
            
            x1 <- object@mapExtent[1]; x2 <- object@mapExtent[2]; y1 <- object@mapExtent[3]; y2 <- object@mapExtent[4]
            yextent<-(y2-y1)/3; xextent<-(x2-x1)/3; delta<-min((y2-y1)/40,(x2-x1)/40)
            
            # SCALE
            if(object@scaleSize==0){scalesize<-(x2-x1)/10; scalesize<-signif(scalesize, digits = 0)}
            if(object@scaleSize!=0){scalesize<-object@scaleSize*1000}
            labelscale<-paste(scalesize/1000,"km",sep=" ")
            rect(x2-scalesize-delta/2, y1+delta, x2-delta/2, y1+(y2-y1)/200+delta/2, col = "black", border = "black")
            rect(x2-scalesize-delta/2, y1+delta, x2-delta/2-scalesize/2, y1+(y2-y1)/200+delta/2, col = "white", border = "black")
            rect(x2-scalesize-delta/2, y1+delta, x2-delta/2-scalesize+scalesize/4, y1+(y2-y1)/200+delta/2, col = "black", border = "black")
            rect(x2-scalesize/4-delta/2, y1+delta, x2-delta/2, y1+(y2-y1)/200+delta/2, col = "white", border = "black")
            text(x2-scalesize/2-delta/2,y1+(y2-y1)/200+delta,paste(labelscale,"\n",sep=""),cex=0.6)
            
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
            rect(x1, y2, x2, y2+delta+strheight(object@mapTitle,cex = size),border = "black",col="black")
            mapTitle<-text(x1+delta/2,y2+delta/2,object@mapTitle,adj=c(0,0),cex=size,col="white",font=2)
            par(xpd=FALSE)
            
            
            # SOURCES
            text(x1+delta/2,y1+delta/2,paste(object@mapSource,object@mapAuthor,sep="\n"),adj=c(0,0),cex=0.6,font=3)    
            
            # FRAME
            if(object@mapFrame==T){rect(x1, y1, x2, y2,border = "black")  }

            
          }
)
