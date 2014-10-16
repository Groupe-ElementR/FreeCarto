# ----------------------------------------------
# CLASSE : PropSymbols
# DESCRIPTION: Build thematic maps with proportional symbols (circles, squares, ...)
# AUTHOR(s) : Nicolas LAMBERT / CNRS / UMS RIATE
# EMAIL(s) : nicolas.lambert@ums-riate.fr
# LAST REVISION: oct 2014
# ----------------------------------------------


library ("maptools")

# TODO TO IMPROVE
# gerer les pbs d'extent
# ajouter une variable fixMax pour pouvoir comparer deux cartes
# positionnement des légendes (revoir le code)

setClass (
  Class = "PropSymbols" ,
  slots = c(
    geom = "Spatial", # basemap    
    data = "data.frame", # data
    geomId = "character", 
    dataId = "character",
    dataField = "character",
    col = "character",
    col2="character",
    breakVal="numeric",
    nbCols="numeric",
    type = "character",
    k="numeric",
    fixMax="logical",
    add="logical", # il y a un délire sur le add (les axes)
    size="numeric",
    var="numeric",
    legPos = "character",
    legTitle="character"
  ),
  prototype=list(
    type="circles",
    col="#E84923",
    col2="#7DC437",
    geomId="undefined",
    dataId="undefined",
    breakVal=0,
    k=0.2,
    title="Title",
    legPos="bottomleft",
    legTitle="",
    nbCols<-1,
    add=T,
    fixMax=F
  )
)

setGeneric(
  name = "AddPropSymbolsLayer" ,
  def=function (object){ standardGeneric ("AddPropSymbolsLayer")
  }
)

setMethod("AddPropSymbolsLayer","PropSymbols",
          function (object){
            
            nameObject <- deparse ( substitute ( object ))
            
            
            #object@type <- "squares"
            if (object@geomId=="undefined"){object@geomId<-names(object@geom@data)[1]}
            if (object@dataId=="undefined"){object@dataId<-names(object@data)[1]}    
            
            dots <- cbind(object@geom@data[,object@geomId],as.data.frame(coordinates(object@geom)))
            colnames(dots) <- c(object@geomId, "x", "y")  
            dots = data.frame(dots, object@data[match(dots[,object@geomId], object@data[,object@dataId]),])
            dots <- dots[order(dots[, object@dataField],decreasing = TRUE),]
            
            x1 <- bbox(object@geom)[1]
            y1 <- bbox(object@geom)[2]
            x2 <- bbox(object@geom)[3]
            y2 <- bbox(object@geom)[4]
            hfdc<-(x2-x1)
            sfdc <- (x2-x1)*(y2-y1)
            sc <- sum(dots[,object@dataField],na.rm=TRUE)
            
            if (object@fixMax==F){
              dots$circleSize <- sqrt((dots[,object@dataField] * object@k * sfdc / sc) / pi) # surface des cercles
              dots$squareSize <-  sqrt(dots[,object@dataField] * object@k * sfdc / sc) # surface des carrés
              dots$heightSize <- dots[,object@dataField]*object@k*hfdc/sc*10 # Hauteur des barres
            }
            
            if (object@fixMax==T){
              dots$circleSize <- sqrt((dots[,object@dataField]*object@k)/pi) # surface des cercles
              dots$squareSize <-  sqrt(dots[,object@dataField]*object@k) # surface des carrés
              dots$heightSize <- dots[,object@dataField]*object@k*10 # Hauteur des barres
            }
            
            dots$var2 <- ifelse(dots[,object@dataField]>=object@breakVal,"sup","inf")
            colours <- c(object@col,object@col2)
            dots$col <- as.factor(dots$var2)
            levels(dots$col) <- colours
            mycols <- as.character(dots$col)
            object@nbCols <- length(levels(as.factor(dots$var2)))
            
            # CIRCLES  
            if (object@type=="circles"){
              symbols(dots[,c("x","y")],circles=dots$circleSize,bg=mycols,add=object@add,inches=FALSE,asp=1,xlab="",ylab="")    
              object@size<- dots$circleSize
              object@var<- dots[,object@dataField]
            }
            
            
            # SQUARES
            if (object@type=="squares"){
              symbols(dots[,c("x","y")],squares=dots$squareSize,bg=mycols,add=object@add,inches=FALSE,asp=1,xlab="",ylab="")
              object@size<- dots$squareSize
              object@var<- dots[,object@dataField]
            }
            
            
            #BARRES    
            if (object@type=="height"){
              
              width<-min((par()$usr[4]-par()$usr[3])/40,(par()$usr[2]-par()$usr[1])/40)
              tmp<-as.matrix(data.frame(width,dots$heightSize))
              dots$y2<-dots$y+dots$heightSize/2
              symbols(dots[,c("x","y2")],rectangles=tmp,add=object@add,bg=mycols,inches=FALSE,asp=1,xlab="",ylab="")
              object@size<-dots$heightSize
              object@var<- dots[,object@dataField]
            } 
            
            
            # recuperation des infos utiles pour la légende
            assign ( nameObject , object , envir = parent.frame ())
#             return( invisible ())

            
          }
)

setGeneric(
  name = "AddPropSymbolsLegend" ,
  def=function (object){ standardGeneric ("AddPropSymbolsLegend")
  }
)




setMethod("AddPropSymbolsLegend","PropSymbols",function (object){
  
  ifelse(object@legTitle=="", legTitle<-object@dataField, legTitle<-object@legTitle)
  
  
  
  # position of le legend ---------------------------------
  x1 <- par()$usr[1] 
  x2 <- par()$usr[2]
  y1 <- par()$usr[3]
  y2 <- par()$usr[4]
  yextent<-(y2-y1)/3
  xextent<-(x2-x1)/3
  delta<-min((y2-y1)/40,(x2-x1)/40)
  #   coords<-data.frame(pos="topleft",x=x1,y=y2)
  #   coords<-rbind(coords,data.frame(pos="top",x=x1+xextent,y=y2))
  #   coords<-rbind(coords,data.frame(pos="topright",x=x1+xextent*2,y=y2))
  #   coords<-rbind(coords,data.frame(pos="left",x=x1,y=y2-yextent))
  #   coords<-rbind(coords,data.frame(pos="right",x=x1+xextent*2,y=y2-yextent))
  #   coords<-rbind(coords,data.frame(pos="bottomleft",x=x1,y=y1+yextent))
  #   coords<-rbind(coords,data.frame(pos="bottom",x=x1+xextent,y=y1+yextent))
  #   coords<-rbind(coords,data.frame(pos="bottomright",x=x1+xextent*2,y=y1+yextent))
  
  
  coords<-data.frame(pos="topleft",x=x1+delta/2,y=y2-delta/2)
  coords<-rbind(coords,data.frame(pos="top",x=x1+xextent+delta/2,y=y2-delta/2))
  coords<-rbind(coords,data.frame(pos="topright",x=x1+xextent*2+delta/2,y=y2-delta/2))
  coords<-rbind(coords,data.frame(pos="left",x=x1+delta/2,y=y2-yextent-delta/2))
  coords<-rbind(coords,data.frame(pos="right",x=x1+xextent*2+delta/2,y=y2-yextent-delta/2))
  coords<-rbind(coords,data.frame(pos="bottomleft",x=x1+delta/2,y=y1+yextent+delta/2))
  coords<-rbind(coords,data.frame(pos="bottom",x=x1+xextent+delta/2,y=y1+yextent+delta/2))
  coords<-rbind(coords,data.frame(pos="bottomright",x=x1+xextent*2+delta/2,y=y1+yextent+delta/2))
  
  #   rect(x1, y1, x2, y2,border = "blue")  
  #   rect(x1,y1,(x1+xextent),(y1+yextent),border="blue")
  #   rect((x1+xextent),y1,(x1+xextent*2),(y1+yextent),border="blue")
  #   rect(x2,y1,(x2-xextent),(y1+yextent),border="blue")
  #   rect(x1,(y1+yextent),(x1+xextent),(y1+yextent*2),border="blue")
  #   rect((x1+xextent),(y1+yextent),(x1+xextent*2),(y1+yextent*2),border="blue")
  #   rect(x2,(y1+yextent),(x2-xextent),(y1+yextent*2),border="blue")
  #   rect(x1,y2,(x1+xextent),(y2-yextent),border="blue")
  #   rect((x1+xextent),y2,(x1+xextent*2),(y2-yextent),border="blue")
  #   rect(x2,y2,(x2-xextent),(y2-yextent),border="blue")
  #   symbols(coords[coords$pos=="topleft","x"],coords[coords$pos=="topleft","y"],circles=5000,add=T,inches=FALSE,bg="red")
  #   symbols(coords[coords$pos=="top","x"],coords[coords$pos=="top","y"],circles=5000,add=T,inches=FALSE,bg="red")
  #   symbols(coords[coords$pos=="topright","x"],coords[coords$pos=="topright","y"],circles=5000,add=T,inches=FALSE,bg="red")
  #   symbols(coords[coords$pos=="bottomleft","x"],coords[coords$pos=="bottomleft","y"],circles=5000,add=T,inches=FALSE,bg="red")
  #   symbols(coords[coords$pos=="bottom","x"],coords[coords$pos=="bottom","y"],circles=5000,add=T,inches=FALSE,bg="red")
  #   symbols(coords[coords$pos=="bottomright","x"],coords[coords$pos=="bottomright","y"],circles=5000,add=T,inches=FALSE,bg="red")
  #   symbols(coords[coords$pos=="left","x"],coords[coords$pos=="left","y"],circles=5000,add=T,inches=FALSE,bg="red")
  #   symbols(coords[coords$pos=="right","x"],coords[coords$pos=="right","y"],circles=5000,add=T,inches=FALSE,bg="red")
  
  l<-NULL
  l$x<-coords[coords$pos==object@legPos,"x"]
  l$y<-coords[coords$pos==object@legPos,"y"]
  
  rLeg <- quantile(object@size,c(1,0.99,0.50,0),type=1,na.rm = TRUE)
  rVal <- quantile(object@var,c(1,0.99,0.50,0),type=1,na.rm = TRUE)
  
  colours<-c(object@col,object@col2)
  rVal2<-as.character(c(object@col,object@col2))
  
  # CIRCLES
  if(object@type=="circles"){
    text(x=l$x,y=l$y,legTitle,adj=c(0,1),cex=0.6)
    xpos<- (l$x+rLeg[1]-rLeg/2)
    ypos <-l$y+rLeg-rLeg[1]*2
    ypos<-ypos-strheight(legTitle,cex = 0.6)-delta
    symbols(x=rep(l$x+rLeg[1],4),y=ypos,circles=rLeg,add=T,bg=object@col,inches=FALSE)
    text(x=rep(l$x+rLeg[1],4)+rLeg[1]*1.2,y=(l$y+(2*rLeg)-rLeg[1]*2-delta-strheight(legTitle,cex = 0.6)),rVal,cex=0.3,srt=0,adj=0)
    for (i in 1:4){  segments (l$x+rLeg[1],(l$y+(2*rLeg[i])-rLeg[1]*2-delta-strheight(legTitle,cex = 0.6)),l$x+rLeg[1]+rLeg[1]*1.1,(l$y+(2*rLeg[i])-rLeg[1]*2-delta-strheight(legTitle,cex = 0.6)))}
    
    
    
    if (object@nbCols==2){
      tmp <- c ((x2-x1), (y2-y1))
      size<-max(tmp)/50
      symbols(x=rep(l$x+rLeg[1],4),y=ypos,circles=rLeg,add=T,bg="#CCCCCC",inches=FALSE)
      for (i in 1:4){  segments (l$x+rLeg[1],(l$y+(2*rLeg[i])-rLeg[1]*2-delta-strheight(legTitle,cex = 0.6)),l$x+rLeg[1]+rLeg[1]*1.1,(l$y+(2*rLeg[i])-rLeg[1]*2-delta-strheight(legTitle,cex = 0.6)))}
      rect(xpos[1]-rLeg[1]/2, ypos[1]-rLeg[1]-delta, xpos[1]-rLeg[1]/2+delta,  ypos[1]-rLeg[1]-delta*2,col=object@col)
      text(x=xpos[1]-rLeg[1]/2+delta*1.2,y=ypos[1]-rLeg[1]-delta-delta/4,paste("< ",object@breakVal),adj=c(0,1),cex=0.5)
      rect(xpos[1]-rLeg[1]/2, ypos[1]-rLeg[1]-delta*2, xpos[1]-rLeg[1]/2+delta,  ypos[1]-rLeg[1]-delta*3,col=object@col2)      
      text(x=xpos[1]-rLeg[1]/2+delta*1.2,y=ypos[1]-rLeg[1]-delta*2-delta/4,paste("> ",object@breakVal),adj=c(0,1),cex=0.5)      
      
    }
    
  }
  
  # SQUARES
  if(object@type=="squares"){
    text(x=l$x,y=l$y,legTitle,adj=c(0,1),cex=0.6) 
    xpos<- (l$x+rLeg[1]-rLeg/2)
    ypos <-l$y+rLeg/2-rLeg[1]
    ypos<-ypos-strheight(legTitle,cex = 0.6)-delta
    symbols(x=xpos,y=ypos,squares=rLeg,add=TRUE,bg=object@col,inches=FALSE)
    text(x=l$x+rLeg[1]*1.2,y=ypos+rLeg/2,rVal,cex=0.3,srt=0,adj=0)
    for (i in 1:4){  segments (l$x+rLeg[1],ypos+rLeg/2,l$x+rLeg[1]*1.1,ypos+rLeg/2)}
    
    
    if (object@nbCols==2){
      symbols(x=xpos,y=ypos,squares=rLeg,add=TRUE,bg="#CCCCCC",inches=FALSE)
      rect(xpos[1]-rLeg[1]/2, ypos[1]-rLeg[1]/2-delta, xpos[1]-rLeg[1]/2+delta, ypos[1]-rLeg[1]/2-delta*2,col=object@col)
      text(x=xpos[1]-rLeg[1]/2+delta*1.2,y=ypos[1]-rLeg[1]/2-delta-delta/4,paste("< ",object@breakVal),adj=c(0,1),cex=0.5)
      rect(xpos[1]-rLeg[1]/2, ypos[1]-rLeg[1]/2-delta*2, xpos[1]-rLeg[1]/2+delta,  ypos[1]-rLeg[1]/2-delta*3,col=object@col2)
      text(x=xpos[1]-rLeg[1]/2+delta*1.2,y=ypos[1]-rLeg[1]/2-delta*2-delta/4,paste("> ",object@breakVal),adj=c(0,1),cex=0.5)    
    }
  }
  
  # BARRES
  if(object@type=="height"){
    text(x=l$x,y=l$y,legTitle,adj=c(0,1),cex=0.6)
    #tmp <- c ((bbox(object@geom)[3]-bbox(object@geom)[1]), (bbox(object@geom)[4]-bbox(object@geom)[2]))
    width<-delta
    tmp<-as.matrix(data.frame(width,rLeg))   
    symbols(x=rep(l$x+width/2,4),y=l$y+rLeg/2-rLeg[1]-strheight(legTitle,cex = 0.6)-delta,rectangles=tmp,add=TRUE,bg=object@col,inches=FALSE)
    for (i in 1:4){  segments (l$x+width,l$y+rLeg[i]-rLeg[1]-strheight(legTitle,cex = 0.6)-delta,l$x+width*2,l$y+rLeg[i]-rLeg[1]-strheight(legTitle,cex = 0.6)-delta)}
    text(x=l$x+2*width*1.2,y=l$y+rLeg-rLeg[1]-strheight(legTitle,cex = 0.6)-delta,rVal,cex=0.3,srt=0,adj=0)
    
    
    if (object@nbCols==2){
      
      symbols(x=rep(l$x+width/2,4),y=l$y+rLeg/2-rLeg[1]-strheight(legTitle,cex = 0.6)-delta,rectangles=tmp,add=TRUE,bg="#CCCCCC",inches=FALSE)
      rect(l$x, l$y-rLeg[1]-delta*2-strheight(legTitle,cex = 0.6), l$x+delta, l$y-delta*3-rLeg[1]-strheight(legTitle,cex = 0.6),col=object@col)
      text(x=l$x+delta*1.2,y=l$y-rLeg[1]-delta*2-delta/4-strheight(legTitle,cex = 0.6),paste("< ",object@breakVal),adj=c(0,1),cex=0.5)
      rect(l$x, l$y-rLeg[1]-delta*3-strheight(legTitle,cex = 0.6), l$x+delta, l$y-delta*4-rLeg[1]-strheight(legTitle,cex = 0.6),col=object@col2)
      text(x=l$x+delta*1.2,y=l$y-rLeg[1]-delta*3-delta/4-strheight(legTitle,cex = 0.6),paste("> ",object@breakVal),adj=c(0,1),cex=0.5)
      
    }
    
    
  }
  
  
  
  
}
)
