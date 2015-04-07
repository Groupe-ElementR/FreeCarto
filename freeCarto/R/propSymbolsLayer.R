#' @title Proportional Symbols Layer
#' @name propSymbolsLayer
#' @param spdf Spatial*DataFrame
#' @param df DataFrame with Ids and Labels
#' @param spdfid Ids of the obj Spatial*DataFrame
#' @param dfid Ids of the DataFrame
#' @param var Symbols variable
#' @param symbols Type of symbol ("circles", "squares", "height")
#' @param col Symbols color
#' @param col2 Symbols color if the break value (\code{breakval})is relevant
#' @param breakval Breaking value if 2 colors are needed
#' @param k Share of the map occupied by symbols
#' @param fixmax Whether the maximum value is fixed or not
#' @param pos Position of the legend
#' @param title Title of the legend
#' @param add Whether to add the layer to an existing map (TRUE) or not (FALSE)
#' @export
#' @import sp
#' @examples
#' data("TNdeleg")
propSymbolsLayer <- function(spdf, df, spdfid = NA, dfid = NA, var, symbols = "circles",
                             col="#E84923", col2="#7DC437", breakval = 0, k = 0.2, fixmax = FALSE,
                             pos = "bottomleft", title = var, add = TRUE){
  if (is.na(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.na(dfid)){dfid <- names(df)[1]}
  dots <- cbind(spdf@data[,spdfid], as.data.frame(sp::coordinates(spdf)))
  colnames(dots) <- c(spdfid, "x", "y")
  dots <- data.frame(dots, df[match(dots[,spdfid], df[,dfid]),])
  dots <- dots[order(dots[, var], decreasing = TRUE),]

  x1 <- sp::bbox(spdf)[1]
  y1 <- sp::bbox(spdf)[2]
  x2 <- sp::bbox(spdf)[3]
  y2 <- sp::bbox(spdf)[4]
  hfdc <- (x2-x1)
  sfdc <- (x2-x1)*(y2-y1)
  sc <- sum(dots[,var],na.rm=TRUE)

  if (fixmax == FALSE){
    dots$circleSize <- sqrt((dots[,var] * k * sfdc / sc) / pi) # surface des cercles
    dots$squareSize <-  sqrt(dots[,var] * k * sfdc / sc) # surface des carrés
    dots$heightSize <- dots[,var] * k * hfdc / sc * 10 # Hauteur des barres
  }

  if (fixmax == TRUE){
    dots$circleSize <- sqrt((dots[, var] * k) / pi) # surface des cercles
    dots$squareSize <-  sqrt(dots[, var] * k ) # surface des carrés
    dots$heightSize <- dots[, var] * k * 10 # Hauteur des barres
  }

  dots$var2 <- ifelse(dots[, var] >= breakval,"sup","inf")
  colours <- c(col, col2)
  dots$col <- as.factor(dots$var2)
  levels(dots$col) <- colours
  mycols <- as.character(dots$col)
  nbCols <- length(levels(as.factor(dots$var2)))

  # CIRCLES
  if (symbols == "circles"){
    symbols(dots[, c("x", "y")], circles = dots$circleSize, bg = mycols, add = add,
            inches = FALSE, asp = 1, xlab = "", ylab = "")
    sizevect <- dots$circleSize
    varvect <- dots[,var]
  }
  ?plot.window

  # SQUARES
  if (symbols == "squares"){
    symbols(dots[, c("x", "y")], squares = dots$squareSize, bg = mycols, add = add, inches = FALSE, asp = 1, xlab = "", ylab = "")
    sizevect <- dots$squareSize
    varvect <- dots[,var]
  }

  #BARRES
  if (symbols == "height"){
    width<-min((par()$usr[4]-par()$usr[3])/40,(par()$usr[2]-par()$usr[1])/40)
    tmp <- as.matrix(data.frame(width,dots$heightSize))
    dots$y2 <- dots$y+dots$heightSize/2
    symbols(dots[,c("x","y2")], rectangles = tmp, add = add, bg = mycols, inches = FALSE, asp = 1, xlab = "", ylab = "")
    sizevect <- dots$heightSize
    varvect<- dots[,var]
  }

  propSymbolsLegendLayer(pos = pos, title = title, varname = var,
                         sizevect = sizevect, varvect = varvect,
                         col = col, col2 = col2, symbols = symbols,
                         nbCols = nbCols, breakval = breakval)


}

propSymbolsLegendLayer <- function(pos = pos, title = title, varname = var,
                                   sizevect = sizevect, varvect = varvect,
                                   col = col, col2 = col2, symbols = symbols,
                                   nbCols = nbCols, breakval = breakval){

  if(is.null(title)){
    title <- varname
  }

  # position of le legend ---------------------------------
  x1 <- par()$usr[1]
  x2 <- par()$usr[2]
  y1 <- par()$usr[3]
  y2 <- par()$usr[4]
  yextent <- (y2 - y1) / 3
  xextent <- (x2 - x1) / 3
  delta <- min((y2 - y1) / 40, (x2 - x1) / 40)

  coords <- data.frame(pos="topleft",x=x1+delta/2,y=y2-delta/2)
  coords <- rbind(coords,data.frame(pos="top",
                                    x=x1+xextent+delta/2,y=y2-delta/2))
  coords <- rbind(coords,data.frame(pos="topright",
                                    x=x1+xextent*2+delta/2,y=y2-delta/2))
  coords <- rbind(coords,data.frame(pos="left",
                                    x=x1+delta/2,y=y2-yextent-delta/2))
  coords <- rbind(coords,data.frame(pos="right",
                                    x=x1+xextent*2+delta/2,y=y2-yextent-delta/2))
  coords <- rbind(coords,data.frame(pos="bottomleft",
                                    x=x1+delta/2,y=y1+yextent+delta/2))
  coords <- rbind(coords,data.frame(pos="bottom",
                                    x=x1+xextent+delta/2,y=y1+yextent+delta/2))
  coords <- rbind(coords,data.frame(pos="bottomright",
                                    x=x1+xextent*2+delta/2,y=y1+yextent+delta/2))

  l<-NULL
  l$x <- coords[coords$pos == pos, "x"]
  l$y <- coords[coords$pos == pos, "y"]

  rLeg <- quantile(sizevect,c(1,0.90,0.50,0),type=1,na.rm = TRUE)
  rVal <- quantile(varvect,c(1,0.90,0.50,0),type=1,na.rm = TRUE)

  colours <- c(col,col2)

  # CIRCLES
  if(symbols == "circles"){
    text(x=l$x,y=l$y,title,adj=c(0,1),cex=0.6)
    xpos <- (l$x+rLeg[1]-rLeg/2)
    ypos <- l$y+rLeg-rLeg[1]*2
    ypos <- ypos-strheight(title,cex = 0.6)-delta
    symbols(x = rep(l$x+rLeg[1],4),y=ypos,circles=rLeg,add=T,bg=col,inches=FALSE)
    text(x=rep(l$x+rLeg[1],4)+rLeg[1]*1.2,y=(l$y+(2*rLeg)-rLeg[1]*2-delta-
                                               strheight(title,cex = 0.6)),
         rVal,cex=0.5,srt=0,adj=0)
    for (i in 1:4){  segments (l$x+rLeg[1],
                               (l$y+(2*rLeg[i])-rLeg[1]*2-delta-
                                  strheight(title,cex = 0.6)),
                               l$x+rLeg[1]+rLeg[1]*1.1,
                               (l$y+(2*rLeg[i])-rLeg[1]*2-delta-
                                  strheight(title,cex = 0.6)))
    }

    if (nbCols == 2){
      tmp <- c ((x2-x1), (y2-y1))
      size <- max(tmp)/50
      symbols(x=rep(l$x+rLeg[1],4),y=ypos,circles=rLeg,add=T,bg="#CCCCCC",inches=FALSE)
      for (i in 1:4){  segments (l$x+rLeg[1],(l$y+(2*rLeg[i])-rLeg[1]*2-delta-strheight(title,cex = 0.6)),l$x+rLeg[1]+rLeg[1]*1.1,(l$y+(2*rLeg[i])-rLeg[1]*2-delta-strheight(title,cex = 0.6)))}
      rect(xpos[1]-rLeg[1]/2, ypos[1]-rLeg[1]-delta, xpos[1]-rLeg[1]/2+delta,  ypos[1]-rLeg[1]-delta*2,col=col)
      text(x=xpos[1]-rLeg[1]/2+delta*1.2,y=ypos[1]-rLeg[1]-delta-delta/4,paste("< ",breakval),adj=c(0,1),cex=0.5)
      rect(xpos[1]-rLeg[1]/2, ypos[1]-rLeg[1]-delta*2, xpos[1]-rLeg[1]/2+delta,  ypos[1]-rLeg[1]-delta*3,col=col2)
      text(x=xpos[1]-rLeg[1]/2+delta*1.2,y=ypos[1]-rLeg[1]-delta*2-delta/4,paste("> ",breakval),adj=c(0,1),cex=0.5)
    }
  }

  # SQUARES
  if(symbols == "squares"){
    text(x=l$x,y=l$y,title,adj=c(0,1),cex=0.6)
    xpos<- (l$x+rLeg[1]-rLeg/2)
    ypos <-l$y+rLeg/2-rLeg[1]
    ypos<-ypos-strheight(title,cex = 0.6)-delta
    symbols(x=xpos,y=ypos,squares=rLeg,add=TRUE,bg=col,inches=FALSE)
    text(x=l$x+rLeg[1]*1.2,y=ypos+rLeg/2,rVal,cex=0.3,srt=0,adj=0)
    for (i in 1:4){  segments (l$x+rLeg[1],ypos+rLeg/2,l$x+rLeg[1]*1.1,ypos+rLeg/2)}


    if (nbCols == 2){
      symbols(x=xpos,y=ypos,squares=rLeg,add=TRUE,bg="#CCCCCC",inches=FALSE)
      rect(xpos[1]-rLeg[1]/2, ypos[1]-rLeg[1]/2-delta, xpos[1]-rLeg[1]/2+delta, ypos[1]-rLeg[1]/2-delta*2,col=col)
      text(x=xpos[1]-rLeg[1]/2+delta*1.2,y=ypos[1]-rLeg[1]/2-delta-delta/4,paste("< ",breakval),adj=c(0,1),cex=0.5)
      rect(xpos[1]-rLeg[1]/2, ypos[1]-rLeg[1]/2-delta*2, xpos[1]-rLeg[1]/2+delta,  ypos[1]-rLeg[1]/2-delta*3,col=col2)
      text(x=xpos[1]-rLeg[1]/2+delta*1.2,y=ypos[1]-rLeg[1]/2-delta*2-delta/4,paste("> ",breakval),adj=c(0,1),cex=0.5)
    }
  }

  # BARRES
  if(symbols == "height"){
    text(x=l$x,y=l$y,title,adj=c(0,1),cex=0.6)
    #tmp <- c ((bbox(object@geom)[3]-bbox(object@geom)[1]), (bbox(object@geom)[4]-bbox(object@geom)[2]))
    width<-delta
    tmp<-as.matrix(data.frame(width,rLeg))
    symbols(x=rep(l$x+width/2,4),y=l$y+rLeg/2-rLeg[1]-strheight(title,cex = 0.6)-
              delta,rectangles=tmp,add=TRUE,bg=col,inches=FALSE)
    for (i in 1:4){  segments (l$x+width,l$y+rLeg[i]-rLeg[1]-strheight(title,cex = 0.6)-
                                 delta,l$x+width*2,l$y+rLeg[i]-rLeg[1]-strheight(title,cex = 0.6)-delta)}
    text(x=l$x+2*width*1.2,y=l$y+rLeg-rLeg[1]-strheight(title,cex = 0.6)-delta,rVal,cex=0.3,srt=0,adj=0)

    if (nbCols==2){

      symbols(x=rep(l$x+width/2,4),y=l$y+rLeg/2-rLeg[1]-strheight(title,cex = 0.6)-delta,rectangles=tmp,add=TRUE,bg="#CCCCCC",inches=FALSE)
      rect(l$x, l$y-rLeg[1]-delta*2-strheight(title,cex = 0.6), l$x+delta, l$y-delta*3-rLeg[1]-strheight(title,cex = 0.6),col=col)
      text(x=l$x+delta*1.2,y=l$y-rLeg[1]-delta*2-delta/4-strheight(title,cex = 0.6),paste("< ",breakval),adj=c(0,1),cex=0.5)
      rect(l$x, l$y-rLeg[1]-delta*3-strheight(title,cex = 0.6), l$x+delta, l$y-delta*4-rLeg[1]-strheight(title,cex = 0.6),col=col2)
      text(x=l$x+delta*1.2,y=l$y-rLeg[1]-delta*3-delta/4-strheight(title,cex = 0.6),paste("> ",breakval),adj=c(0,1),cex=0.5)
    }
  }

}





