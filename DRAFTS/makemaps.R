library(maptools)
setwd("/home/nlambert/Documents/R/freeCarto")
rm(list=ls())


# ----------------------------------------------------------------------------
# NewMap  Create a new graphic window => fix the size of the final ouput
# ----------------------------------------------------------------------------

NewMap<-function(shp,title,sources){
  #par(omi=c(0,0,0,0), mgp=c(0,0,0),mar=c(0,0,0,0) , family = "D")
  #par(mfrow=c(1,1),cex=1,cex.lab = 0.75,cex.main=0.2,cex.axis=0.2)
  #plot.new()
  layer <- readShapeSpatial(shp)
  x1 <- bbox(layer)[1]
  y1 <- bbox(layer)[2]
  x2 <- bbox(layer)[3]
  y2 <- bbox(layer)[4]
  plot(layer, col="#FFFFFF00",border="#FFFFFF00",xlim=c(x1,x2), ylim=c(y1,y2))
  title(main=title,sub=sources,cex.sub=0.7)
}


# ----------------------------------------------------------------------------
# AddLayer - Adding static layers
# ----------------------------------------------------------------------------

AddLayer<-function(shp,colorstroke="black",color="blue",thickness=1){
layer <- readShapeSpatial(shp)
if(getinfo.shape(shp)[2]=="5"){plot(layer, border=colorstroke, col=color, lwd=thickness,add=T)}else{} #poly
if(getinfo.shape(shp)[2]=="3"){plot(layer, col=color, lwd=thickness,add=T)}else{} #line
if(getinfo.shape(shp)[2]=="1"){points(layer,cex = 0.1, pch=16, add=T)}else{} #dot
}


# ----------------------------------------------------------------------------
# AddLabels - Adding labels from a shapfile
# ----------------------------------------------------------------------------

AddLabels<-function(shp,field,color,size){
  layer <- readShapeSpatial(shp)
  # if poly
  if(getinfo.shape(shp)[2]=="5"){
    pt <- cbind(layer@data[,field],as.data.frame(coordinates(layer)))
    colnames(pt) <- c("field","x","y")
    text(pt$x, pt$y , labels = pt$field,cex=size,col=color)
    }else{} 
  #if line
  if(getinfo.shape(shp)[2]=="3"){}else{} 
  # if dot
  if(getinfo.shape(shp)[2]=="1"){}else{} 
}


# ----------------------------------------------------------------------------
# AddPropSymbol - Adding proportionnal symbols
# ----------------------------------------------------------------------------


AddPropSymbols<-function(shp,csv,type="circles",mycol="red"){
  layer <- readShapeSpatial(shp)
  # if poly
  if(getinfo.shape(shp)[2]=="5"){
    
    
    #csv<-"data/tunisie_data_del_2011.csv"
    #layer <- readShapeSpatial("geom/Tunisie_snuts4.shp")
    
    csv<-read.csv( csv,header=TRUE,sep=";",dec=",",encoding="latin1",)
    pt <- cbind(layer@data[,"id"],as.data.frame(coordinates(layer)))
    colnames(pt) <- c("id","x","y")
    head(pt)
    head(csv)
    pt = data.frame(pt, csv[match(pt[,"id"], csv[,"del"]),])
    pt$var<-pt$SUP2010
    k<-100000 # coef
    pt$circleSize <- sqrt((pt$var*k)/pi) # surface des cercles
    pt$squareSize <-  sqrt(pt$var*k) # surface des carrés
    k2<-10 # coef
    pt$heightSize <- pt$var*k2 # Hauteur des barres
    pt <- pt[order(pt$circleSize,decreasing=TRUE),]
    
    if (type=="squares"){symbols(pt[,c("x","y")],squares=pt$squareSize,add=TRUE,bg=mycol,inches=FALSE)}
    if (type=="circles"){symbols(pt[,c("x","y")],circles=pt$circleSize,add=TRUE,bg=mycol,inches=FALSE)}
    if (type=="height"){
    tmp<-as.matrix(data.frame(10000,pt$heightSize))
    pt$y2<-pt$y+pt$heightSize/2
    symbols(pt[,c("x","y2")],rectangles=tmp,add=TRUE,bg=mycol,inches=FALSE)
    }
        
  }else{} 
  #if line
  if(getinfo.shape(shp)[2]=="3"){}else{} 
  # if dot
  if(getinfo.shape(shp)[2]=="1"){}else{} 
}



# ***********************************************************************************************
# HOW TO CREATE A MAP
# ***********************************************************************************************

par(mfrow=c(1,3))

NewMap("geom/Tunisie_snuts0.shp","Squares","FreeCarto project")
AddLayer("geom/Tunisie_snuts4.shp",colorstroke="#E3E8F7",color="#000099")
AddPropSymbols("geom/Tunisie_snuts4.shp","data/tunisie_data_del_2011.csv",type="squares","yellow")
#AddLabels("geom/Tunisie_snuts2.shp","id","red",0.6)

NewMap("geom/Tunisie_snuts0.shp","Circles","FreeCarto project")
AddLayer("geom/Tunisie_snuts4.shp",colorstroke="#E3E8F7",color="#000099")
AddPropSymbols("geom/Tunisie_snuts4.shp","data/tunisie_data_del_2011.csv",type="circles","red")

NewMap("geom/Tunisie_snuts0.shp","Height","FreeCarto project")
AddLayer("geom/Tunisie_snuts4.shp",colorstroke="#E3E8F7",color="#000099")
AddPropSymbols("geom/Tunisie_snuts4.shp","data/tunisie_data_del_2011.csv",type="height","green")

par(mfrow=c(1,1))
