library(maptools)
setwd("/home/nlambert/Documents/R/FreeCarto/DATA/TUN/")
rm(list=ls())

# encore un test
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
  title(main=title,cex.main=2,sub=sources,cex.sub=1.2)
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

# SPECIFS #
# la jointure se fait sur la 1ere col du fond de carte et du csv

# TODO #
# génériciser la taille des symboles par défaut : k
# gestion de la legende : dans la fct ? En dehors ?
# gerer points et lignes
# gerer donnes manquantes


AddPropSymbols<-function(shp,csv,type="circles",mycol="red"){
  layer <- readShapeSpatial(shp)
  csv<-read.csv( csv,header=TRUE,sep=";",dec=",",encoding="latin1",)
  # if poly
  if(getinfo.shape(shp)[2]=="5"){  
    pt <- cbind(layer@data[,1],as.data.frame(coordinates(layer)))
    colnames(pt) <- c("id","x","y")
    head(pt)
    head(csv)
    pt = data.frame(pt, csv[match(pt[,1], csv[,1]),])
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

NewMap("Tunisie_snuts4.shp","Squares","FreeCarto project")
AddLayer("Tunisie_snuts4.shp",colorstroke="#E3E8F7",color="#000099")
AddPropSymbols("Tunisie_snuts4.shp","tunisie_data_del_2011.csv",type="squares","yellow")
#AddLabels("Tunisie_snuts4.shp","id","red",0.6)

NewMap("Tunisie_snuts4.shp","Circles","FreeCarto project")
AddLayer("Tunisie_snuts4.shp",colorstroke="#E3E8F7",color="#000099")
AddPropSymbols("Tunisie_snuts4.shp","tunisie_data_del_2011.csv",type="circles","red")

NewMap("Tunisie_snuts4.shp","Height","FreeCarto project")
AddLayer("Tunisie_snuts4.shp",colorstroke="#E3E8F7",color="#000099")
AddPropSymbols("Tunisie_snuts4.shp","tunisie_data_del_2011.csv",type="height","green")

par(mfrow=c(1,1))