# ----------------------------------------------------
# R, UN LANGAGE POUR LES STATS
# ----------------------------------------------------

setwd("/home/nlambert/Documents/R/6novembre")
donnees<-read.csv( "data/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",encoding="latin1",)
head(donnees)

mean(donnees$POPTO2010)
sd(donnees$POPTO2010)
summary(donnees$POPTO2010)
plot.new()
boxplot(donnees$POPTO2010,main="Population totale en 2010", horizontal=TRUE)

# ----------------------------------------------------
# MAIS DANS R, ON PEUT AUSSI GERRE LES OBJETS SPATIAUX
# ----------------------------------------------------

library(maptools)
library(rgeos)

# Ouverture de deux couches

fdcOri<-"geom/Tunisie_snuts4.shp"
fdcOri2<-"geom/sebkhas.shp"
delegations<-readShapeSpatial(fdcOri)
sebkhas<-readShapeSpatial(fdcOri2)
plot(delegations, col="#CCCCCC")
plot(sebkhas, add=T,col="white")

# Tester le shp
getinfo.shape(fdcOri)
class(delegations)
head(delegations@data)
head(gIsValid(delegations, byid = TRUE, reason=TRUE))


# ---------------------------------------
# MAIS DANS R, ON PEUT AUSSI FAIRE DU SIG
# ---------------------------------------

#extraction d'un polygone
poly1<-delegations[delegations@data$id=="TS3234",]
plot(poly1,col="black", add=T)

#Extraction des contours
b = gBoundary(poly1)
plot(b, col="red",lwd=3,add=T)

#buffer
buff<-gBuffer(poly1, byid=TRUE, id=NULL, width=20000, quadsegs=5, capStyle="ROUND",joinStyle="ROUND", mitreLimit=1.0)
plot(buff,add=TRUE,col="yellow")
plot(poly1,col="black", add=T)

#centroide
centres<-gCentroid(poly1, byid=TRUE, id = NULL)
plot(centres,col="red",add=T,lwd=4)
head(centres@coords)

#Aggregation des géométries
head(delegations@data)
buff<-gBuffer(delegations, byid=TRUE, id=NULL, width=1, quadsegs=5, capStyle="ROUND",joinStyle="ROUND", mitreLimit=1.0)
gouvernorats<-gUnaryUnion(buff,id = delegations@data$id_snuts3)
regions<-gUnaryUnion(buff, id = delegations@data$id_snuts2)
macro<-gUnaryUnion(buff, id = delegations@data$id_snuts1)
country<-gUnaryUnion(buff, id = delegations@data$id_snuts0)
par(mfrow=c(1,5))
plot(delegations)
title(main="Délégations")
plot(gouvernorats)
title(main="Gouvernorats")
plot(regions)
title(main="Regions")
plot(macro)
title(main="Zones")
plot(country)
title(main="Pays")
par(mfrow=c(1,1))


# ---------------------------------------
# EXEMPLE D'UTILISATION RAPIDE
# Existe t il une relation entre l'indice de développement régional
# Et la distance à la côte en Tunise
# ---------------------------------------

# ----------------------------------------------------------
# Etape 1 : Ouverture du tableau de données

donnees<-read.csv( "data/tunisie_data_del_2011.csv",header=TRUE,sep=";",dec=",",encoding="latin1",)
head(donnees)
donnees<-donnees[,c("del","del_nom","IDRVA2011")]
head(donnees)

# ----------------------------------------------------------
# etape 2 : créer une nouvelle variable : distance à la côte

centres<-readShapeSpatial("geom/Tunisie_snuts4_centres.shp")
coast<-readShapeSpatial("geom/coast.shp")
plot(coast,col="red", lwd=1.5)
plot(centres,add=TRUE)

# calcul distance points -> ligne 
dist<-gDistance(coast,centres,byid=TRUE)
dist<-data.frame(centres@data$id,dist)
colnames(dist)<-c("id","dist")
head(dist)

# Jointure entre les deux tableaux et selections des colonnes
mydata = data.frame(dist, donnees[match(dist[,"id"], donnees[,"del"]),])
mydata<-mydata[,c("id","del_nom","IDRVA2011","dist")]
colnames(mydata)<-c("id","nom","idr","dist")
head(mydata)

# ----------------------------------------------------------
# etape 3 : Analyse statistique

# Variable quantitative à expliquer (Y) : idr
# Variable quantitative explicative (X) : dist (en log)
mydata$logDist<-log(mydata$dist)

# résumé et visualisation stat des variables
summary(mydata$idr)
summary(mydata$logDist)

plot.new()
par(mfrow=c(2,2))
hist(mydata$idr,main="idr",breaks=10)
hist(mydata$logDist,main="dist à la côte (log)",breaks=10)
boxplot(mydata$idr,main="idr", horizontal=TRUE)
boxplot(mydata$logDist,main="dist à la côte", horizontal=TRUE)

# Etude de la relation entre X et Y
X <- mydata$logDist
Y <- mydata$idr
par(mfrow=c(1,1))
plot(X,Y, main="Relation entre X et Y", xlab="distance à la côte (log)",  ylab="Indice de développement régional", type="p",  pch=20,   cex=0.7)   

cor(X,Y)
cor.test(X,Y) # une relatio est signification si p-value < 0.05 (5% d'erreur)
MonModele <- lm(Y~X)
summary(MonModele)
names(MonModele)
abline(MonModele,  col="red")

# Calcul des résuidus
mydata$Yres<-MonModele$residuals
mydata$Yres_std<-mydata$Yres/(sd(mydata$Yres))
head(mydata)

# ----------------------------------------------------------
# etape 4 : Cartographie
library(RColorBrewer)
library(classInt)

fdc <- readShapeSpatial("geom/Tunisie_snuts4.shp")
codecarto<-names(fdc@data)[1]
fdc@data = data.frame(fdc@data, mydata[match(fdc@data[,codecarto], mydata[,"id"]),])
#distr <- classIntervals(fdc@data$Yres_std,5,style="quantile")$brks
distr<-c(-1000,-2,-1,-0.5,0,0.5,1,2,1000)
colours <- brewer.pal(8,"RdBu")
#?brewer.pal
colMap <- colours[(findInterval(fdc@data$Yres_std,distr,all.inside=TRUE))]
plot(fdc, col=colMap,border="#000000",lwd=0.2)
legend(x="bottomleft", legend=leglabs(round(distr,2),over="sup. ",under="inf. "), fill=colours, bty="n",pt.cex=1,cex=0.7,title="residus standardisés")
title(main="Residus standardisés",sub="Auteur: Nicolas Lambert, CNRS, 2014",cex.sub=0.7)

