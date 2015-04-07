#' @title choroLayer
#' @description plot a chorpoleth layer
#' @name choroLayer
#' @details plot a chorpoleth layer
#' @param spdf spatial dataframe (geometries)
#' @param df dataframe (data)
#' @param spdfid id of the spdf (if null, the first column is used)
#' @param dfid id of the df (if null, the first column is used)
#' @param var field used
#' @param distr vector of classes
#' @param col vector of colors
#' @param nbclass number of classes targeted (if null,
#' the Huntsberger method is used)
#' @param method discretization method ("sd", "equal",
#' "quantile", "jenks","q6","geom")
#' @param pal1 color palette 1
#' @param pal2 color palette 2
#' @param breakval Diverging value
#' @param alpha alphaeffect enhancing contrasts with opacity
#' @param add add=T
#'
#' @examples
#' choroLayer(spdf = TNdeleg.spdf, df = TNdeleg, var =  "housing", nbclass = 20,
#' pal1="red.pal",pal2="green.pal",breakval=10000,method = "equal",add=FALSE,
#' alpha=TRUE)
#'
#' @return plot
#' @export

choroLayer <- function(spdf, df, spdfid = NULL, dfid = NULL, var, distr=NULL,
                       col = NULL, nbclass=NULL, method="quantile",
                       pal1 = "blue.pal", pal2 = "green.pal", breakval = NULL,
                       alpha = FALSE,add=T)
{

  layer <- choro(spdf=spdf, df=df, spdfid = spdfid, dfid = dfid, var=var, distr=distr, col = col, nbclass=nbclass, method=method, pal1 = pal1, pal2 = pal2, breakval = breakval, alpha = alpha)

  # poly
  plot(layer$spdf, col=as.vector(layer$spdf@data$colMap),border="black",lwd=1,add=add)

  # lines (todo)

  # dots (todo)


}


