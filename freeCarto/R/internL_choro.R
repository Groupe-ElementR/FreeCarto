# load("/home/nlambert/Documents/R/github/FreeCarto/freeCarto/data/colors.RData")
# load("/home/nlambert/Documents/R/github/FreeCarto/freeCarto/data/TNdeleg.rda")
# source("/home/nlambert/Documents/R/github/FreeCarto/freeCarto/R/internL_discretization.R")
# source("/home/nlambert/Documents/R/github/FreeCarto/freeCarto/R/internL_palette.R")

#' choro
#'
#' add color gradients to spdf according to data classes
#' @details xxx
#'
#' @param spdf spatial dataframe (geometries)
#' @param df dataframe (data)
#' @param spdfid id of the spdf (if null, the first column is used)
#' @param dfid id of the df (if null, the first column is used)
#' @param var field used
#' @param distr vector of classes
#' @param col vector of colors
#' @param nbclass number of classes targeted (if null, the Huntsberger method is used)
#' @param method discretization method ("sd", "equal", "quantile", "jenks","q6","geom")
#' @param pal1 color palette 1
#' @param pal2 color palette 2
#' @param breakval Diverging value
#' @param alpha alphaeffect enhancing contrasts with opacity
#'
#' @examples
#' # xxx
#'
#' @return list


choro <- function(spdf, df, spdfid = NULL, dfid = NULL, var, distr=NULL, col = NULL, nbclass=NULL, method="quantile", pal1 = "blue.pal", pal2 = "green.pal", breakval = 0, alpha = FALSE)
  {

  # If ids undefined, the first column is chosen
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(dfid)){dfid<-names(df)[1]}


  # Join
  spdf@data <- data.frame(spdf@data, df[match(spdf@data[,spdfid], df[,dfid]),])

  # Discretization
  if (is.null(distr)){
    field <- spdf@data[,var]
    distr <- discretization(v=field,nbclass=nbclass,method=method)
  }


  # Colors
  if(is.null(col)){

    # Si pas de breakval, on breakval = le min de la distribution
    if (is.null(breakval)) {breakval <- distr[1]}

    # one color

    if (breakval < distr[2] | breakval > distr[length(distr)-1]) { col <- palette(pal1,nbclass,alphaeffect=alpha) }

    # two colors
    else
    {
      bornes <- distr[2:(length(distr)-1)]
      if (breakval %in% bornes) { middle <- FALSE } else { middle <- TRUE }
      num1 <- 0
      num2 <- 0
      for(i in 1:length(bornes)){if(bornes[i]<=breakval){num1 <- num1 + 1}}
      for(i in 1:length(bornes)){if(bornes[i]>=breakval){num2 <- num2 + 1}}
      col <- palette(pal1,num1,pal2,num2,middle=middle,alphaeffect=alpha)
    }
  }

  # Affectation des couleurs au spdf
  colMap <- col[(findInterval(spdf@data[,var],distr,all.inside=TRUE))]
  spdf@data <- data.frame(spdf@data,colMap)


return(list(spdf=spdf,distr=distr,col=col))
}



# mycols <- c("#A2D6EC","#F6F6F6FF","#BBDAAD","#97C38B","#74AC69","#468E3D","#247524","#16642A")
#test <-choro(spdf = TNdeleg.spdf, df = TNdeleg, spdfid = NULL, dfid = NULL, col = mycols, breakval = 15000 ,pal1="red.pal",var =  "housing", nbclass = 8, method = "quantile",alpha=TRUE)
# test$distr
# head(test$spdf@data)
# test$col
# display.gradient(test$col)

